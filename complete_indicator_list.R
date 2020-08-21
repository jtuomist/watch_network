
# Data from https://hiilineutraalisuomi.fi/fi-FI/Paastot_ja_indikaattorit

df <- read.csv("KAIKKI KUNNAT_ALas 1.1_web.csv", dec=",", stringsAsFactors = TRUE)[-(15:17)] # remove empty columns

tie <- rbind(
  read.csv("TIELIIKENNE_ALas 1.1_web_hinku.csv", dec=",", stringsAsFactors = TRUE),
  read.csv("TIELIIKENNE_ALas 1.1_web_käyttöperustainen.csv", dec=",", stringsAsFactors = TRUE),
  read.csv("TIELIIKENNE_ALas 1.1_web_lipasto.csv", dec=",", stringsAsFactors = TRUE),
  read.csv("TIELIIKENNE_ALas 1.1_web_oma_ja_läpiajo.csv", dec=",", stringsAsFactors = TRUE)
)

# Units used:
# päästöt: ktCO2e
# tuuli: ktCO2e
# energiankulutus: GWh
# suorite: Mkm

colnames(df)[12:14] <- c("päästöt","tuuli","energiankulutus")
colnames(tie)[7:9] <- c("päästöt","energiankulutus","suorite")

# Korjataan epäjohdonmukaisuudet
levels(tie$taso_4)[levels(tie$taso_4)=="Moottoripyörät"] <- "Moottoripyörät ja mopot" 
levels(df$taso_2)[levels(df$taso_2)=="Työkoneet"] <- "Teollisuus ja työkoneet"

unique(tie[5:6]) # Tarkimmat tasot liikenteessä
unique(df[8:9]) # Tarkimmat tasot kuntien päästöissä

# tie ja df sisältävät samat tasot 4 ja 5 paitsi liikenteessä taso 5 Moottoripyörät ja mopot jakautuu vielä pienempiin ryhmiin.
# Lisätään nämä yhteiseen jaotteluun.

luokat <- unique(df[5:9])
luokat <- rbind(
  luokat[luokat$taso_4!="Moottoripyörät ja mopot",],
  merge(luokat[colnames(luokat)!="taso_5"], unique(tie[tie$taso_4=="Moottoripyörät ja mopot",5:6]))
)

luokat <- luokat[order(luokat$taso_1, luokat$taso_2, luokat$taso_3, luokat$taso_4),]
luokat$id <- 1:nrow(luokat)

write.csv(luokat, "alas_luokat.csv", row.names = FALSE)

#############################################################################3
# Produce the standard insight network
library(OpasnetUtils)

objects.latest("Op_en3861", code_name="makeGraph")

df <- read.csv("watch insight network.csv")
df <- df[grepl("graph",df$id),] # Only take the standard network nodes
df$label <- df$source

#> colnames(df)
#[1] "source"      "target"      "interaction" "id"          "gpc"         "basic"       "hinku"       "alas"       
#[9] "secap"       "hiv"         "scope"       "indices"     "description" "unit"        "label"

colnames(df)[c(4,5,1,14,3,2,13,12)] <- c("Oldid", "type", "Item", "id", "rel", "Object", "Description", "Context")

gr <- makeGraph(df)

render_graph(gr)

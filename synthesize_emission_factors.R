# This is code on watch_network/synthesize_emission_factors.R
# It collects data from SYKE emission repository and synthesizes emission factors from there.

library(plotly)
library(ggplot2)

# Data from https://hiilineutraalisuomi.fi/fi-FI/Paastot_ja_indikaattorit

df <- read.csv("KAIKKI KUNNAT_ALas 1.1_web.csv", dec=",", stringsAsFactors = TRUE)[-(15:17)] # remove empty columns

tie <- rbind(
  cbind(read.csv("TIELIIKENNE_ALas 1.1_web_hinku.csv", dec=",", stringsAsFactors = TRUE),hinku=TRUE),
  cbind(read.csv("TIELIIKENNE_ALas 1.1_web_käyttöperustainen.csv", dec=",", stringsAsFactors = TRUE),hinku=FALSE),
  cbind(read.csv("TIELIIKENNE_ALas 1.1_web_lipasto.csv", dec=",", stringsAsFactors = TRUE),hinku=FALSE),
  cbind(read.csv("TIELIIKENNE_ALas 1.1_web_oma_ja_läpiajo.csv", dec=",", stringsAsFactors = TRUE),hinku=FALSE)
)

# Units used:
# emission: ktCO2e
# wind: ktCO2e
# energy_use: GWh
# mileage: Mkm

#colnames(df)
#[1] "kuntanumero"    "kunta"          "maakunta"       "vuosi"          "taso_1"         "taso_2"        
#[7] "taso_3"         "taso_4"         "taso_5"         "hinku.laskenta" "päästökauppa"   "emission"      
#[13] "wind"           "energy_use"    

colnames(df)[c(2,4,8,9,12:14)] <- c("municipality","year","level4","level5","emission","wind","energy_use")

#colnames(tie)
#[1] "kuntanumero"     "kunta"           "maakunta"        "vuosi"           "taso_4"         
#[6] "taso_5"          "päästöt"         "energiankulutus" "suorite"         "laskentaperuste"
#[11] "hinku" 

colnames(tie)[c(2,4:10)] <- c("municipality","year","level4", "level5","emission","energy_use",
                              "mileage","model")

# Korjataan epäjohdonmukaisuudet
levels(tie$level4)[levels(tie$level4)=="Moottoripyörät"] <- "Moottoripyörät ja mopot" 
levels(df$taso_2)[levels(df$taso_2)=="Työkoneet"] <- "Teollisuus ja työkoneet"

tie$emission_factor <- tie$emission / tie$energy_use # kton/GWh = kg/kWh
tie$energy_factor <- tie$energy_use / tie$mileage # GWh/Mkm = kWh/km
tie$fuel_efficiency <- tie$energy_factor / 10 * 100 # 10.0 kWh/l; kWh/km --> l/100 km

################# Screening for different emission factors

df <- df[!is.na(df$energy_use) & df$energy_use>0 & df$taso_2 != "Liikenne", ]
df$level5 <- droplevels(df$level5)

df$emission_factor <- df$emission / df$energy_use

tmp <- df[df$year==2015 & df$municipality=="Helsinki", ]

plot_ly(tmp, x=~emission_factor, y=~level5, color=~level4, type="scatter", mode="markers")

############# Search for determinants of emission factors in non-traffic emissions

###### Oil heating

tmp <- df[df$level4=="Öljylämmitys",]
tmp <- aggregate(tmp[c("emission","energy_use")], by = tmp[c("year")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~year, y=~emission_factor, color=~year,
        type="scatter", mode="lines")

# Oil heating emission factor varies between level5 and municipality only by the rounding error.
# But there seems to be a meaningful time trend. So, we assume that oil heating has the same
# emission factor for all level5 and municipality but specific for year.

out <- data.frame(tmp, sector = "Öljylämmitys")

####### Consumer electricity

tmp <- df[df$level4 %in% c("Kulutussähkö") | df$level5 %in% c("Lähijunat", "Metrot ja raitiovaunut","Sähkö"),]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("year")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~year, y=~emission_factor, color=~year, type="scatter", mode="lines")

out <- rbind(out, cbind(tmp, sector="Kulutussähkö"))

# Also electricity has a time trend but differences between communities and type_5 are just noise.
# Therefore make a time trend of emission factors.
# This applies to electric transport modes as well.

####### Heating electricity

tmp <- df[df$level4 %in% c("Sähkölämmitys","Maalämpö"),]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("year")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~year, y=~emission_factor, color=~year, type="scatter", mode="lines")

## There is only noise between municipalities and level_5, therefore make a time trend only.
# Geothermal energy does not differ from heating electricity and therefore it is merged here.

out <- rbind(out, cbind(tmp, sector="Sähkölämmitys"))

######### Wood heating

tmp <- df[df$level4=="Puulämmitys",]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("level5")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~level5, y=~emission_factor, color=~level5, type="scatter", mode="markers")

# There is a clear difference between level_5 but only noise between municipalities.
# There is no time trend, but residential and commercial buildings have a higher emission factor
# while agricultural and industry buildings have a lower emission factor.

out <- rbind(
  cbind(out, level5=NA),
  cbind(tmp, sector="Kulutussähkö", year=NA))

##################### District heating

tmp <- df[df$level4=="Kaukolämpö",]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("municipality","year","päästökauppa")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~year, y=~emission_factor, color=~municipality, type="scatter", mode="lines")

## There is only noise between level_5 but clear differences between municipalities and over time.
#aggregate(tmp$päästökauppa, by=tmp["municipality"], FUN=function(x) mean(x=="On"))
# This result shows that some municipalities have different kinds of district heating and
# therefore they may have two different emission factors.
# Thus, emission trade makes a difference in emission factor and it must be included.

out <- rbind(
  cbind(out, municipality=NA, päästökauppa=NA),
  cbind(tmp, sector="Kaukolämpö",level5=NA))

##################### Other heating

tmp <- df[df$level4=="Muu lämmitys",]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("year","level5")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

aggregate(df["energy_use"], by = df["level4"], sum)
sum(df$energy_use)

plot_ly(tmp, x=~year, y=~emission_factor, color=~level5, type="scatter", mode="lines")

## There are differences between municipalities and level_5 as well as over time.
## However, the differences between municipalities is maybe not crucial, as the energy use of this
## sector is fairly small, as it is only a few percent of the total.
## Therefore, only level_5 and year will be included.

out <- rbind(out, cbind(tmp, sector="Muu lämmitys",päästökauppa=NA,municipality=NA))

##################### Industry and machinery

tmp <- df[df$taso_2=="Teollisuus ja työkoneet",]
tmp <- aggregate(tmp[c("emission", "energy_use")], by = tmp[c("year","level5","päästökauppa")], FUN=sum)
tmp$emission_factor <- tmp$emission / tmp$energy_use

plot_ly(tmp, x=~year, y=~emission_factor, color=~paste(level5,päästökauppa), type="scatter", mode="lines")

## There are differences over time for industrial fuels. 
## For industrial and agriculatural machinery, there is only noise over time and across level_5.
## in contrast, the differences between municipalities seem to be noise.
## However, emission trade classification (yes/no) is used with Industrial fuels as can be shown:
## #aggregate(tmp$päästökauppa, by=tmp["municipality"], FUN=function(x) mean(x=="On"))
## Therefore, time and emission trade are included for industrial fuel, but all other level_5 will be constant.

out <- rbind(out, cbind(tmp[tmp$level5=="Teollisuuden polttoaineet",],
                        sector="Teollisuus ja työkoneet", municipality = NA))

tmp <- df[df$taso_2=="Teollisuus ja työkoneet" & df$level5!="Teollisuuden polttoaineet",]
tmp <- aggregate(tmp[c("emission","energy_use")], by=tmp["level5"],sum)
tmp$emission_factor <- sum(tmp$emission) / sum(tmp$energy_use)

out <- rbind(out, cbind(year=NA,municipality=NA, päästökauppa=NA,sector="Teollisuus ja työkoneet",tmp))

############################# ANALYZE ROAD TRAFFIC EMISSION AND ENERGY FACTORS

###### Emission factor

road <- tie[tie$hinku==FALSE,]
levels(road$model)[levels(road$model) %in% c("Oma tieliikenne","Läpiajoliikenne")] <- "Transit"
road <- aggregate(road[c("emission","energy_use","mileage")], 
                  by = road[c("municipality","year","level4","level5","model")], FUN=sum)
road$emission_factor <- road$emission / road$energy_use

road$timevar <- road$model
#levels(road$timevar)
#[1] "Käyttöperusteinen" "Transit"           "Lipasto"           "SYKE"             

levels(road$timevar) <- c("use","transit","vtt","vtt")

road <- reshape(road, direction="wide",
                v.names=c("emission","energy_use","mileage","emission_factor"),
                idvar=c("municipality","year","level4","level5"),
                timevar="timevar",
                drop=c("kuntanumero", "maakunta","hinku","model"))

#table(is.na(road$emission.Lipasto), is.na(road$emission.SYKE))
#SYKE model is used to calculate results when Lipasto is not available. Those can be merged without loss of data.

plot_ly(road[road$municipality=="Helsinki" & road$year==2015,], x=~emission_factor.vtt, y=~emission_factor.use,
        color=~paste(level5, level4), type="scatter",mode="markers")

plot_ly(road[road$level5=="Henkilöautot" & road$level4=="Tiet",],
        x=~year, y=~emission_factor.transit, color=~municipality, type="scatter",mode="lines")

ggplot(tie[tie$year==2015,],
       aes(x=emission_factor, color=model, linetype=level4))+stat_ecdf()+
  facet_wrap(~level5)+
  coord_cartesian(xlim=c(0.15, 0.35))

# From the graph above we can make several conclusions:
# * Distributions are mostly very tight indicating that differences between municipalities are negligible.
# * Results for buses look crazy and are ignored until they are better undestood.
# * Results by SYKE model (supplementing values for Lipasto model) look crazy and are ignored.
# * Lipasto has essentially the same emission factors for road and street.
# * The difference between streets and roads exist but is inconsistent between models.
# * The default emission factor seems to be 0.23 kg/kWh (CO2e). Only these subgroupd deviate:
# ** Cars on roads by use and transit models (0.27 kg/kWh)
# ** Vans by use and transit models (0.24-0.27 kg/kWh)
# ** Trucks on roads by use and transit models and on streets by SYKE model (0.20 kg/kWh)
# ** Trucks on streets by use and transit models (0.31 kg/kWh sic!)
# As an overall conclusion, it may be sufficient to integrate data by keeping year and level5 but
# integrating over model and level4

tmp <- aggregate(tie[c("emission","energy_use","mileage")], 
                 by = tie[c("year","level5")], FUN=function(x) sum(x, na.rm=TRUE))
tmp$emission_factor <- tmp$emission / tmp$energy_use
tmp$energy_factor <- tmp$energy_use / tmp$mileage

#tmp$emf_sd <- aggregate(tie$emission_factor, by=tie[c("year","level5")], FUN=function(x) sd(x, na.rm=TRUE))[["x"]]
# Standard deviarion is 0.01 - 0.06 i.e. small except for mopeds and buses.

ggplot(tmp, aes(x=year, y=emission_factor, color=level5))+geom_line()

# It is strange that mopo and mopoauto values change abruptly between 2017 and 2018. Why?
# Another strange result is the clearly lower values in 2014 and 2015 for trucks, lorries, and buses.

out <- rbind(out,
             cbind(tmp[!colnames(tmp) %in% c("mileage","energy_factor")],
                   sector="Tieliikenne",municipality=NA,päästökauppa=NA))

write.csv(out, "~/devel/ghg-notebooks/emission_factors_of_energy_consumption.csv", row.names=FALSE)

##### Energy_factor

ggplot(tie[tie$year==2015,],
       aes(x=fuel_efficiency, color=model, linetype=level4))+stat_ecdf()+
  facet_wrap(~level5)+
  scale_x_log10()

plot_ly(aggregate(tie$fuel_efficiency, by=tie[c("year","level5")], FUN=function(x) mean(x, na.rm=TRUE)),
        x=~year, y=~x, color=~level5, type="scatter",mode="lines&markers")

# Based on the graphs above, the following conclusions can be made:
# * SYKE estimates are all over the place and unbelievable. They will be ignored.
# * Other estimate are very close to each other and can be just averaged out.
# * Fuel efficiency is almost constant over time, which is not realistic.
# * Year and level5 are kept, other indices are aggregated.


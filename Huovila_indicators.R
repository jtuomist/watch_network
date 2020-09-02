# This code analyzes Huovila et al 2019 paper indicators. https://doi.org/10.1016/j.cities.2019.01.029
# Superheadings for columns:
# A-D: categories and indicators
# E-N: Sector # TWO points given to sectors
# O: Sum E-N # Note! There are errors in summary columns.
# P-T: Indicator type # ONE point given to type.
# U: Sum P-T 
# V-W: Urban focus # THREE points given (see below)
# V: Sustainability: Sum X-Z
# W: Smartness: Sum AA-AB
# X-Z: Sustainability # THREE points given to sustainability and smartness together
# AA-AB: Smartness
# AC: Sum X-AB 
# AD: Climate relevance. Note! Not an original column. This is added by me.
# Row 420: Total sum

library(reshape2)

df <- read.csv("Huovila_2019_standardized_indicators.csv")

df$Sub.category <- gsub("\n", " ", df$Sub.category)
df <- df[df$Indicator!="",]

for(y in c("Standard","Category","Sub.category")){
  z <- ""
  for(x in 1:nrow(df)) {
    z <- ifelse(df[[y]][x]!="",df[[y]][x],z)
    df[[y]][x] <- z
  }
}

#> colnames(df)
#[1] "Standard"                                 "Category"                                
#[3] "Sub.category"                             "Indicator"                               
#[5] "Natural.environment"                      "Built.environment"                       
#[7] "Water.and.waste"                          "Transport"                               
#[9] "Energy"                                   "Economy"                                 
#[11] "Education..culture..innovation...science" "Health..well.being...Safety"             
#[13] "Governance.and.citizen.engagement"        "ICT"                                     
#[15] "Sum_E.N"                                  "Input"                                   
#[17] "Process"                                  "Output"                                  
#[19] "Outcome"                                  "Impact"                                  
#[21] "Sum_P.T"                                  "Sustaina.bility"                         
#[23] "Smartness"                                "People"                                  
#[25] "Planet"                                   "Prosperity"                              
#[27] "Hard"                                     "Soft"                                    
#[29] "Sum_X.AB"                                 "Climate_relevant"                        
 
sector <- c("Environment","Built.environment","Water.and.waste","Transport","Energy","Economy",
            "Education","Wellbeing","Governance","ICT")
colnames(df)[5:14] <- sector
df$Category <- tolower(df$Category)
df$Sub.category <- tolower(df$Sub.category)

View(aggregate(df[df$Climate_relevant==1,sector], by=df[c("Category")], FUN=function(x) sum(x, na.rm=TRUE)))

out <- melt(df[df$Climate_relevant==1, c("Indicator",sector)],id.vars="Indicator", variable.name = "Sector")
out <- out[!is.na(out$value),c("Sector","Indicator")]
rownames(out) <- 1:nrow(out)

write.csv(out, "climate_relevant_indicators_from_7_standards.csv")

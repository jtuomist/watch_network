## This code is http://fi.opasnet.org/fi/Tautitaakka_Suomessa

library(OpasnetUtils)
#library(tidyverse)
library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)

objects.latest("Op_en6007", code_name = "hnh2035") # [[OpasnetUtils/Drafts]] pushIndicatorGraph
transl <- as_tibble(opbase.data("Op_fi3944", subset="Tautiluokittelu")) %>% # [[Tautitaakka Suomessa]]
  mutate(Id=as.integer(as.character(Id)))

BS <- 24

palet <- c(
  '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f',
  '#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', # First 12 colours from Colorbrewer Paired.
  'cyan2','cyan4','plum1','plum4', 'darkslategray4','darkslategray1','firebrick3'
)

###################### Graphs for the Tautitaakka auttaa hahmottamaan ... article

# DALYs by causes (risks not included)
dc <- as_tibble(opasnet.csv("2/2d/IHME_Fin_Risks_by_Cause.zip", wiki="opasnet_en",
                            unzip="IHME-GBD_2017_DATA-8ce9adcf-1.csv",sep=",",dec=".",header=TRUE)) %>%
  left_join(transl[transl$Type=="Cause",], by=c("cause_id"="Id")) %>%
  mutate(cause_name = Result) # transl combines some entries from cause_name to Name (in Finnish: Result)
#  "Neglected tropical diseases and malaria",
#  "HIV/AIDS and sexually transmitted infections",
#  "Enteric infections" >> "Other infectious diseases"

dc2 <- dc %>% 
  filter(dc$measure_name=="DALYs (Disability-Adjusted Life Years)" & dc$metric_name=="Number" & dc$year == 2017) %>%
  group_by(cause_name) %>% 
  summarise(value=sum(val)) %>%
  arrange(value)

dc2$cause_name <- factor(dc2$cause_name, levels=dc2$cause_name)
dc2$valy <- dc2$value/1000 + ifelse(dc2$value<250000, 20, -40) # unit kDALY/a

plot_cause <- ggplot(dc2, aes(x=cause_name,weight=value/1000,fill=cause_name, label=round(value/1000)))+
  geom_bar(position="stack")+geom_text(aes(y=valy))+coord_flip()+
  guides(fill=FALSE)+
  scale_fill_manual(values = rev(palet))+ 
  labs(
    title="Tautitaakka Suomessa syittäin 2017",
    x="Tauti tai haitta",
    y="Tautitaakka (tuhatta DALYa vuodessa)")

plot_cause
# ggsave("Tautitaakka Suomessa 2017.svg", width=8, height=5) # Png conversion: 300 pixels/inch, font size 4 times larger

############## DALYs by causes and risks

dcr <- as_tibble(
  opasnet.csv("2/2d/IHME_Fin_Risks_by_Cause.zip", wiki="opasnet_en",
              unzip="IHME-GBD_2017_DATA-8c9ca17f-1.csv",sep=",",dec=".",header=TRUE)
) %>%
  left_join(transl[transl$Type=="Cause",], by=c("cause_id"="Id")) %>%
  mutate(cause_name = factor(Result, levels=dc2$cause_name)) %>% # transl combines some entries from cause_name to Name (in Finnish: Result)
  left_join(transl[transl$Type=="Risk",], by=c("rei_id"="Id")) %>%
  mutate(rei_name=Result.y)

dcr2 <- dcr %>%
  filter(year==2017) %>%
  group_by(rei_name) %>%
  summarise(value = sum(val)) %>%
  arrange(value)

dcr <- dcr %>%
  mutate(rei_name = factor(rei_name, levels= dcr2$rei_name)) %>% # transl combines some entries from cause_name to Name (in Finnish: Result)
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)" & metric_name=="Number")

plot_risk <- ggplot(dcr[dcr$year==2017 , ],
                    aes(x=rei_name, weight=val/1000, fill=cause_name))+geom_bar()+coord_flip()+
  scale_fill_manual(values = rev(palet)[-8])+ # Skin diseases missing, so remove that colour 
  guides(fill = guide_legend(reverse=TRUE, title=NULL, keyheight=0.8))+
  theme_gray(base_size=11)+ theme(legend.position=c(0.83,0.29)) + 
  labs(
    title = "Tautitaakka Suomessa 2017 tunnettujen riskitekijöiden mukaan",
    x="Riskitekijä", y="Tautitaakka (tuhatta DALYa vuodessa)")

plot_risk
# ggsave("Tautitaakka Suomessa 2017 riskitekijöittäin.svg", width=8, height=6) # Png conversion: 300 pixels/inch, font size 4 times larger

########################### Graphs with timeline

dc$cause_name <- factor(dc$cause_name, levels=dc2$cause_name)
levels(dc$cause_name)[0:9-length(levels(dc2$cause_name))] <- "Muut"
dc <- dc[dc$measure_name=="DALYs (Disability-Adjusted Life Years)" & dc$metric_name=="Number",]

plot_cause_time <- ggplot(dc, aes(x=year,weight=val*10E-6,fill=cause_name))+geom_bar(position="stack")+
  guides(fill=guide_legend(title="Tauti tai haitta"))+
  labs(
    title="Tautitaakka Suomessa syittäin",
    x="Vuosi",
    y="Tautitaakka (miljoonaa DALYa vuodessa)")

dcrenvh <- dcr[dcr$rei_id %in% c(82,85,89,125,126,135,381), ] # Environmental health risk factors

plot_envhrisk_time <- ggplot(dcrenvh, aes(x=year, weight=val, fill=rei_name))+geom_bar(position="stack")+
  guides(fill=guide_legend(title="Riskitekijä"))+
  labs(
    title="Ympäristöterveyden tautitaakka Suomessa",
    x="Vuosi",
    y="Tautitaakka (DALY/a)"
  )

plot_envhrisk_time

levels(dcr$rei_name)[0:9 - length(levels(dcr$rei_name))] <- "Muu riskitekijä"

plot_risk_time <- ggplot(dcr,
                         aes(x=year,weight=val,fill=rei_name))+geom_bar(position="stack")+
  labs(
    title="Tautitaakka Suomessa riskitekijöittäin",
    x="Vuosi",
    y="Tautitaakka (DALY/a)")

plot_risk_time

if(FALSE) { # Graphs compatible with HNH2035 action plan.
  
  pushIndicatorGraph(
    plot_ly(dcrenvh[dcrenvh$rei_id %in% c(82,85,89,125,126,135,381), ],
            x=~year, y=~val, color=~as.character(rei_name), type="bar") %>%
      layout(
        barmode="stack",
        title="Ympäristöterveyden tautitaakka Suomessa",
        xaxis=list(title="Vuosi"),
        yaxis=list(title="Tautitaakka (DALY/a)")
      )
    ,84
  )
  
  pushIndicatorGraph(
    plot_ly(dc,
            x=~year, y=~val, color=~cause_name, type="bar") %>%
      layout(
        barmode="stack",
        title="Kokonaistautitaakka Suomessa",
        xaxis=list(title="Vuosi"),
        yaxis=list(title="Tautitaakka (DALY/a)")
      )
    ,83
  )
}

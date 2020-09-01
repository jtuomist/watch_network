
#############################
### Create the list of ALas model classes

unique(tie[5:6]) # Tarkimmat tasot liikenteessä
unique(df[8:9]) # Tarkimmat tasot kuntien päästöissä

# tie ja df sisältävät samat tasot 4 ja 5 paitsi liikenteessä taso 5 Moottoripyörät ja mopot jakautuu vielä pienempiin ryhmiin.
# Lisätään nämä yhteiseen jaotteluun.

luokat <- unique(df[5:9])
luokat <- rbind(
  luokat[luokat$level4!="Moottoripyörät ja mopot",],
  merge(luokat[colnames(luokat)!="level5"], unique(tie[tie$level4=="Moottoripyörät ja mopot",5:6]))
)

luokat <- luokat[order(luokat$taso_1, luokat$taso_2, luokat$taso_3, luokat$level4),]
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

colnames(df)[c(4,6,1,14,3,2,13,12)] <- c("Oldid", "type", "Item", "id", "rel", "Object", "Description", "Context")

gr <- makeGraph(df)

render_graph(gr)

# Does not work?!?
# export_graph(gr, file_name="insight_network_of_standard_indicators.svg", file_type="svg")

###############################################3
# Data analysis of CDP cities

library(jsonlite)

dt <- fromJSON("https://data.cdp.net/resource/542d-zyj8.json")

#> colnames(dt)
#[1] "year_reported_to_cdp"                                                                                                                           
#[2] "account_number"                                                                                                                                 
#[3] "organization"                                                                                                                                   
#[4] "city"                                                                                                                                           
#[5] "country"                                                                                                                                        
#[6] "cdp_region"                                                                                                                                     
#[7] "reporting_authority"                                                                                                                            
#[8] "access"                                                                                                                                         
#[9] "city_wide_emissions_inventory"                                                                                                                  
#[10] "accounting_year"                                                                                                                                
#[11] "primary_protocol"                                                                                                                               
#[12] "common_reporting_framework_inventory_format_gpc"                                                                                                
#[13] "change_in_emissions"                                                                                                                            
#[14] "population"                                                                                                                                     
#[15] "population_year"                                                                                                                                
#[16] "geocoded_column"                                                                                                                                
#[17] "last_update"                                                                                                                                    
#[18] ":@computed_region_n7dz_xzkg"                                                                                                                    
#[19] "inventory_boundary"                                                                                                                             
#[20] "primary_protocol_comment"                                                                                                                       
#[21] "gases_included"                                                                                                                                 
#[22] "total_scope_1_emissions_metric_tonnes_co2e"                                                                                                     
#[23] "total_scope_2_emissions_metric_tonnes_co2e"                                                                                                     
#[24] "total_scope_3_emissions"                                                                                                                        
#[25] "total_basic_emissions_gpc"                                                                                                                      
#[26] "total_basic_emissions_gpc_1"                                                                                                                    
#[27] "reason_for_change"                                                                                                                              
#[28] "direct_emissions_scope_1_metric_tonnes_co2e_for_total_generation_of_grid_supplied_energy"                                                       
#[29] "direct_emissions_scope_1_metric_tonnes_co2e_for_total_emissions_excluding_generation_of_grid_supplied_energy"                                   
#[30] "indirect_emissions_from_use_of_grid_supplied_energy_scope_2_metric_tonnes_co2e_for_total_emissions_excluding_generation_of_grid_supplied_energy"
#[31] "emissions_occurring_outside_city_boundary_scope_3_metric_tonnes_co2e_for_total_emissions_excluding_generation_of_grid_supplied_energy"          
#[32] "indirect_emissions_from_use_of_grid_supplied_energy_scope_2_metric_tonnes_co2e_for_total_generation_of_grid_supplied_energy"                    
#[33] "emissions_occurring_outside_city_boundary_scope_3_metric_tonnes_co2e_for_total_generation_of_grid_supplied_energy"                              

colnames(dt)[22:26] <- c("scope1", "scope2", "scope3", "basic","basic+")
colnames(dt)[28:33] <- c("scope1_for_grid","scope1_excl_grid","scope2_for_grid","scope2_excl_grid","scope3_for_grid","scope3_excl_grid")

tmp <- lapply(dt[28:33], FUN=is.na)
table(tmp[c(1,3,5)+1])
table(tmp[1:2])
table(is.na(dt[[22]]),is.na(dt[[25]]),tmp[[2]])
tmp$all_or_none <- tmp[]
table(dt[["primary_protocol"]], useNA = "ifany")
dt$protocol <- ifelse(is.na(dt$primary_protocol), "Not known",
                      ifelse(dt$primary_protocol=="Global Protocol for Community Greenhouse Gas Emissions Inventories (GPC)","GPC",
                             ifelse(dt$primary_protocol=="2006 IPCC Guidelines for National Greenhouse Gas Inventories", "IPCC","Other")))
table(dt$protocol,is.na(dt[[25]]), useNA = "ifany")
# Basic/basic+ classification seems to be specific to GPC, because everyone who has reported those have used GPC protocol.
table(dt$protocol,is.na(dt[[22]]), useNA = "ifany")
# Scope1/2/3 classification has been used in mostly GPC, but 17 cities using other protocols have used that as well.
table(dt$protocol,is.na(dt[[29]]), useNA = "ifany")
# Of those 277 cities who have not reported a protol also have not reported any emissions. Protocol may be a mandatory field for reporting emissions.

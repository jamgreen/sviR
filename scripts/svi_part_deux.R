#another attempt at the CDC SVI...ideally collabing with Mjumbe
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, tidycensus, sf)



###########################################################################
###########################################################################
###                                                                     ###
###                           DATA COLLECTION                           ###
###                                                                     ###
###########################################################################
###########################################################################


##################################################################
##                 Socioeconomic Status Section                 ##
##################################################################

basic_demo <- get_acs(geography = "zcta", 
                variables = c(tot_pop = "S0601_C01_001",
                              hsg_units = "DP04_0001",
                              hholds = "DP02_0001"), year = 2021, 
                output = "wide", geometry = TRUE)

rpl1 <- get_acs(geography = "zcta", 
                variables = c(below_150_pov = "S1701_C01_040",
                              unemployed = "DP03_0005",
                              hsg_inc_20k = "S2503_C01_028",
                              hsg_inc_20_35k = "S2503_C01_032",
                              hsg_inc_35_49k = "S2503_C01_036",
                              hsg_inc_50_75k = "S2503_C01_040",
                              no_high_school = "B06009_002",
                              uninsured_pop = "S2701_C04_001"), 
                year = 2021, output = "wide")

#################################################################
##                  Household Characteristics                  ##
#################################################################

rpl2 <- get_acs(geography = "zcta", 
                variables = c(older_65 = "S0101_C01_030",
                              younger_17 = "B09001_001",
                              disability_pop = "DP02_0072",
                              single_dad = "B11012_010",
                              single_mom = "B11012_015"), year = 2021, 
                output = "wide")

limited_english <- get_acs(geography = "zcta",
                           variables = c("B16005_007E",
                                           "B16005_008E",
                                           "B16005_012E",
                                           "B16005_013E",
                                           "B16005_017E",
                                           "B16005_018E",
                                           "B16005_022E",
                                           "B16005_023E",
                                           "B16005_029E",
                                           "B16005_030E",
                                           "B16005_034E",
                                           "B16005_035E",
                                           "B16005_039E",
                                           "B16005_040E",
                                           "B16005_044E",
                                           "B16005_045E"), year = 2021,
                           output = "wide")


#################################################################
##                Racial/Ethnic Minority Status                ##
#################################################################

rpl3 <- get_acs(geography = "zcta",
                variables = c("DP05_0071E",
                                "DP05_0078E",
                                "DP05_0079E",
                                "DP05_0080E",
                                "DP05_0081E",
                                "DP05_0082E",
                                "DP05_0083E"), year = 2021,
                output = "wide")

#################################################################
##                 Housing Type/Transportation                 ##
#################################################################

rpl4 <- get_acs(geography = "zcta", 
                variables = c(hsg_10_19_units = "DP04_0012",
                              hsg_20_plus_units = "DP04_0013",
                              mobile_homes = "DP04_0014",
                              hsg_crowding_1 = "DP04_0078",
                              hsg_crowding_2 = "DP04_0079",
                              no_autos = "DP04_0058",
                              group_quarters = "B26001_001"), year = 2021, 
                output = "wide")

############################################################################
############################################################################
###                                                                      ###
###                         INDIVIDUAL RPL CALCS                         ###
###                                                                      ###
############################################################################
############################################################################

#################################################################
##                 Total Counts                                ##
#################################################################

rpl1 <- rpl1 %>%
  mutate(E_POV150 = (below_150_povE),
          E_UNEMP = (unemployedE),
          E_HBURD = (hsg_inc_20kE +
                      hsg_inc_20_35kE +
                      hsg_inc_35_49kE +
                      hsg_inc_50_75kE),
          E_NOHSDP = (no_high_schoolE),
          E_UNINSUR = (uninsured_popE))

#################################################################
##                 Percentages                                  ##
#################################################################

rpl1 <- rpl1 %>%
  left_join(basic_demo %>% select(GEOID, tot_popE), by = "GEOID") %>%
  mutate(EP_POV150 = (E_POV150 / tot_popE) * 100,
          EP_UNEMP = (E_UNEMP / tot_popE) * 100,
          EP_HBURD = (E_HBURD / tot_popE) * 100,
          EP_NOHSDP = (E_NOHSDP / tot_popE) * 100,
          EP_UNINSUR = (E_UNINSUR / tot_popE) * 100) %>%
  select(-tot_popE)

#################################################################
##                 Percentile Ranks                             ##
#################################################################

rpl1 <- rpl1 %>%
  mutate(EPL_POV150 = percent_rank(EP_POV150),
          EPL_UNEMP = percent_rank(EP_UNEMP),
          EPL_HBURD = percent_rank(EP_HBURD),
          EPL_NOHSDP = percent_rank(EP_NOHSDP),
          EPL_UNINSUR = percent_rank(EP_UNINSUR))

#################################################################
##                 Final RPL Calculations                       ##
#################################################################

rpl1 <- rpl1 %>%
  mutate(SPL_THEME1 = (EPL_POV150 +
                        EPL_UNEMP +
                        EPL_HBURD +
                        EPL_NOHSDP +
                        EPL_UNINSUR))

rpl1 <- rpl1 %>%
  mutate(RPL_THEME1 = percent_rank(SPL_THEME1))

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
                              EP_AGE65 = "S0101_C02_030",
                              younger_17 = "B09001_001",
                              EP_DISABL = "DP02_0072P",
                              disability_pop = "DP02_0072",
                              single_dad = "B11012_010",
                              single_mom = "B11012_015",
                              pop_5_older = "B16005_001"), year = 2021,
                output = "wide")

limited_english <- get_acs(geography = "zcta",
                           variables = c("B16005_007",
                                           "B16005_008",
                                           "B16005_012",
                                           "B16005_013",
                                           "B16005_017",
                                           "B16005_018",
                                           "B16005_022",
                                           "B16005_023",
                                           "B16005_029",
                                           "B16005_030",
                                           "B16005_034",
                                           "B16005_035",
                                           "B16005_039",
                                           "B16005_040",
                                           "B16005_044",
                                           "B16005_045"), year = 2021,
                           output = "wide")


#################################################################
##                Racial/Ethnic Minority Status                ##
#################################################################

rpl3 <- get_acs(geography = "zcta",
                variables = c("DP05_0071",
                                "DP05_0078",
                                "DP05_0079",
                                "DP05_0080",
                                "DP05_0081",
                                "DP05_0082",
                                "DP05_0083"), year = 2021,
                output = "wide")

#################################################################
##                 Housing Type/Transportation                 ##
#################################################################

rpl4 <- get_acs(geography = "zcta",
                variables = c(hsg_10_19_units = "DP04_0012",
                              hsg_20_plus_units = "DP04_0013",
                              mobile_homes_per = "DP04_0014P",
                              mobile_homes = "DP04_0014",
                              hsg_crowding_1 = "DP04_0078",
                              hsg_crowding_2 = "DP04_0079",
                              no_autos_per = "DP04_0058P",
                              no_autos = "DP04_0058",
                              group_quarters = "B26001_001",
                              hsg_units_occupied = "DP04_0002"), year = 2021,
                output = "wide")

############################################################################
############################################################################
###                                                                      ###
###                         INDIVIDUAL RPL CALCS                         ###
###                                                                      ###
############################################################################
############################################################################

#################################################################
##                 RPL1                                        ##
#################################################################

# Total Counts
rpl1 <- rpl1 %>%
  mutate(E_POV150 = (below_150_povE),
          E_UNEMP = (unemployedE),
          E_HBURD = (hsg_inc_20kE +
                      hsg_inc_20_35kE +
                      hsg_inc_35_49kE +
                      hsg_inc_50_75kE),
          E_NOHSDP = (no_high_schoolE),
          E_UNINSUR = (uninsured_popE))

# Percentages
rpl1 <- rpl1 %>%
  left_join(basic_demo %>% select(GEOID, tot_popE), by = "GEOID") %>%
  mutate(EP_POV150 = (E_POV150 / tot_popE) * 100,
          EP_UNEMP = (E_UNEMP / tot_popE) * 100,
          EP_HBURD = (E_HBURD / tot_popE) * 100,

          # docs say to use S0601_C01_033E, but don't call it out
          # in the data collection phase.
          EP_NOHSDP = (E_NOHSDP / tot_popE) * 100,

          # docs say to use S2701_C05_001E, but don't call it out
          # in the data collection phase.
          EP_UNINSUR = (E_UNINSUR / tot_popE) * 100) %>%
  select(-tot_popE)

# Percentile Ranks
rpl1 <- rpl1 %>%
  mutate(EPL_POV150 = percent_rank(EP_POV150),
          EPL_UNEMP = percent_rank(EP_UNEMP),
          EPL_HBURD = percent_rank(EP_HBURD),
          EPL_NOHSDP = percent_rank(EP_NOHSDP),
          EPL_UNINSUR = percent_rank(EP_UNINSUR))

# Final RPL1 Calculations
rpl1 <- rpl1 %>%
  mutate(SPL_THEME1 = (EPL_POV150 +
                        EPL_UNEMP +
                        EPL_HBURD +
                        EPL_NOHSDP +
                        EPL_UNINSUR))

rpl1 <- rpl1 %>%
  mutate(RPL_THEME1 = percent_rank(SPL_THEME1))

##----------------------------------------------------------------
##                      RPL 2 Calculations                       -
##----------------------------------------------------------------

rpl2 <- rpl2 %>%
  select(-NAME) %>%
  left_join(basic_demo, by = "GEOID") %>%
  mutate(EP_AGE17 = (younger_17E/tot_popE)*100,
         EP_SNGPNT = ((single_dadE + single_momE)/hholdsE)*100)

limited_english <- limited_english %>%
  mutate(E_LIMENG = B16005_007E + B16005_008E +
            B16005_012E + B16005_013E +
           B16005_017E + B16005_018E +
           B16005_022E + B16005_023E +
           B16005_029E + B16005_030E +
           B16005_034E + B16005_035E +
           B16005_039E + B16005_040E +
           B16005_044E + B16005_045E) %>%
  select(GEOID, E_LIMENG)

rpl2 <- rpl2 %>%
  left_join(limited_english) %>%
  mutate(EP_LIMENG = (E_LIMENG/pop_5_olderE)*100)

rpl2 <- rpl2 %>%
  mutate(EPL_AGE65 = percent_rank(EP_AGE65E),
         EPL_AGE17 = percent_rank(EP_AGE17),
         EPL_DISAB = percent_rank(EP_DISABLE),
         EPL_SINGPNT = percent_rank(EP_SNGPNT),
         EPL_LIMENG = percent_rank(EP_LIMENG),
         SPL_THEME2 = EPL_AGE65 + EPL_AGE17 +
         EPL_DISAB + EPL_SINGPNT + EPL_LIMENG,
         RPL_THEME2 = percent_rank(SPL_THEME2))

rpl2 <- rpl2 %>%
  as_tibble() %>%
  select(-geometry)

#################################################################
##                 RPL3                                        ##
#################################################################

# Total Counts
rpl3 <- rpl3 %>%
  mutate(E_MINRTY = (DP05_0071E +
                      DP05_0078E +
                      DP05_0079E +
                      DP05_0080E +
                      DP05_0081E +
                      DP05_0082E +
                      DP05_0083E))

# Percentages
rpl3 <- rpl3 %>%
  left_join(basic_demo %>% select(GEOID, tot_popE), by = "GEOID") %>%
  mutate(EP_MINRTY = (E_MINRTY / tot_popE) * 100) %>%
  select(-tot_popE)

# Percentile Ranks
rpl3 <- rpl3 %>%
  mutate(EPL_MINRTY = percent_rank(EP_MINRTY))

# Final RPL3 Calculations
rpl3 <- rpl3 %>%
  mutate(SPL_THEME3 = (EPL_MINRTY))

rpl3 <- rpl3 %>%
  mutate(RPL_THEME3 = percent_rank(SPL_THEME3))

rpl3 <- rpl3 %>%
  as_tibble()

##----------------------------------------------------------------
##                      RPL 4 Calculations                       -
##----------------------------------------------------------------

rpl4 <- rpl4 %>%
  select(-NAME) %>%
  left_join(basic_demo, by = "GEOID")

rpl4 <- rpl4 %>%
  mutate(EP_MUNIT = ((hsg_10_19_unitsE + hsg_20_plus_unitsE)/hsg_unitsE)*100,
         EP_MOBILE = mobile_homes_perE,
         EP_CROWD = ((hsg_crowding_1E + hsg_crowding_2E)/hsg_units_occupiedE)*100,
         EP_NOVEH = no_autos_perE,
         EP_GROUPQ = (group_quartersE/tot_popE)*100)

rpl4 <- rpl4 %>%
  mutate(EPL_MUNIT = percent_rank(EP_MUNIT),
         EPL_MOBILE = percent_rank(EP_MOBILE),
         EPL_CROWD = percent_rank(EP_CROWD),
         EPL_NOVEH = percent_rank(EP_NOVEH),
         EPL_GROUPQ = percent_rank(EP_GROUPQ),
         SPL_THEME4 = EPL_MUNIT + EPL_MOBILE +
           EPL_CROWD + EPL_NOVEH + EPL_GROUPQ,
         RPL_THEME4 = percent_rank(SPL_THEME4))

rpl4 <- rpl4 %>%
  as_tibble() %>%
  select(-geometry)

#################################################################
##                 Combined SVI                                ##
#################################################################

svi <- rpl1 %>%
  left_join(rpl2, by = "GEOID") %>%
  left_join(rpl3, by = "GEOID") %>%
  left_join(rpl4, by = "GEOID") %>%
  mutate(SPL_THEMES = (SPL_THEME1 + SPL_THEME2 +
                       SPL_THEME3 + SPL_THEME4)) %>%
  mutate(RPL_THEMES = percent_rank(SPL_THEMES))
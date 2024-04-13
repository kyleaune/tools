#---- Script Metadata #----
# Title: CDC/ATSDR SVI Rebalance
# Date: 04/12/2024
# Author: Kyle T. Aune, PhD, MPH
#--------------------------

#---- Libraries #----
pkgs <- c("tidyverse", "tidycensus", "httr", "jsonlite")

invisible(lapply(pkgs, library, character.only = TRUE))

# Set Census parameters for local rebalance

year <- 2020
survey <- "acs5"
geo <- "tract"
state <- "MD"
county <- "Baltimore city"

# Should tract values be aggregated up to community statistical area?
csa.agg <- TRUE

# Output directory
dir <- "~/OneDrive - Johns Hopkins/"


#---- Download Required Census Variables #----

# Variable definitions and calculations from:
#   https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html

vars <- c(
  "S0601_C01_001E", # Total population
  "DP04_0001E", # Housing units (HU)
  "DP04_0002E", # Occupied HU
  "DP02_0001E", # Households
  "S1701_C01_040E", # Persons <150% poverty
  "DP03_0005E", # Civilian unemployed (>16) population
  "DP03_0001E", # Civilian population >16
  "S1810_C01_001E", # Civial noninstitutionalized population
  "S1811_C01_001E", # Civilian noninstitutionalized population >16 population
  "S2503_C01_028E", # HU with income < $20k & housing cost > 30% income
  "S2503_C01_032E", # HU with income $20-35k & housing cost > 30% income
  "S2503_C01_036E", # HU with income #35-50k & housing cost > 30% income
  "S2503_C01_040E", # HU with income $50-75k & housing cost > 30% income
  "B06009_002E", # Population (>25) without HS diploma
  "S1501_C01_006E", # Population >25
  "S2701_C04_001E", # Uninsured population
  "S0101_C01_030E", # Population >65
  "B09001_001E", # Population <17
  "DP02_0072E", # Disabled population
  "B11012_010E", # Male single parent
  "B11012_015E", # Female single parent
  "B16005_007E", # Native Spanish speaker, speaks English "not well"
  "B16005_008E", # Native Spanish speaker, speaks English "not at all"
  "B16005_012E", # Native Indo-European language speaker, speaks English "not well"
  "B16005_013E", # Native Indo-European language  speaker, speaks English "not at all"
  "B16005_017E", # Native Asian and Pacific Island language speaker, speaks English "not well"
  "B16005_018E", # Native Asian and Pacific Island language speaker, speaks English "not at all"
  "B16005_022E", # Native other language speaker, speaks English "not well"
  "B16005_023E", # Native other language speaker, speaks English "not at all"
  "B16005_029E", # Foreign-born Spanish speaker, speaks English "not well"
  "B16005_030E", # Foreign-born Spanish speaker, speaks English "not at all"
  "B16005_034E", # Foreign-born Indo-European language speaker, speaks English "not well"
  "B16005_035E", # Foreign-born Indo-European language  speaker, speaks English "not at all"
  "B16005_039E", # Foreign-born Asian and Pacific Island language speaker, speaks English "not well"
  "B16005_040E", # Foreign-born Asian and Pacific Island language speaker, speaks English "not at all"
  "B16005_044E", # Foreign-born other language speaker, speaks English "not well"
  "B16005_045E", # Foreign-born other language speaker, speaks English "not at all"
  "B16005_001E", # Population age 5 and older
  "DP05_0071E", # Hispanic or Latino population
  "DP05_0078E", # Non-Hispanic Black population
  "DP05_0079E", # Non-Hispanic American Indian population
  "DP05_0080E", # Non-Hispanic Asian population
  "DP05_0081E", # Non-Hispanic Hawaiian or Pac Isl population
  "DP05_0082E", # Non-Hispanic >1 race population
  "DP05_0083E", # Non-Hispanic other race population
  "DP04_0012E", # Population in multi-unit structure (10-19 units)
  "DP04_0013E", # Population in multi-unit structure (>19 units)
  "DP04_0014E", # Population in mobile home
  "DP04_0078E", # Population with 1-1.5 occupants per room
  "DP04_0079E", # Population with >1.5 occupants per room
  "DP04_0058E", # Households with no vehicles
  "B26001_001E", # Population in group quarters
  "S1701_C01_001E", # Population for whom poverty rate is determined
  "DP03_0009PE", # Unemployment rate
  "S2503_C01_001E" # Occupied HU
  )

# Download 2020 ACS 5-year estimates at pre-specified level
acs <- get_acs(
  geography = geo,
  variables = vars,
  survey = survey,
  year = year,
  state = state,
  county = county,
  output = "wide")

# Filtering for only estimates
acs <- acs %>%
  select(any_of(c("GEOID", vars))) %>%
  # Renaming native census fields
  rename(
    E_TOTPOP = S0601_C01_001E,
    E_HU = DP04_0001E,
    E_HH = DP02_0001E,
    E_POV150 = S1701_C01_040E,
    E_UNEMP = DP03_0005E,
    E_NOHSDP = B06009_002E,
    E_UNINSUR = S2701_C04_001E,
    E_AGE65 = S0101_C01_030E,
    E_AGE17 = B09001_001E,
    E_DISABL = DP02_0072E,
    E_MOBIL = DP04_0014E,
    E_NOVEH = DP04_0058E,
    E_GROUPQ = B26001_001E) %>%
  # Calculating combined fields
  mutate(
    E_HBURD = S2503_C01_028E +
      S2503_C01_032E +
      S2503_C01_036E +
      S2503_C01_040E,
    E_SNGPNT = B11012_010E +
      B11012_015E,
    E_LIMENG = B16005_007E +
      B16005_008E +
      B16005_012E +
      B16005_013E +
      B16005_017E +
      B16005_018E +
      B16005_022E +
      B16005_023E +
      B16005_029E +
      B16005_030E +
      B16005_034E +
      B16005_035E +
      B16005_039E +
      B16005_040E +
      B16005_044E +
      B16005_045E,
    E_MINRTY = DP05_0071E +
      DP05_0078E +
      DP05_0079E +
      DP05_0080E +
      DP05_0081E +
      DP05_0082E +
      DP05_0083E,
    E_MUNIT = DP04_0012E +
      DP04_0013E,
    E_CROWD = DP04_0078E +
      DP04_0079E)


#---- Aggregating to CSA #----

if (csa.agg == TRUE) {
  # Importing 2020 census tract to CSA crosswalk file fron BNIA
  csa.cw.json <- fromJSON(content(
    GET(
      "https://services1.arcgis.com/mVFRs7NF4iFitgbY/arcgis/rest/services/Tract2020_to_CSA2020/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
    ),
    "text"
  ),
  flatten = TRUE)
  
  csa.cw<- jsonlite::flatten(csa.cw.json$features) %>%
    # Renaming fields
    rename(GEOID = attributes.GEOID_Tract_2020,
           CSA = attributes.Community_Statistical_Area_2020) %>%
    # Keeping only tract and CSA columns
    select(GEOID, CSA)
  
  # Joining CSA to each census tract in ACS data
  acs <- acs %>%
    left_join(csa.cw, by = "GEOID")
  
  csa <- acs %>%
    group_by(CSA) %>%
    summarize(
      across(
        .cols = c(E_POV150, S1701_C01_001E, E_UNEMP, DP03_0001E,
                  E_HBURD, S2503_C01_001E, E_NOHSDP, S1501_C01_006E,
                  E_UNINSUR, S1810_C01_001E, E_AGE65, E_TOTPOP, E_AGE17,
                  E_DISABL, E_SNGPNT, E_HH, E_LIMENG, B16005_001E,
                  E_MINRTY, E_MUNIT, E_HU, E_MOBIL, E_CROWD, DP04_0002E,
                  E_NOVEH, E_GROUPQ),
        .fn = ~ sum(.x),
        .names = "{.col}"
      )
    )
}


#---- SVI Function #----

# Function to calculate percentages, rank variables, and calculate SVI
svifx <- function(df) {
  df <- df %>%
    # Calculating percentages
    mutate(
      EP_POV150 = (E_POV150 / S1701_C01_001E) * 100,
      EP_UNEMP = (E_UNEMP / DP03_0001E) * 100,
      EP_HBURD = (E_HBURD / S2503_C01_001E) * 100,
      EP_NOHSDP = (E_NOHSDP / S1501_C01_006E) * 100,
      EP_UNINSUR = (E_UNINSUR / S1810_C01_001E) * 100,
      EP_AGE65 = (E_AGE65 / E_TOTPOP) * 100,
      EP_AGE17 = (E_AGE17 / E_TOTPOP) * 100,
      EP_DISABL = (E_DISABL / S1810_C01_001E) * 100,
      EP_SNGPNT = (E_SNGPNT / E_HH) * 100,
      EP_LIMENG = (E_LIMENG / B16005_001E) * 100,
      EP_MINRTY = (E_MINRTY / E_TOTPOP) * 100,
      EP_MUNIT = (E_MUNIT / E_HU) * 100,
      EP_MOBIL = (E_MOBIL / E_HU) * 100,
      EP_CROWD = (E_CROWD / DP04_0002E) * 100,
      EP_NOVEH = (E_NOVEH / E_HH) * 100,
      EP_GROUPQ = (E_GROUPQ / E_TOTPOP) * 100
    ) %>%
    # Calculate percentiles
    mutate(
      EPL_POV150 = percent_rank(EP_POV150),
      EPL_UNEMP = percent_rank(EP_UNEMP),
      EPL_HBURD = percent_rank(EP_HBURD),
      EPL_NOHSDP = percent_rank(EP_NOHSDP),
      EPL_UNINSUR = percent_rank(EP_UNINSUR),
      EPL_AGE65 = percent_rank(EP_AGE65),
      EPL_AGE17 = percent_rank(EP_AGE17),
      EPL_DISABL = percent_rank(EP_DISABL),
      EPL_SNGPNT = percent_rank(EP_SNGPNT),
      EPL_LIMENG = percent_rank(EP_LIMENG),
      EPL_MINRTY = percent_rank(EP_MINRTY),
      EPL_MUNIT = percent_rank(EP_MUNIT),
      EPL_MOBIL = percent_rank(EP_MOBIL),
      EPL_CROWD = percent_rank(EP_CROWD),
      EPL_NOVEH = percent_rank(EP_NOVEH),
      EPL_GROUPQ = percent_rank(EP_GROUPQ)
    ) %>%
    # Calculate SVI percentiles by theme and overall
    mutate(
      # Theme 1 - socioeconomic status
      SPL_THEME1 = EPL_POV150 +
        EPL_UNEMP +
        EPL_HBURD +
        EPL_NOHSDP +
        EPL_UNINSUR,
      # Theme 2 - household characteristics
      SPL_THEME2 = EPL_AGE65 +
        EPL_AGE17 +
        EPL_DISABL +
        EPL_SNGPNT +
        EPL_LIMENG,
      # Theme 3 - racial and ethnic minority status
      SPL_THEME3 = EPL_MINRTY,
      # Theme 4 - housing type / transportation
      SPL_THEME4 = EPL_MUNIT +
        EPL_MOBIL +
        EPL_CROWD +
        EPL_NOVEH +
        EPL_GROUPQ,
      # All themes
      SPL_THEMES = SPL_THEME1 +
        SPL_THEME2 +
        SPL_THEME3 +
        SPL_THEME4
    ) %>%
    # Calculate SVI rankings by theme and overall
    mutate(
      RPL_THEME1 = percent_rank(SPL_THEME1),
      RPL_THEME2 = percent_rank(SPL_THEME2),
      RPL_THEME3 = percent_rank(SPL_THEME3),
      RPL_THEME4 = percent_rank(SPL_THEME4),
      RPL_THEMES = percent_rank(SPL_THEMES)
    )
    
  
  return(df)
}


#---- Rebalancing SVI #----

svi.ct <- svifx(acs) %>%
  select(c(GEOID, EPL_POV150:RPL_THEMES))

write.csv(svi.ct,
          paste0(dir, "2020_CDC-ATSDR_SVI_Baltimore_tract.csv"))

if (csa.agg == TRUE) {
  svi.csa <- svifx(csa) %>%
    select(c(CSA, EPL_POV150:RPL_THEMES))
  
  write.csv(svi.csa,
            paste0(dir, "2020_CDC-ATSDR_SVI_Baltimore_CSA.csv"))
}
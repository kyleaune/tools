#---- Script Metadata #----
# Title: AQS Download Script
# Date: 04/17/2024
# Author: Kyle T. Aune, PhD, MPH
#---------------------------

#---- Setup #----

# Load/install required pacakges
pkgs <- c("tidyverse", "RAQSAPI")

install.packages(setdiff(pkgs, rownames(installed.packages())))

lapply(pkgs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

# Set working directory
setwd("C:/Users/aunek/OneDrive - Johns Hopkins/Research/240416 - SEARCH")

# Setup AQS API credentials
email <- "kaune1@jhu.edu"
key <- "greykit58"

aqs_credentials(email, key)

# Register for an API key (if necessary)
# browseURL(paste0("https://aqs.epa.gov/data/api/signup?email="), email)


#---- Query API for Relevant Setup Info #----

# Criteria Air Pollutants (CAPs) AQS parameter codes
cap.params <- aqs_parameters_by_class("CRITERIA")

# Hazardous Air Pollutants (HAPs) AQS parameter codes
hap.params <- aqs_parameters_by_class("HAPS")

# All AQS parameter codes (this is big, good luck wading through these)
all.params <- aqs_parameters_by_class("ALL")

# Identify site codes using EPA AirData interactive map:
  # https://epa.maps.arcgis.com/apps/webappviewer/index.html?id=5f239fd3e72f424f98ef3d5def547eb5


#---- Download NO2 (ppb) for MDE Stations Near Baltimore (2018-2023) #----

# Site numbers:
  # MDE Essex (2018-2023): 24-005-3001
  # MDE Lake Montabello (2022-2023): 24-510-5253
  # MDE Oldtown (2018-2021): 24-510-0040

# Essex site data
mdesx <- aqs_sampledata_by_site(parameter = "42602",
                                bdate = as.Date("20180101",
                                                format = "%Y%m%d"),
                                edate = as.Date("20231231",
                                                format = "%Y%m%d"),
                                stateFIPS = "24",
                                countycode = "005",
                                sitenum = "3001")
## Method: Teledyne Model T500U - Cavity Attenuated Phase Shift Spectroscopy 

# Lake Montabello data
mdelm <- aqs_sampledata_by_site(parameter = "42602",
                                bdate = as.Date("20220101",
                                                format = "%Y%m%d"),
                                edate = as.Date("20231231",
                                                format = "%Y%m%d"),
                                stateFIPS = "24",
                                countycode = "510",
                                sitenum = "5253")
## Method: Primarily Instrumental - Chemiluminescence Teledyne API 200 EU/501,
## but a 3 month period (04-06/2023) also used Teledyne Model T500U - Cavity
## Attenuated Phase Shift Spectroscopy

# Oldtown data
mdeold <- aqs_sampledata_by_site(parameter = "42602",
                                 bdate = as.Date("20180101",
                                                 format = "%Y%m%d"),
                                 edate = as.Date("20211231",
                                                 format = "%Y%m%d"),
                                 stateFIPS = "24",
                                 countycode = "510",
                                 sitenum = "0040")
## Method: Instrumental - Chemiluminescence Teledyne API 200 EU/501 


#---- MDE Reference Hourly Values #----

# Renaming and formatting columns for merge
mdesx <- mdesx %>%
  rename(no2.essex = sample_measurement) %>%
  mutate(datetime_gmt = as.POSIXct(paste(as.character(date_gmt),
                                         as.character(time_gmt)),
                                   format = "%Y-%m-%d %H:%M",
                                   tz = "UTC"))
mdelm <- mdelm %>%
  rename(no2.lakemont = sample_measurement) %>%
  mutate(datetime_gmt = as.POSIXct(paste(as.character(date_gmt),
                                         as.character(time_gmt)),
                                   format = "%Y-%m-%d %H:%M",
                                   tz = "UTC")) %>%
  # Keeping only chemiluminescence method
  filter(method == "Instrumental - Chemiluminescence Teledyne API 200 EU/501")
mdeold <- mdeold %>%
  rename(no2.oldtown = sample_measurement) %>%
  mutate(datetime_gmt = as.POSIXct(paste(as.character(date_gmt),
                                         as.character(time_gmt)),
                                   format = "%Y-%m-%d %H:%M",
                                   tz = "UTC"))

mde <- mdesx %>%
  dplyr::select(datetime_gmt, no2.essex) %>%
  full_join(mdelm[, c("datetime_gmt", "no2.lakemont")], by = c("datetime_gmt")) %>%
  full_join(mdeold[, c("datetime_gmt", "no2.oldtown")], by = c("datetime_gmt"))

# Save merged hourly data
write.csv(mde, "mde_no2_20180101-20231231.csv")

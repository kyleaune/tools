#---- Script Metadata #----
# Title: Census ACS Mass Backup
# Date: 02/04/2025
# Author: Kyle T. Aune, PhD, MPH
#--------------------------

pkgs <- c("tidyverse", "tidycensus", "arrow", "parallel", "doParallel", "foreach")

# Installing any of the packages that you don't already have
install.packages(setdiff(pkgs, rownames(installed.packages())))

# Loading all the packages
lapply(pkgs, library, character.only = TRUE, quietly = TRUE, verbose = FALSE)

setwd("/Volumes/T7 Shield/Census_ACS_Backup/")

# 5 year ACS at census tract level by state (2009-2023)
yrs <- 2009:2023

# Variable lists by year
vars.acs5 <- lapply(yrs, function(x) {
  acs5 <- load_variables(year = x, dataset = "acs5")
  return(data.frame(acs5var = acs5$name, acs5lab = acs5$label))
})

vars.dp05 <- lapply(yrs, function(x) {
  dp05 <- load_variables(year = x, dataset = "acs5/profile")
  return(data.frame(dp05var = dp05$name, dp05lab = dp05$label) %>%
           filter(grepl("PR", dp05var) == FALSE))
})

# Downloading all years' DP05 at census tract level by state
s <- state.abb

cl <- makeCluster(ceiling(length(state.abb) / (length(state.abb) %% detectCores() + 1)))
registerDoParallel(cl)

foreach(ii = seq_along(s)) %dopar% {
  require(dplyr)
  require(tidycensus)
  require(arrow)
  
  tryCatch({
    mapply(function(y, v) {
      x <- get_acs(
        geography = "tract",
        state = s[ii],
        year = y,
        survey = "acs5",
        variables = v$dp05var,
        output = "wide")
      
      write_parquet(x, paste0("DP05", "_", s[ii], "_tracts_", y, ".parquet"))
      write.csv(v, paste0("DP05", "_", s[ii], "_tracts_", y, "_vars.csv"))
    },
    y = yrs,
    v = vars.dp05,
    SIMPLIFY = FALSE)
  },
  # If an error occurs, print a message, return NA, and keep going
  error = function(e) {
    message(paste0("And error occurred on file ", fn.nldas[ii]))
    print(e)
    return(NA)
  },
  # If a warning occurs, print a message, download the file, and keep going
  warning = function(w) {
    message("A warning occurred")
    print(w)
  })
  
}

stopCluster(cl)

# Downloading all years' ACS5 at census tract level by state
cl <- makeCluster(ceiling(length(state.abb) / (length(state.abb) %% detectCores() + 1)))
registerDoParallel(cl)

foreach(ii = seq_along(s)) %dopar% {
  require(dplyr)
  require(tidycensus)
  require(arrow)
  
  tryCatch({
    mapply(function(y, v) {
      x <- get_acs(
        geography = "tract",
        state = s[ii],
        year = y,
        survey = "acs5",
        variables = v$acs5var,
        output = "wide")
      
      write_parquet(x, paste0("ACS5", "_", s[ii], "_tracts_", y, ".parquet"))
      write.csv(v, paste0("ACS5", "_", s[ii], "_tracts_", y, "_vars.csv"))
    },
    y = yrs,
    v = vars.acs5,
    SIMPLIFY = FALSE)
  },
  # If an error occurs, print a message, return NA, and keep going
  error = function(e) {
    message(paste0("And error occurred on file ", fn.nldas[ii]))
    print(e)
    return(NA)
  },
  # If a warning occurs, print a message, download the file, and keep going
  warning = function(w) {
    message("A warning occurred")
    print(w)
  })
  
}

stopCluster(cl)


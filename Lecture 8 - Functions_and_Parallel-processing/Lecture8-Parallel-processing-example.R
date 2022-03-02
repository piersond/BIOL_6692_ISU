# Parallel processing examples

# Load MIMICS model
# Available at https://github.com/piersond/MIMICS_HiRes
setwd("C:/github/MIMICS_HiRes")
source("MIMICS_ftns/MIMICS_base_ftn.R")

# Load data to run through the MIMICS model
MIM_data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_all_raw.csv")

# Make the dataframe longer for testing purposes
MIM_data <- rbind(MIM_data, MIM_data[rep(1, 5000), ])

# Run the model, measure compution time
start <- Sys.time()
MIM_output <- MIM_data %>% split(1:nrow(MIM_data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
Sys.time() - start



### Setup parallel processing
library(furrr)

# Set number of cores to use
no_cores <- availableCores() - 1
plan(multisession, gc = FALSE, workers = no_cores)

# Run the model using future_map
start <- Sys.time()
MIM_output <- MIM_data %>% split(1:nrow(MIM_data)) %>% future_map(~ MIMICS1(df=.)) %>% bind_rows()
Sys.time() - start

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()
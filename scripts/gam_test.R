library(tidyverse)
library(mgcv)

WATER_METADATA_PHYTOPLANKTON <- read_csv("data/WATER_METADATA_PHYTOPLANKTON.csv")   # 57,945
data <- WATER_METADATA_PHYTOPLANKTON %>% select(datecollected, decimallatitude, decimallongitude, parameter_value, taxaname) %>% 
            mutate(datecollected = as.Date(datecollected)) %>% 
            mutate(datecollected_numeric = as.numeric(datecollected - min(datecollected))) %>% 
            drop_na()  # 57,596

species_list <- unique(data$taxaname)  # List of species
species_models <- list()  # To store models
residuals <- list()

for (species in species_list) {
  species_data <- subset(data, taxaname == species)
  species_models[[species]] <- gam(parameter_value ~ s(datecollected_numeric) + s(decimallatitude, decimallongitude) + s(datecollected_numeric, decimallatitude, decimallongitude), data = data)
  residuals[[species]] <- residuals(species_models[[species]])
}

gam_model <- gam(parameter_value ~ s(datecollected_numeric) + s(decimallatitude, decimallongitude) + s(datecollected_numeric, decimallatitude, decimallongitude), data = data)
gam_model <- gam(parameter_value ~ s(decimallatitude, decimallongitude), data = data)



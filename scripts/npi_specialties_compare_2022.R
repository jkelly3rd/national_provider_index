library(data.table)
library(tidyverse)
library(readxl)

# import the taxonomy list from the data folder
# This is a list of all the taxonomy codes and their descriptions
# We will use this to filter the NPI data for the professionals we want to extract
taxonomy_list <- read_csv("data/Taxonomy_List.csv") %>% janitor::clean_names()


# reference to save for later
# Use the zip county file in the data folder to add the county FIPS code to the specialty file
# ZIP crosswalk that adds the county FIPS number for the county that most of the zip sits in
# This is typically 95+ percent of a zip in a county; but this is the best we can do to assign
# these providers by their addresses to counties
zip2county <- read_excel("data/ZIP_COUNTY_122021.xlsx") %>%
  group_by(zip) %>% # for each unique sample
  arrange(-tot_ratio) %>% # order by total_reads DESC
  slice(1) %>% select(1:4)
colnames(zip2county) <- c("zip","county","city","state_zip")
zip2county <- subset(zip2county, !(zip2county$state_zip %in% c("PR","VI","GU","AS","MP")))
# fixes a few outdated FIPS codes in rural America
zip2county$county <- ifelse(zip2county$county=="46113","46102",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02261","02063",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02270","02158",zip2county$county)

# Import, but immediately reduce to just the bare minimum columns we need to filter taxonomy codes
# We're harvesting the relevant codes for specialties or sub-specialties as a first step
npi <- fread("NPPES_Data_Dissemination_March_2024/npidata_pfile_20050523-20240310.csv", select = c(1:108)) %>% janitor::clean_names()

# Filter by the list of taxonomy codes for the professionals we want to extract
# This can be done code, or multiples separated by commas
taxonomy_codes <- c('103TM1800X', #Psychologist Intellectual & Developmental Disabilities
                    '2080P0008X', #Pediatrics Neurodevelopmental Disabilities
                    '2084P0005X', #Psychiatry & Neurology Neurodevelopmental Disabilities 
                    '2080P0006X') #Pediatrics DevelopmentalBehavioral Pediatrics

# Filter taxonomy columns for the mental health professions and turn into new df
# Using %in% for filter here so that we can reuse this for cases with multiple taxonomy codes
npi_specialty_2024 <- filter(npi, healthcare_provider_taxonomy_code_1 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_2 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_3 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_4 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_5 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_6 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_7 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_8 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_9 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_10 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_11 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_12 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_13 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_14 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_15 %in% taxonomy_codes) 

# Create new field for zip code with just the first five digits
npi_specialty_2024$zip <- substr(npi_specialty_2024$provider_business_practice_location_address_postal_code,1,5)

# Now add the corresponding county to the provider file based on the corresponding zip code for the provider business practice location
npi_specialty_2024 <- left_join(npi_specialty_2024,zip2county %>% select(1:2,4),by=("zip"))

# needs to be a de-dupe check phase in here - possibly if you're going to get granular and use small ns from the analysis
# If we're going to stay small and says counties that have fewer than 5, which might be wise... may not need to further clean?

# Create a pivot table counting the number of providers by county and state
providers_by_county <- npi_specialty_2024 %>%
  group_by(county, state_zip) %>%
  summarise(providers=n())

# Write the specialty file to csv for backup; specify name of specialty and year to be more specific
write.csv(npi_specialty_2024, "npi_pediatrics_devdisability.csv")



# STOP HERE/ IGNORE UNLESS COMPARING
# IF NECESSARY FOR COMPARISON OVER TIME
# Repeat the above steps for the 2022 file for comparison (or whatever year we're comparing against)
npi_2022 <- fread("NPPES_Data_Dissemination_April_2022/npidata_pfile_20050523-20220410.csv", select = c(1:108)) %>% janitor::clean_names()

npi_specialty_2022 <- filter(npi_2022, healthcare_provider_taxonomy_code_1 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_2 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_3 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_4 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_5 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_6 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_7 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_8 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_9 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_10 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_11 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_12 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_13 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_14 %in% taxonomy_codes | 
                               healthcare_provider_taxonomy_code_15 %in% taxonomy_codes) 

# Create new field for zip code with just the first five digits
npi_specialty_2022$zip <- substr(npi_specialty_2022$provider_business_practice_location_address_postal_code,1,5)

# Now add the corresponding county to the provider file based on the corresponding zip code for the provider business practice location
npi_specialty_2022 <- left_join(npi_specialty_2022,zip2county %>% select(1:2,4),by=("zip"))

write.csv(npi_specialty_2022, "npi_fetal_maternal_April22.csv")
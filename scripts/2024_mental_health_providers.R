library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(readr)
library(readxl)
library(htmlwidgets)
library(sf)


# copy npi_specialty_2024 from npi_specialties.R as mentalhealthproviders
mentalhealthproviders <- read_csv("data/npi_mental_health_county.csv")

# Quick pivot table of the number of providers by county
providers_by_county <- mentalhealthproviders %>%
  group_by(county) %>%
  summarise(providers=n())

# Get demographic data for COUNTIES using acs 5 year
# Gives us population and geometry for every county, by FIPS code, needed for mapping later
counties <- get_acs(geography = "county", 
                       year = 2022,
                       output = 'wide',
                       variables = "B03002_001", 
                       geometry = TRUE) %>%
  rename("population"="B03002_001E") %>% 
  select(-4) %>%
  janitor::clean_names()

# this joins the count of providers per county to the geometry file with population from Census Bureau
providers_by_county_formap <- full_join(counties,providers_by_county,
                                         by=c("geoid"="county"))
# replaces the blank NA fields for counties with no providers to actual zeros
providers_by_county_formap$providers[is.na(providers_by_county_formap$providers)] <- 0
# Calculates ratios/rates in two different ways to help us think about this for reporting
# Per Capita number of providers as well as a patient-to-provider ratio; either shows the same
# just two different ways to think about the availability question
providers_by_county_formap$per100Kpeople <- round(providers_by_county_formap$providers/(providers_by_county_formap$population/100000),1)
providers_by_county_formap$prov_patient_ratio <- round(providers_by_county_formap$population/providers_by_county_formap$providers,1)
# For cases with infinity on ratio, changed to ratio NA so areas with zero providers are flagged separately on map later
providers_by_county_formap <- providers_by_county_formap %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
providers_by_county_formap <- providers_by_county_formap %>% filter(!is.na(geoid))

# transforming the projection of the map to something leaflet can work with easily
providers_by_county_formap <- providers_by_county_formap %>% st_transform(4326)

# Set bins for numbers of patients to providers, in four quartiles
# we can change that to five or six, but simple is always better for viewers


bins <- c(0, 1, 10, 50, 100, 500, 1200)
pal <- colorBin(palette="plasma",bins=bins,domain=providers_by_county_formap$per100Kpeople)

# crude popup label, which we can improve on if we decide to publish the map
label <- paste(sep = "<br>", "<b>",providers_by_county_formap$name,
               "<br><b>Number of providers: </b>",providers_by_county_formap$providers,
               "<br><b>Providers Per 100K people: </b>",providers_by_county_formap$per100Kpeople,
               "<br><b>Patient To Provider Ratio: </b>",providers_by_county_formap$prov_patient_ratio)

# creates a color-coded county map based on the ratio of patients to providers in each county
# adds legend that we need to do some more work on to get the wording right; add sourcing; etc
providers_by_county_map <- leaflet(providers_by_county_formap, options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
  setView(-95.6,38.8, zoom = 4) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = label, weight = 1, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.4,
              fillColor = ~pal(`per100Kpeople`)) %>%
addLegend(opacity = 0.4,
          values = ~per100Kpeople, 
          pal=pal,
          position = "topleft", 
          title = "<big>Mental health care access by county</big><br><small>Providers per 100,000 residents. Click any county for details.") 
providers_by_county_map
# saveWidget(providers_by_county_map, 'providers_by_county.html', title = "ABC OTV Mental Health Access By County Interactive Map", selfcontained = TRUE)


# this takes the data we used for the map, removes the geo col (size reasons)
# and then saves for backup as csv for analysis for story
providers_by_county_table <- providers_by_county_formap %>% st_drop_geometry()
providers_by_county_table$state <- substr(providers_by_county_table$geoid,1,2)
providers_by_county_table$state_name <- sub('.*,\\s*', '', providers_by_county_table$name)
providers_by_county_table$under50per <- ifelse(providers_by_county_table$per100Kpeople < 50, "Under50","Not")

providers_by_county_state <- providers_by_county_table %>%
  group_by(state_name,under50per) %>%
  summarise(count=n()) %>% 
  pivot_wider(names_from = under50per, values_from = count)

providers_by_county_state[is.na(providers_by_county_state)] <- 0
providers_by_county_state$percent <- round(providers_by_county_state$Under50/(providers_by_county_state$Under50+providers_by_county_state$Not)*100,1)


write_csv(providers_by_county_table,"providers_by_county_table.csv")
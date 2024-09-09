library(data.table)

# import with just the cols we need to reduce the memory load
npi <- fread("NPPES_Data_Dissemination_September_2024/npidata_pfile_20050523-20240908.csv", select = c(1,48,52,56,60,64,
                                                                      68,72,76,80,84,
                                                                      88,92,96,100,104))

#filter by mental health codes
tax <- c('2084A0401X','2084P0802X', '2084B0040X', '2084P0804X', '2084P0805X', '2084P0800X', '2084P0015X', '2084S0012X', '103T00000X', '103TA0400X', '103TA0700X', '103TC0700X', '103TC2200X', '103TB0200X', '103TC1900X', '103TE1000X', '103TF0000X', '103TP2701X', '103TH0004X', '103TH0100X', '103TM1800X', '103TP0016X', '103TP0814X', '103TP2700X', '103TR0400X', '103TS0200X', '103TW0100X', '363LP0808X', '103K00000X', '103G00000X', '103GC0700X', '101Y00000X', '101YA0400X', '101YP1600X', '101YP2500X', '101YS0200X', '106H00000X', '102L00000X')
tax2 <- c('2084A0401X','2084P0802X', '2084B0040X', '2084P0804X', '2084P0805X', '2084P0800X', '2084P0015X', '2084S0012X', '103T00000X', '103TA0400X', '103TA0700X', '103TC0700X', '103TC2200X', '103TB0200X', '103TC1900X', '103TE1000X', '103TF0000X', '103TP2701X', '103TH0004X', '103TH0100X', '103TM1800X', '103TP0016X', '103TP0814X', '103TP2700X', '103TR0400X', '103TS0200X', '103TW0100X', '363LP0808X', '103K00000X', '103G00000X', '103GC0700X', '101Y00000X', '101YA0400X', '101YP1600X', '101YP2500X', '101YS0200X', '106H00000X', '102L00000X')


#rename taxonomy columns for easier filtering
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_1"] <- "code1"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_2"] <- "code2"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_3"] <- "code3"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_4"] <- "code4"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_5"] <- "code5"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_6"] <- "code6"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_7"] <- "code7"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_8"] <- "code8"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_9"] <- "code9"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_10"] <- "code10"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_11"] <- "code11"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_12"] <- "code12"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_13"] <- "code13"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_14"] <- "code14"
names(npi)[names(npi) == "Healthcare Provider Taxonomy Code_15"] <- "code15"

#filter taxonomy columns for the mental health professions and turn into new df
npi_filter <- filter(npi, code1 %in% tax | code2 %in% tax | code3 %in% tax | code4 %in% tax | code5 %in% tax | code6 %in% tax | code7 %in% tax | code8 %in% tax | code9 %in% tax | code10 %in% tax | code11 %in% tax | code12 %in% tax | code13 %in% tax | code14 %in% tax | code15 %in% tax) %>% select(1)

# import with just the cols we need to reduce the memory load
npi_mental <- fread("NPPES_Data_Dissemination_September_2024/npidata_pfile_20050523-20240908.csv", select = c(1:108)) %>% filter(npi$NPI %in% npi_filter$NPI) %>% janitor::clean_names()

npi_mental_32940 <- npi_mental[npi_mental$provider_business_practice_location_address_postal_code %like% "^32940", ]

npi_mental_32940_missing <- npi_mental_32940 %>% filter(npi_mental_32940$npi %in% mentalhealthproviders$npi)


npi_mental_primary1 <- npi_mental %>% filter(healthcare_provider_taxonomy_code_1 %in% tax & healthcare_provider_primary_taxonomy_switch_1 == "Y" & entity_type_code == "1")
npi_mental_categories <- npi_mental_primary1 %>% group_by(healthcare_provider_taxonomy_code_1) %>% summarise(count=n())


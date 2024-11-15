# R script for EiA version of"carob"

## ISSUES
# 1. Needs finalizing
# 2. Need to add several weeding times

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "1KYnKWUTZbXbnLnb9dB2Wjci"
   group <- "eia"
   
   meta <- data.frame(
      dataset_id = uri,
      uri = uri,
      authors ="Isaiah Nyagumbo, Patricia Masikati, John Omondi",
      publication= NA,
      data_institute = "CIMMYT; ICRAF-CIFOR; IITA",
      title = NA,
      group = group,
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC016",
      usecase_name = 'CH-CerLeg-Solidaridad',
      activity = 'addon',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_type = "survey",
      carob_date="2024-11-14",
      notes = "Field boundary available (but not captured)"
   )
   
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Chinyanja-Solidaridad-Soy-CropCut", full.names = T))
   
   f1 <- ff[basename(ff) == "Yield_cut_survey_for_maize_malawi_zambia_mozambique_2023.xlsx"]
   f2 <- ff[basename(ff) == "Yield_cut_survey_for_soybean_malawi_mozambique_zambia_2023.xlsx"]
   
   r1 <- carobiner::read.excel(f1)
   r2 <- carobiner::read.excel(f2)
   
   d1 <- data.frame(
     dataset_id = uri,
     hhid = r1$farmer_id,
     # record_id,
     country = r1$country,
     adm1 = r1$province,
     adm2 = r1$district,
     adm3 = r1$epa,
     adm4 = r1$section,
     longitude = as.numeric(r1$field_gps_longitude),
     latitude = as.numeric(r1$field_gps_latitude),
     # plot_geom = r1$plot_geotraced_area, # This is interesting to capture
     plot_area = as.numeric(r1$plot_geotraced_area_m2),
     geo_uncertainty = as.numeric(r1$field_gps_precision),
     elevation = as.numeric(r1$field_gps_altitude),
     geo_from_source = TRUE,
     crop = r1$crop_harvested,
     pest_severity = r1$pest_damage,
     disease_severity = r1$diasease_damage,
     plant_density = as.numeric(r1$`plant_population/ha`),
     dm_yield = as.numeric(r1$`dry grain yield (kg/ha)`),
     # land_prep_date = as.Date(as.integer(r1$date_landprep), origin = "1900-01-01"), # Is it relevant?
     planting_date = as.character(as.Date(as.integer(r1$date_planting), origin = "1900-01-01")),
     # seed_density = r1$`seedrate_kg/ha`,
     variety_type = r1$seed_type,
     variety = r1$variety_name,
     weeding_done = as.integer(r1$number_of_weedings) > 0,
     weeding_times = as.integer(r1$number_of_weedings),
     fertilizer_used = as.integer(r1$inorg_input1_kg) > 0 | as.integer(r1$inorg_input2_kg) > 0,
     fertilizer_type1 = paste0(r1$inorg_input1),
     fertilizer_type2 = paste0(r1$inorg_input2),
     fertilizer_amount1 = as.integer(r1$inorg_input1_kg),
     fertilizer_amount2 = as.integer(r1$inorg_input2_kg),
     N_fertilizer = NA,
     P_fertilizer = NA,
     K_fertilizer = NA,
     S_fertilizer = NA,
     herbicide_used = !is.na(as.integer(r1$date_herbicide)),
     herbicide_dates = as.character(as.Date(as.integer(r1$date_herbicide), origin = "1900-01-01")),
     fungicide_used = !is.na(as.integer(r1$date_fungicide)),
     fungicide_dates = as.character(as.Date(as.integer(r1$date_fungicide), origin = "1900-01-01")),
     insecticide_used = !is.na(as.integer(r1$date_insecticide)),
     insecticide_dates = as.character(as.Date(as.integer(r1$date_insecticide), origin = "1900-01-01")),
     harvest_date = as.character(as.Date(as.integer(r1$date_harvest), origin = "1900-01-01")),
     labour_manday = as.numeric(r1$amount_permanday),
     labour_ha = as.numeric(r1$amount_peracre) * 0.4,
     currency = r1$`labor/currency`
   )
   
   d2 <- data.frame(
     dataset_id = uri,
     # record_id,
     country = r2$country,
     adm1 = r2$province,
     adm2 = r2$district,
     adm3 = r2$epa,
     adm4 = r2$section,
     longitude = as.numeric(r2$field_gps_longitude),
     latitude = as.numeric(r2$field_gps_latitude),
     plot_area = r2$plot_geotraced_area_m2,
     geo_uncertainty = r2$field_gps_precision,
     elevation = r2$field_gps_altitude,
     geo_from_source = TRUE,
     crop = r2$crop_harvested,
     pest_severity = r2$pest_damage,
     disease_severity = r2$diasease_damage,
     # plant_density = r2$`plant_population/ha`,
     dm_yield = as.numeric(r2$total_dry_grain_yield_kg_ha),
     # land_prep_date = as.Date(as.integer(r2$date_land_preparation), origin = "1900-01-01"), # Is it relevant?
     planting_date = as.character(as.Date(as.integer(r2$date_planting), origin = "1900-01-01")),
     # seed_density = r2$seedrate_kg_ha,
     variety_type = r2$seed_type,
     variety = r2$variety_name,
     weeding_done = as.integer(r2$number_of_weedings) > 0,
     weeding_times = as.integer(r2$number_of_weedings),
     fertilizer_used = as.integer(r2$inorg_input1_kg) > 0,
     fertilizer_type1 = paste0(r2$inorg_input1),
     fertilizer_amount1 = as.integer(r2$inorg_input1_kg),
     N_fertilizer = NA,
     P_fertilizer = NA,
     K_fertilizer = NA,
     S_fertilizer = NA,
     herbicide_used = !is.na(as.integer(r2$date_herbicide)),
     herbicide_dates = as.character(as.Date(as.integer(r2$date_herbicide), origin = "1900-01-01")),
     fungicide_used = !is.na(as.integer(r2$date_fungicide)),
     fungicide_dates = as.character(as.Date(as.integer(r2$date_fungicide), origin = "1900-01-01")),
     insecticide_used = !is.na(as.integer(r2$date_insecticide)),
     insecticide_dates = as.character(as.Date(as.integer(r2$date_insecticide), origin = "1900-01-01")),
     harvest_date = as.character(as.Date(as.integer(r2$date_harvest), origin = "1900-01-01")),
     labour_manday = r2$amount_permanday,
     labour_ha = as.numeric(r2$amount_peracre) * 0.4,
     currency = r2$`labor/currency`
   )
   
   d <- carobiner::bindr(d1, d2)
   
   # EGB:
   # # Formatting the data a bit...
   d$pest_severity <- ifelse(d$pest_severity == 1, "Very low",
                             ifelse(d$pest_severity == 2, "Low",
                                    ifelse(d$pest_severity == 3, "High", "Very High")))
   d$disease_severity <- ifelse(d$disease_severity == 1, "Very low",
                             ifelse(d$disease_severity == 2, "Low",
                                    ifelse(d$disease_severity == 3, "High", "Very High")))
   
   # # Format fertilizer information
   d$fertilizer_type1[grepl("NPK", d$fertilizer_type1, ignore.case = TRUE)] <- gsub("NPK", "NPK", d$fertilizer_type1[grepl("NPK", d$fertilizer_type1, ignore.case = TRUE)], ignore.case = TRUE)
   d$fertilizer_type2[grepl("NPK", d$fertilizer_type2, ignore.case = TRUE)] <- gsub("NPK", "NPK", d$fertilizer_type2[grepl("NPK", d$fertilizer_type2, ignore.case = TRUE)], ignore.case = TRUE)
   d$fertilizer_type1[grepl("urea", d$fertilizer_type1, ignore.case = TRUE)] <- gsub("urea", "urea", d$fertilizer_type1[grepl("urea", d$fertilizer_type1, ignore.case = TRUE)], ignore.case = TRUE)
   d$fertilizer_type2[grepl("urea", d$fertilizer_type2, ignore.case = TRUE)] <- gsub("Area", "urea", gsub("Ureia", "urea", gsub("urea", "urea", d$fertilizer_type2[grepl("urea", d$fertilizer_type2, ignore.case = TRUE)], ignore.case = TRUE)))
   d$fertilizer_type1[grepl("D compound/urea", d$fertilizer_type1, ignore.case = TRUE)] <- "D-compound; urea"
   d$fertilizer_type1[grepl("pound", d$fertilizer_type1, ignore.case = TRUE)] <- "D-compound"
   d$fertilizer_type2[grepl("pound", d$fertilizer_type2, ignore.case = TRUE)] <- "D-compound"
   d$fertilizer_type1[grepl("chitowe", d$fertilizer_type1, ignore.case = TRUE)] <- "chitowe"
   d$fertilizer_type2[grepl("chitowe", d$fertilizer_type2, ignore.case = TRUE)] <- "chitowe"
   d$fertilizer_type1[grepl("23", d$fertilizer_type1, ignore.case = TRUE)] <- "N23N214S"
   d$fertilizer_type2[grepl("23", d$fertilizer_type2, ignore.case = TRUE)] <- "N23N214S"
   d$fertilizer_type1[grepl("Super", d$fertilizer_type1, ignore.case = TRUE)] <- "unknown"
   d$fertilizer_type2[grepl("Super", d$fertilizer_type2, ignore.case = TRUE)] <- "unknown"
   d$fertilizer_type1[grepl("ocula", d$fertilizer_type1, ignore.case = TRUE)] <- NA
   d$fertilizer_type2[grepl("Can", d$fertilizer_type2, ignore.case = TRUE)] <- "CAN"
   
   
   unique(d$fertilizer_type1)
   
   
   d$N_fertilizer[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] * 0.16
   d$P_fertilizer[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] * 0.05
   d$S_fertilizer[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] * 0.04
   d$fertilizer_type[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)] <- gsub("NPK NPK", "NPK", gsub("Chitowe", "NPK", d$fertilizer_type[grepl("chitowe", d$fertilizer_type, ignore.case = TRUE)]))
   d$fertilizer_type[grepl("NPK", d$fertilizer_type, ignore.case = TRUE)] <- gsub("NPK", "NPK", d$fertilizer_type[grepl("NPK", d$fertilizer_type, ignore.case = TRUE)], ignore.case = TRUE)
   d$fertilizer_type[grepl("UREA", d$fertilizer_type, ignore.case = TRUE)] <- gsub("UREA", "urea", d$fertilizer_type[grepl("UREA", d$fertilizer_type, ignore.case = TRUE)], ignore.case = TRUE)
   d$fertilizer_type[grepl("Ureia", d$fertilizer_type, ignore.case = TRUE)] <- "NPK; urea"
   d$fertilizer_type[grepl("compound", d$fertilizer_type, ignore.case = TRUE)] <- gsub("DCompound", "D-compound", gsub("D  compound", "D-compound", gsub("D compound", "D-compound", d$fertilizer_type[grepl("compound", d$fertilizer_type, ignore.case = TRUE)], ignore.case = TRUE)))
   d$N_fertilizer[grepl("23", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("23", d$fertilizer_type, ignore.case = TRUE)] * 0.23
   d$P_fertilizer[grepl("23", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("23", d$fertilizer_type, ignore.case = TRUE)] * 0.21
   d$S_fertilizer[grepl("23", d$fertilizer_type, ignore.case = TRUE)] <- d$fertilizer_amount[grepl("23", d$fertilizer_type, ignore.case = TRUE)] * 0.04
   d$fertilizer_type[grepl("23", d$fertilizer_type, ignore.case = TRUE)] <- "NPK"
   d$inoculated <- FALSE
   d$inoculated[grepl("ocula", d$fertilizer_type, ignore.case = TRUE)] <- TRUE # "ocula" for all instances of inoculate...
   d$fertilizer_type[grepl("ocula", d$fertilizer_type, ignore.case = TRUE)] <- NA
   d$fertilizer_type <- gsub("None; ", "", gsub("; None", "", gsub("Super D", "S-compound", gsub("Can", "CAN", gsub("NPK; NPK", "NPK", gsub("Area", "urea", gsub("urea; urea", "urea", gsub("NKP", "NPK; ", gsub("NPK NPK; ", "NPK; ", gsub("Basal dressing Compound D", "D-compound", gsub("50; ", "", gsub("NA; ", "", gsub("; NA", "", d$fertilizer_type)))))))))))))
   d$fertilizer_type[grepl(" and ", d$fertilizer_type, ignore.case = TRUE)] <- "NPK; urea"
   d$fertilizer_type[grepl(" and ", d$fertilizer_type, ignore.case = TRUE)] <- "NPK; urea"
   d$OM_used <- NA
   d$OM_used[grepl("Chimato", d$fertilizer_type, ignore.case = TRUE)] <- TRUE
   d$OM_type <- NA
   d$OM_type[grepl("Chimato", d$fertilizer_type, ignore.case = TRUE)] <- "chimato" # Apparently a Malawian OM type (https://plantwiseplusknowledgebank.org/doi/full/10.1079/pwkb.20237800252)
   d$fertilizer_type[grepl("Chimato", d$fertilizer_type, ignore.case = TRUE)] <- NA
   d$fertilizer_type <- gsub("D-compound/urea", "D-compound; urea", d$fertilizer_type)
   d$fertilizer_type <- gsub("NA", NA, d$fertilizer_type)
   d$fertilizer_type <- gsub("NPKurea", "NPK; urea", gsub("NPK; ", "NPK", d$fertilizer_type))
   
   

   # carobiner::write_files(meta, d, path=path)
   
}

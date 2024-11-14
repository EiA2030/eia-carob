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
      carob_date="2024-11-14"
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
     longitude = r1$field_gps_longitude,
     latitude = r1$field_gps_latitude,
     plot_geom = r1$plot_geotraced_area,
     plot_area = r1$plot_geotraced_area_m2,
     geo_uncertainty = r1$field_gps_precision,
     elevation = r1$field_gps_altitude,
     geo_from_source = TRUE,
     crop = r1$crop_harvested,
     pest_severity = r1$pest_damage,
     disease_severity = r1$diasease_damage,
     plant_density = r1$`plant_population/ha`,
     dm_yield = r1$`dry grain yield (kg/ha)`,
     landprep_date = r1$date_landprep,
     planting_date = r1$date_planting,
     # seed_density = r1$`seedrate_kg/ha`,
     variety_type = r1$seed_type,
     variety = r1$variety_name,
     weeding_done = as.integer(r1$number_of_weedings) > 0,
     weeding_times = as.integer(r1$number_of_weedings),
     fertilizer_used = as.integer(r1$inorg_input1_kg) > 0 | as.integer(r1$inorg_input2_kg) > 0,
     fertilizer_type = paste0(r1$inorg_input1, " || ", r1$inorg_input2),
     fertilizer_amount = as.integer(r1$inorg_input1_kg) + as.integer(r1$inorg_input2_kg),
     N_fertilizer = NA,
     P_fertilizer = NA,
     K_fertilizer = NA,
     herbicide_used = !is.na(as.integer(r1$date_herbicide)),
     herbicide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r1$date_herbicide)[2]), by = "day")[as.integer(unique(r1$date_herbicide)[2])],
     fungicide_used = !is.na(as.integer(r1$date_fungicide)),
     fungicide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r1$date_fungicide)[2]), by = "day")[as.integer(unique(r1$date_fungicide)[2])],
     insecticide_used = !is.na(as.integer(r1$date_insecticide)),
     insecticide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r1$date_insecticide)[2]), by = "day")[as.integer(unique(r1$date_insecticide)[2])],
     harvest_date = r1$date_harvest,
     labour_manday = r1$amount_permanday,
     labour_ha = r1$amount_peracre * 0.40,
     currency = r1$`labor/currency`
   )
   
   d2 <- data.frame(
     dataset_id = uri,
     hhid = r2$farmer_id,
     # record_id,
     country = r2$country,
     adm1 = r2$province,
     adm2 = r2$district,
     adm3 = r2$epa,
     adm4 = r2$section,
     longitude = r2$field_gps_longitude,
     latitude = r2$field_gps_latitude,
     plot_geom = r2$plot_geotraced_area,
     plot_area = r2$plot_geotraced_area_m2,
     geo_uncertainty = r2$field_gps_precision,
     elevation = r2$field_gps_altitude,
     geo_from_source = TRUE,
     crop = r2$crop_harvested,
     pest_severity = r2$pest_damage,
     disease_severity = r2$diasease_damage,
     plant_density = r2$`plant_population/ha`,
     dm_yield = r2$`dry grain yield (kg/ha)`,
     landprep_date = r2$date_landprep,
     planting_date = r2$date_planting,
     # seed_density = r2$`seedrate_kg/ha`,
     variety_type = r2$seed_type,
     variety = r2$variety_name,
     weeding_done = as.integer(r2$number_of_weedings) > 0,
     weeding_times = as.integer(r2$number_of_weedings),
     fertilizer_used = as.integer(r2$inorg_input1_kg) > 0 | as.integer(r2$inorg_input2_kg) > 0,
     fertilizer_type = paste0(r2$inorg_input1, " || ", r2$inorg_input2),
     fertilizer_amount = as.integer(r2$inorg_input1_kg) + as.integer(r2$inorg_input2_kg),
     N_fertilizer = NA,
     P_fertilizer = NA,
     K_fertilizer = NA,
     herbicide_used = !is.na(as.integer(r2$date_herbicide)),
     herbicide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r2$date_herbicide)[2]), by = "day")[as.integer(unique(r2$date_herbicide)[2])],
     fungicide_used = !is.na(as.integer(r2$date_fungicide)),
     fungicide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r2$date_fungicide)[2]), by = "day")[as.integer(unique(r2$date_fungicide)[2])],
     insecticide_used = !is.na(as.integer(r2$date_insecticide)),
     insecticide_dates = seq.Date(as.Date("1900-01-01"), length.out = as.integer(unique(r2$date_insecticide)[2]), by = "day")[as.integer(unique(r2$date_insecticide)[2])],
     harvest_date = r2$date_harvest,
     labour_manday = r2$amount_permanday,
     labour_ha = r2$amount_peracre * 0.40,
     currency = r2$`labor/currency`
   )
   
   # carobiner::write_files(meta, d, path=path)
   
}

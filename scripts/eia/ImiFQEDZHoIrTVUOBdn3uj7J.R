# R script for EiA version of"carob"

## ISSUES
# 1. DOI missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...

"
  
  uri <- "ImiFQEDZHoIrTVUOBdn3uj7J"
  group <- "eia"
  
  meta <- data.frame(
    uri = uri,
    dataset_id = uri,
    publication= NA,
    authors = "Michael Kermah; Samuel Adjei-Nsiah",
    data_institute ="IITA",
    title = NA,
    group = group,
    license = 'none',
    carob_contributor = "Eduardo Garcia Bendito",
    usecase_code= "USC010",
    usecase_name = "GH-CerLeg-Esoko",
    activity = 'validation',
    treatment_vars= "none",
    response_vars= "none",
    project = 'Excellence in Agronomy',
    data_type = "experiment",
    carob_date="2025-01-16",
    notes= NA
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Ghana-Soybean-Validation/", full.names = T))
  
  # Retrieve relevant file(s)
  f1 <- ff[basename(ff) == "20241216145951_EiA_esoko_validation_fertilizer_raw_2024.xlsx"]
  f2 <- ff[basename(ff) == "20241216145951_EiA_esoko_validation_intercropping_raw_2024.xlsx"]
  
  # Read relevant file(s)
  r1 <- readxl::read_excel(f1)
  
  # Filter for plot description and experiment layout
  r11 <- r1[r1$`group/event` == "event1", ]
  
  # Process data overall
  d <- data.frame(
    country = r11$`group_location/country`[2:length(r11$`group_location/country`)],
    adm1 = r11$`group_location/adm1`[2:length(r11$`group_location/adm1`)],
    adm2 = r11$`group_location/adm2`[2:length(r11$`group_location/adm2`)],
    currency = r11$`group_location/currency`[2:length(r11$`group_location/currency`)],
    longitude = round(as.numeric(r11$`group_location/longitude`[2:length(r11$`group_location/longitude`)]), 3),
    latitude = round(as.numeric(r11$`group_location/latitude`[2:length(r11$`group_location/latitude`)]), 3),
    elevation = round(as.numeric(r11$`group/_geopoint_household_altitude`[2:length(r11$`group/_geopoint_household_altitude`)]), 0),
    geo_uncertainty = round(as.numeric(r11$`group/_geopoint_household_precision`[2:length(r11$`group/_geopoint_household_precision`)]), 1),
    trial_id = ifelse(is.na(r11[[5]][2:length(r11[[5]])]), r11[[6]][2:length(r11[[6]])], r11[[5]][2:length(r11[[5]])])
  )
  
  # Process land preparation and description data
  d2 <- data.frame(
    trial_id = ifelse(is.na(r11[[5]][2:length(r11[[5]])]), r11[[6]][2:length(r11[[6]])], r11[[5]][2:length(r11[[5]])]),
    previous_crop = ifelse(sub(" .*", "", r11$`group_land/previous_crop`[2:length(r11$`group_land/previous_crop`)]) == "other", NA, sub(" .*", "", r11$`group_land/previous_crop`[2:length(r11$`group_land/previous_crop`)])),
    previous_fertilizer = ifelse(r11$`group_land/inorganic_fertilizer_previous_season`[2:length(r11$`group_land/inorganic_fertilizer_previous_season`)] == "no", FALSE, TRUE),
    previous_OM = ifelse(r11$`group_land/organic_fertilizer_previous_season`[2:length(r11$`group_land/organic_fertilizer_previous_season`)] == "no", FALSE, TRUE),
    irrigated = ifelse(r11$`group_land/irrigation_technique`[2:length(r11$`group_land/irrigation_technique`)] == "rainfed", FALSE, TRUE),
    previous_crop_residue_management	= r11$`group_land/residue_management`[2:length(r11$`group_land/residue_management`)],
    previous_crop_residue_perc = r11$`group_land/residue_remaining`[2:length(r11$`group_land/residue_remaining`)],
    land_prep_method = r11$`group_land/land_preparation`[2:length(r11$`group_land/land_preparation`)],
    land_prep_implement = r11$`group_land/tillage_technique_1`[2:length(r11$`group_land/tillage_technique_1`)]
  )
  d2$previous_crop_residue_perc <- ifelse(d2$previous_crop_residue_perc == "0", 0,
                                          ifelse(d2$previous_crop_residue_perc == "1_49", 33.3,
                                                 ifelse(d2$previous_crop_residue_perc == "50_99", 66.6, 100)))
  
  # Process plot management data
  d2p <- data.frame(
    trial_id = ifelse(is.na(r11[[5]][2:length(r11[[5]])]), r11[[6]][2:length(r11[[6]])], r11[[5]][2:length(r11[[5]])]),
    treatment_p1 = r11$`group_layout/group_layoutp1/treatment_p1`[2:length(r11$`group_layout/group_layoutp1/treatment_p1`)],
    plot_length_p1 = as.numeric(r11$`group_layout/group_layoutp1/plot_lenght_1_m_p1`[2:length(r11$`group_layout/group_layoutp1/plot_lenght_1_m_p1`)]),
    plot_width_p1 = as.numeric(r11$`group_layout/group_layoutp1/plot_width_1_m_p1`[2:length(r11$`group_layout/group_layoutp1/plot_width_1_m_p1`)]),
    plot_area_p1 = as.numeric(r11$`group_layout/group_layoutp1/plot_area_p1`[2:length(r11$`group_layout/group_layoutp1/plot_area_p1`)]),
    crop_p1 = r11$`group_layout/group_layoutp1/crop_p1`[2:length(r11$`group_layout/group_layoutp1/crop_p1`)],
    variety_p1 = r11$`group_layout/group_layoutp1/variety_p1`[2:length(r11$`group_layout/group_layoutp1/variety_p1`)],
    planting_date_p1 = as.character(as.Date(as.integer(r11$`group_layout/group_layoutp1/planting_date_p1`[2:length(r11$`group_layout/group_layoutp1/planting_date_p1`)]), origin = "1900-01-01")),
    seed_density_p1 = as.numeric(r11$`group_layout/group_layoutp1/planting_density_p1`[2:length(r11$`group_layout/group_layoutp1/planting_density_p1`)]),
    planting_method_p1 = r11$`group_layout/group_layoutp1/planting_technique_p1`[2:length(r11$`group_layout/group_layoutp1/planting_technique_p1`)],
    row_spacing_p1 = as.integer(r11$`group_layout/group_layoutp1/row_spacing_p1`[2:length(r11$`group_layout/group_layoutp1/row_spacing_p1`)]),
    plant_spacing_p1 = as.integer(r11$`group_layout/group_layoutp1/plant_spacing_p1`[2:length(r11$`group_layout/group_layoutp1/plant_spacing_p1`)]),
    fertilizer_type_p1 = "none",
    fertilizer_date_p1 = NA,
    fertilizer_amount_p1 = 0,
    N_fertilizer_p1 = 0,
    P_fertilizer_p1 = 0,
    K_fertilizer_p1 = 0,
    fertilizer_price_p1 = 0,
    treatment_p2 = r11$`group_layout/group_layoutp2/treatment_p2`[2:length(r11$`group_layout/group_layoutp2/treatment_p2`)],
    plot_length_p2 = as.numeric(r11$`group_layout/group_layoutp2/plot_lenght_1_m_p2`[2:length(r11$`group_layout/group_layoutp2/plot_lenght_1_m_p2`)]),
    plot_width_p2 = as.numeric(r11$`group_layout/group_layoutp2/plot_width_1_m_p2`[2:length(r11$`group_layout/group_layoutp2/plot_width_1_m_p2`)]),
    plot_area_p2 = as.numeric(r11$`group_layout/group_layoutp2/plot_area_p2`[2:length(r11$`group_layout/group_layoutp2/plot_area_p2`)]),
    crop_p2 = r11$`group_layout/group_layoutp2/crop_p2`[2:length(r11$`group_layout/group_layoutp2/crop_p2`)],
    variety_p2 = r11$`group_layout/group_layoutp2/variety_p2`[2:length(r11$`group_layout/group_layoutp2/variety_p2`)],
    planting_date_p2 = as.character(as.Date(as.integer(r11$`group_layout/group_layoutp2/planting_date_p2`[2:length(r11$`group_layout/group_layoutp2/planting_date_p2`)]), origin = "1900-01-01")),
    seed_density_p2 = as.numeric(r11$`group_layout/group_layoutp2/planting_density_p2`[2:length(r11$`group_layout/group_layoutp2/planting_density_p2`)]),
    planting_method_p2 = r11$`group_layout/group_layoutp2/planting_technique_p2`[2:length(r11$`group_layout/group_layoutp2/planting_technique_p2`)],
    row_spacing_p2 = as.integer(r11$`group_layout/group_layoutp2/row_spacing_p2`[2:length(r11$`group_layout/group_layoutp2/row_spacing_p2`)]),
    plant_spacing_p2 = as.integer(r11$`group_layout/group_layoutp2/plant_spacing_p2`[2:length(r11$`group_layout/group_layoutp2/plant_spacing_p2`)]),
    fertilizer_type_p2 = gsub(" ", ";", r11$`group_fertilizer_2/fertilizer_type_p2`[2:length(r11$`group_fertilizer_2/fertilizer_type_p2`)]),
    fertilizer_date_p2 = as.character(as.Date(as.integer(r11$`group_fertilizer_2/fertilizer_date_p2`[2:length(r11$`group_fertilizer_2/fertilizer_date_p2`)]), origin = "1900-01-01")),
    fertilizer_amount_p2 = rowSums(data.frame(lapply(r11[2:nrow(r11), c("group_fertilizer_2/soa_amount_p2", "group_fertilizer_2/tsp_amount_p2",
                                                                        "group_fertilizer_2/yaramila_croplift_bio_amount_p2", "group_fertilizer_2/mop_amount_p2",
                                                                        "group_fertilizer_2/new_yara_legume_amount_p2", "group_fertilizer_2/nodulmax_plus_amount_p2")],
                                                     function(x) as.numeric(as.character(x)))),
                                   na.rm = TRUE),
    N_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_2/soa_amount_p2`[2:length(r11$`group_fertilizer_2/soa_amount_p2`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`[2:length(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_2/new_yara_legume_amount_p2`[2:length(r11$`group_fertilizer_2/new_yara_legume_amount_p2`)]) * 0.04)),
                              na.rm = TRUE),
    P_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_2/tsp_amount_p2`[2:length(r11$`group_fertilizer_2/tsp_amount_p2`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`[2:length(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_2/new_yara_legume_amount_p2`[2:length(r11$`group_fertilizer_2/new_yara_legume_amount_p2`)]) * 0.04)),
                              na.rm = TRUE),
    K_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_2/mop_amount_p2`[2:length(r11$`group_fertilizer_2/mop_amount_p2`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`[2:length(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_2/new_yara_legume_amount_p2`[2:length(r11$`group_fertilizer_2/new_yara_legume_amount_p2`)]) * 0.04)),
                              na.rm = TRUE),
    fertilizer_price_p2 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_2/soa_amount_p2`[2:length(r11$`group_fertilizer_2/soa_amount_p2`)]) * as.integer(r11$`group_price/soa_price`[2:length(r11$`group_price/soa_price`)]),
                                                   as.integer(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`[2:length(r11$`group_fertilizer_2/yaramila_croplift_bio_amount_p2`)]) * as.integer(r11$`group_price/yaramila_croplift_bio_price`[2:length(r11$`group_price/yaramila_croplift_bio_price`)]), # https://www.yara.com/crop-nutrition/our-global-fertilizer-brands/yaramila/
                                                   as.integer(r11$`group_fertilizer_2/mop_amount_p2`[2:length(r11$`group_fertilizer_2/mop_amount_p2`)]) * as.integer(r11$`group_price/mop_price`[2:length(r11$`group_price/mop_price`)]),
                                                   as.integer(r11$`group_fertilizer_2/new_yara_legume_amount_p2`[2:length(r11$`group_fertilizer_2/new_yara_legume_amount_p2`)]) * as.integer(r11$`group_price/new_yara_legume_price`[2:length(r11$`group_price/new_yara_legume_price`)]),
                                                   as.integer(r11$`group_fertilizer_2/nodulmax_plus_amount_p2`[2:length(r11$`group_fertilizer_2/nodulmax_plus_amount_p2`)]) * as.integer(r11$`group_price/nodulmax_plus_price`[2:length(r11$`group_price/nodulmax_plus_price`)]))),
                                  na.rm = TRUE),
    treatment_p3 = r11$`group_layout/group_layoutp3/treatment_p3`[2:length(r11$`group_layout/group_layoutp3/treatment_p3`)],
    plot_length_p3 = as.numeric(r11$`group_layout/group_layoutp3/plot_lenght_1_m_p3`[2:length(r11$`group_layout/group_layoutp3/plot_lenght_1_m_p3`)]),
    plot_width_p3 = as.numeric(r11$`group_layout/group_layoutp3/plot_width_1_m_p3`[2:length(r11$`group_layout/group_layoutp3/plot_width_1_m_p3`)]),
    plot_area_p3 = as.numeric(r11$`group_layout/group_layoutp3/plot_area_p3`[2:length(r11$`group_layout/group_layoutp3/plot_area_p3`)]),
    crop_p3 = r11$`group_layout/group_layoutp3/crop_p3`[2:length(r11$`group_layout/group_layoutp3/crop_p3`)],
    variety_p3 = r11$`group_layout/group_layoutp3/variety_p3`[2:length(r11$`group_layout/group_layoutp3/variety_p3`)],
    planting_date_p3 = as.character(as.Date(as.integer(r11$`group_layout/group_layoutp3/planting_date_p3`[2:length(r11$`group_layout/group_layoutp3/planting_date_p3`)]), origin = "1900-01-01")),
    seed_density_p3 = as.numeric(r11$`group_layout/group_layoutp3/planting_density_p3`[2:length(r11$`group_layout/group_layoutp3/planting_density_p3`)]),
    planting_method_p3 = r11$`group_layout/group_layoutp3/planting_technique_p3`[2:length(r11$`group_layout/group_layoutp3/planting_technique_p3`)],
    row_spacing_p3 = as.integer(r11$`group_layout/group_layoutp3/row_spacing_p3`[2:length(r11$`group_layout/group_layoutp3/row_spacing_p3`)]),
    plant_spacing_p3 = as.integer(r11$`group_layout/group_layoutp3/plant_spacing_p3`[2:length(r11$`group_layout/group_layoutp3/plant_spacing_p3`)]),
    fertilizer_type_p3 = gsub(" ", ";", r11$`group_fertilizer_3/fertilizer_type_p3`[2:length(r11$`group_fertilizer_3/fertilizer_type_p3`)]),
    fertilizer_date_p3 = as.character(as.Date(as.integer(r11$`group_fertilizer_3/fertilizer_date_p3`[2:length(r11$`group_fertilizer_3/fertilizer_date_p3`)]), origin = "1900-01-01")),
    fertilizer_amount_p3 = rowSums(data.frame(lapply(r11[2:nrow(r11), c("group_fertilizer_3/soa_amount_p3", "group_fertilizer_3/tsp_amount_p3",
                                                                        "group_fertilizer_3/yaramila_croplift_bio_amount_p3", "group_fertilizer_3/mop_amount_p3",
                                                                        "group_fertilizer_3/new_yara_legume_amount_p3", "group_fertilizer_3/nodulmax_plus_amount_p3")],
                                                     function(x) as.numeric(as.character(x)))),
                                   na.rm = TRUE),
    N_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_3/soa_amount_p3`[2:length(r11$`group_fertilizer_3/soa_amount_p3`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`[2:length(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_3/new_yara_legume_amount_p3`[2:length(r11$`group_fertilizer_3/new_yara_legume_amount_p3`)]) * 0.04)),
                              na.rm = TRUE),
    P_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_3/tsp_amount_p3`[2:length(r11$`group_fertilizer_3/tsp_amount_p3`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`[2:length(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_3/new_yara_legume_amount_p3`[2:length(r11$`group_fertilizer_3/new_yara_legume_amount_p3`)]) * 0.04)),
                              na.rm = TRUE),
    K_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_3/mop_amount_p3`[2:length(r11$`group_fertilizer_3/mop_amount_p3`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`[2:length(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_3/new_yara_legume_amount_p3`[2:length(r11$`group_fertilizer_3/new_yara_legume_amount_p3`)]) * 0.04)),
                              na.rm = TRUE),
    fertilizer_price_p3 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_3/soa_amount_p3`[2:length(r11$`group_fertilizer_3/soa_amount_p3`)]) * as.integer(r11$`group_price/soa_price`[2:length(r11$`group_price/soa_price`)]),
                                                   as.integer(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`[2:length(r11$`group_fertilizer_3/yaramila_croplift_bio_amount_p3`)]) * as.integer(r11$`group_price/yaramila_croplift_bio_price`[2:length(r11$`group_price/yaramila_croplift_bio_price`)]), # https://www.yara.com/crop-nutrition/our-global-fertilizer-brands/yaramila/
                                                   as.integer(r11$`group_fertilizer_3/mop_amount_p3`[2:length(r11$`group_fertilizer_3/mop_amount_p3`)]) * as.integer(r11$`group_price/mop_price`[2:length(r11$`group_price/mop_price`)]),
                                                   as.integer(r11$`group_fertilizer_3/new_yara_legume_amount_p3`[2:length(r11$`group_fertilizer_3/new_yara_legume_amount_p3`)]) * as.integer(r11$`group_price/new_yara_legume_price`[2:length(r11$`group_price/new_yara_legume_price`)]),
                                                   as.integer(r11$`group_fertilizer_3/nodulmax_plus_amount_p3`[2:length(r11$`group_fertilizer_3/nodulmax_plus_amount_p3`)]) * as.integer(r11$`group_price/nodulmax_plus_price`[2:length(r11$`group_price/nodulmax_plus_price`)]))),
                                  na.rm = TRUE),
    treatment_p4 = r11$`group_layout/group_layoutp4/treatment_p4`[2:length(r11$`group_layout/group_layoutp4/treatment_p4`)],
    plot_length_p4 = as.numeric(r11$`group_layout/group_layoutp4/plot_lenght_1_m_p4`[2:length(r11$`group_layout/group_layoutp4/plot_lenght_1_m_p4`)]),
    plot_width_p4 = as.numeric(r11$`group_layout/group_layoutp4/plot_width_1_m_p4`[2:length(r11$`group_layout/group_layoutp4/plot_width_1_m_p4`)]),
    plot_area_p4 = as.numeric(r11$`group_layout/group_layoutp4/plot_area_p4`[2:length(r11$`group_layout/group_layoutp4/plot_area_p4`)]),
    crop_p4 = r11$`group_layout/group_layoutp4/crop_p4`[2:length(r11$`group_layout/group_layoutp4/crop_p4`)],
    variety_p4 = r11$`group_layout/group_layoutp4/variety_p4`[2:length(r11$`group_layout/group_layoutp4/variety_p4`)],
    planting_date_p4 = as.character(as.Date(as.integer(r11$`group_layout/group_layoutp4/planting_date_p4`[2:length(r11$`group_layout/group_layoutp4/planting_date_p4`)]), origin = "1900-01-01")),
    seed_density_p4 = as.numeric(r11$`group_layout/group_layoutp4/planting_density_p4`[2:length(r11$`group_layout/group_layoutp4/planting_density_p4`)]),
    planting_method_p4 = r11$`group_layout/group_layoutp4/planting_technique_p4`[2:length(r11$`group_layout/group_layoutp4/planting_technique_p4`)],
    row_spacing_p4 = as.integer(r11$`group_layout/group_layoutp4/row_spacing_p4`[2:length(r11$`group_layout/group_layoutp4/row_spacing_p4`)]),
    plant_spacing_p4 = as.integer(r11$`group_layout/group_layoutp4/plant_spacing_p4`[2:length(r11$`group_layout/group_layoutp4/plant_spacing_p4`)]),
    fertilizer_type_p4 = gsub(" ", ";", r11$`group_fertilizer_4/fertilizer_type_p4`[2:length(r11$`group_fertilizer_4/fertilizer_type_p4`)]),
    fertilizer_date_p4 = as.character(as.Date(as.integer(r11$`group_fertilizer_4/fertilizer_date_p4`[2:length(r11$`group_fertilizer_4/fertilizer_date_p4`)]), origin = "1900-01-01")),
    fertilizer_amount_p4 = rowSums(data.frame(lapply(r11[2:nrow(r11), c("group_fertilizer_4/soa_amount_p4", "group_fertilizer_4/tsp_amount_p4",
                                                                        "group_fertilizer_4/yaramila_croplift_bio_amount_p4", "group_fertilizer_4/mop_amount_p4",
                                                                        "group_fertilizer_4/new_yara_legume_amount_p4", "group_fertilizer_4/nodulmax_plus_amount_p4")],
                                                     function(x) as.numeric(as.character(x)))),
                                   na.rm = TRUE),
    N_fertilizer_p4 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_4/soa_amount_p4`[2:length(r11$`group_fertilizer_4/soa_amount_p4`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`[2:length(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_4/new_yara_legume_amount_p4`[2:length(r11$`group_fertilizer_4/new_yara_legume_amount_p4`)]) * 0.04)),
                              na.rm = TRUE),
    P_fertilizer_p4 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_4/tsp_amount_p4`[2:length(r11$`group_fertilizer_4/tsp_amount_p4`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`[2:length(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_4/new_yara_legume_amount_p4`[2:length(r11$`group_fertilizer_4/new_yara_legume_amount_p4`)]) * 0.04)),
                              na.rm = TRUE),
    K_fertilizer_p4 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_4/mop_amount_p4`[2:length(r11$`group_fertilizer_4/mop_amount_p4`)]) * 0.21,
                                               as.integer(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`[2:length(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`)]) * 0.12,
                                               as.integer(r11$`group_fertilizer_4/new_yara_legume_amount_p4`[2:length(r11$`group_fertilizer_4/new_yara_legume_amount_p4`)]) * 0.04)),
                              na.rm = TRUE),
    fertilizer_price_p4 = rowSums(data.frame(cbind(as.integer(r11$`group_fertilizer_4/soa_amount_p4`[2:length(r11$`group_fertilizer_4/soa_amount_p4`)]) * as.integer(r11$`group_price/soa_price`[2:length(r11$`group_price/soa_price`)]),
                                                   as.integer(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`[2:length(r11$`group_fertilizer_4/yaramila_croplift_bio_amount_p4`)]) * as.integer(r11$`group_price/yaramila_croplift_bio_price`[2:length(r11$`group_price/yaramila_croplift_bio_price`)]), # https://www.yara.com/crop-nutrition/our-global-fertilizer-brands/yaramila/
                                                   as.integer(r11$`group_fertilizer_4/mop_amount_p4`[2:length(r11$`group_fertilizer_4/mop_amount_p4`)]) * as.integer(r11$`group_price/mop_price`[2:length(r11$`group_price/mop_price`)]),
                                                   as.integer(r11$`group_fertilizer_4/new_yara_legume_amount_p4`[2:length(r11$`group_fertilizer_4/new_yara_legume_amount_p4`)]) * as.integer(r11$`group_price/new_yara_legume_price`[2:length(r11$`group_price/new_yara_legume_price`)]),
                                                   as.integer(r11$`group_fertilizer_4/nodulmax_plus_amount_p4`[2:length(r11$`group_fertilizer_4/nodulmax_plus_amount_p4`)]) * as.integer(r11$`group_price/nodulmax_plus_price`[2:length(r11$`group_price/nodulmax_plus_price`)]))),
                                  na.rm = TRUE)
  )
  
  d2p <- reshape(d2p, 
                 varying = list(
                   paste0("treatment_p", 1:4), 
                   paste0("plot_length_p", 1:4), 
                   paste0("plot_width_p", 1:4), 
                   paste0("plot_area_p", 1:4), 
                   paste0("crop_p", 1:4), 
                   paste0("variety_p", 1:4), 
                   paste0("planting_date_p", 1:4), 
                   paste0("seed_density_p", 1:4), 
                   paste0("planting_method_p", 1:4), 
                   paste0("row_spacing_p", 1:4), 
                   paste0("plant_spacing_p", 1:4), 
                   paste0("fertilizer_type_p", 1:4), 
                   paste0("fertilizer_date_p", 1:4), 
                   paste0("fertilizer_amount_p", 1:4), 
                   paste0("N_fertilizer_p", 1:4), 
                   paste0("P_fertilizer_p", 1:4), 
                   paste0("K_fertilizer_p", 1:4), 
                   paste0("fertilizer_price_p", 1:4)
                 ), 
                 v.names = c("treatment", "plot_length", "plot_width", "plot_area", 
                             "crop", "variety", "planting_date", "seed_density", 
                             "planting_method", "row_spacing", "plant_spacing", 
                             "fertilizer_type", "fertilizer_date", "fertilizer_amount", 
                             "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_price"), 
                 timevar = "plot_name", 
                 times = c("p1", "p2", "p3", "p4"), 
                 direction = "long")
  d2p$seed_density <- ifelse(d2p$seed_density > 100, d2p$seed_density/10,
                             ifelse(d2p$seed_density <= 5, NA, NA)) * 10000
  row.names(d2p) <- 1:nrow(d2p)
  d2p$id <- NULL
  
  # Filter for weeding (events 2 and 5)
  r12 <- r1[r1$`group/event` %in% c("event2", "event5"), ]
  
  # Filter for harvest (event 9)
  r13 <- r1[r1$`group/event` %in% c("event9"), ]
  
  d3p <- data.frame(
    trial_id = ifelse(is.na(r13[[5]][2:length(r13[[5]])]), r13[[6]][2:length(r13[[6]])], r13[[5]][2:length(r13[[5]])]),
    harvest_date_p1 = as.character(as.Date(as.integer(r13$`group_soybean/harvest_date`[2:length(r13$`group_soybean/harvest_date`)]), origin = "1900-01-01")),
    fw_yield_p1 = as.numeric(r13$`group_soybean/grain_fresh_weight_p1`[2:length(r13$`group_soybean/grain_fresh_weight_p1`)]),
    crop_price_p1 = (as.numeric(r13$`group_soybean/grain_fresh_weight_p1`[2:length(r13$`group_soybean/grain_fresh_weight_p1`)]) * as.integer(r13$`group_soybean/soybean_grain_price_100kg`[2:length(r13$`group_soybean/soybean_grain_price_100kg`)])) / 100,
    harvest_date_p2 = as.character(as.Date(as.integer(r13$`group_soybean/harvest_date`[2:length(r13$`group_soybean/harvest_date`)]), origin = "1900-01-01")),
    fw_yield_p2 = as.numeric(r13$`group_soybean/grain_fresh_weight_p2`[2:length(r13$`group_soybean/grain_fresh_weight_p2`)]),
    crop_price_p2 = (as.numeric(r13$`group_soybean/grain_fresh_weight_p2`[2:length(r13$`group_soybean/grain_fresh_weight_p2`)]) * as.integer(r13$`group_soybean/soybean_grain_price_100kg`[2:length(r13$`group_soybean/soybean_grain_price_100kg`)])) / 100,
    harvest_date_p3 = as.character(as.Date(as.integer(r13$`group_soybean/harvest_date`[2:length(r13$`group_soybean/harvest_date`)]), origin = "1900-01-01")),
    fw_yield_p3 = as.numeric(r13$`group_soybean/grain_fresh_weight_p3`[2:length(r13$`group_soybean/grain_fresh_weight_p3`)]),
    crop_price_p3 = (as.numeric(r13$`group_soybean/grain_fresh_weight_p3`[2:length(r13$`group_soybean/grain_fresh_weight_p3`)]) * as.integer(r13$`group_soybean/soybean_grain_price_100kg`[2:length(r13$`group_soybean/soybean_grain_price_100kg`)])) / 100,
    harvest_date_p4 = as.character(as.Date(as.integer(r13$`group_soybean/harvest_date`[2:length(r13$`group_soybean/harvest_date`)]), origin = "1900-01-01")),
    fw_yield_p4 = as.numeric(r13$`group_soybean/grain_fresh_weight_p4`[2:length(r13$`group_soybean/grain_fresh_weight_p4`)]),
    crop_price_p4 = (as.numeric(r13$`group_soybean/grain_fresh_weight_p4`[2:length(r13$`group_soybean/grain_fresh_weight_p4`)]) * as.integer(r13$`group_soybean/soybean_grain_price_100kg`[2:length(r13$`group_soybean/soybean_grain_price_100kg`)])) / 100
  )
  
  d3p <- reshape(d3p, 
                 varying = list(
                   paste0("harvest_date_p", 1:4), 
                   paste0("fw_yield_p", 1:4), 
                   paste0("crop_price_p", 1:4)
                 ), 
                 v.names = c("harvest_date", "fw_yield", "crop_price"), 
                 timevar = "plot_name", 
                 times = c("p1", "p2", "p3", "p4"), 
                 direction = "long")
  row.names(d3p) <- 1:nrow(d3p)
  
  # Merge d2 (management) & d4 (yield)
  d1p <- merge(d2p, d3p, by = intersect(names(d2p), names(d3p)))
  d1p$id <- NULL
  
  # Calculate yield (kg/ha)
  d1p$fw_yield <- (d1p$fw_yield / d1p$plot_area) * 10000 # kg/m2 -> kg/ha
  
  # Remove some duplicates
  dd <- data.frame()
  for (trid in unique(d1p$trial_id)) {
    subs <- d1p[d1p$trial_id == trid,]
    if (nrow(subs) <= 4){
      rec <- subs
    } else {
      for (treat in unique(subs$treatment)){
        subs <- d1p[d1p$trial_id == trid & d1p$treatment == treat,]
        if (treat == "site_specific_ino"){
          rec <- subs[grepl("nodulmax_plus", subs$fertilizer_type),]
          dd <- rbind(dd, rec)
        } else if(treat == "control") {
          rec <- subs[which(!is.na(subs$seed_density)) ,]
          rec <- rec[which(as.Date(rec$planting_date) == max(as.Date(rec$planting_date))) ,]
          dd <- rbind(dd, rec)
        } else if(treat == "governement") {
          rec <- subs[which(!is.na(subs$seed_density)) ,]
          rec <- rec[which(as.Date(rec$planting_date) == max(as.Date(rec$planting_date))) ,]
          dd <- rbind(dd, rec)
        } else if(treat == "site_specific") {
          rec <- subs[which(!is.na(subs$seed_density)) ,]
          rec <- rec[which(as.Date(rec$planting_date) == max(as.Date(rec$planting_date))) ,]
          dd <- rbind(dd, rec)
        }
      }
    }
    dd <- rbind(dd, rec)
  }
  dd <- dd[!duplicated(dd),]
  
  # Filter d to single trial_id records, with lowest geo_uncertinty
  d <- do.call(rbind, lapply(split(d, d$trial_id), function(d) {d[d$geo_uncertainty == min(d$geo_uncertainty), , drop = FALSE]}))
  row.names(d) <- 1:nrow(d)
  
  d <- merge(d, dd, "trial_id")
  
  # Remove unused columns
  d <- d[,colnames(d)[!(colnames(d) %in% c("plot_name"))]]
  
  carobiner::write_files(meta, d, path=path)
  
}

# R script for EiA version of"carob"

## ISSUES
# 1. DOI missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "zdaA2DuaaJI5HOVtSrlLmOCj"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication = NA,
      authors = "Kalpana Sharma; Elly Otieno; Kristin Peterson",
      data_institute = "CIP",
      title = NA,
      group = group,
      license = 'none',
      carob_contributor = "Eduardo Garcia Bendito",
      usecase_code = "USC013",
      usecase_name = 'NG-Akilimo-MC Sprout',
      activity = 'validation',
      treatment_vars = "none",
      response_vars = "none",
      project = 'Excellence in Agronomy',
      data_type = "experiment",
      carob_date = "2025-01-13",
      notes = NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-Sprout-Validation/", full.names = T))
   
   # Retrieve relevant file(s)
   f1 <- ff[basename(ff) == "20241216131010_EiA_MercyCorpsSprout_Validation_raw_2024.xlsx"]
   
   # Read relevant file(s) and filter events
   r0 <- readxl::read_excel(f1)
   r1 <- r0[r0$`intro/event` == "event1", ]

   # Process data overall
   d1 <- data.frame(
     country = r1$`location/country`[2:length(r1$`location/country`)],
     longitude = round(as.numeric(r1$`location/longitude`[2:length(r1$`location/longitude`)]), 3),
     latitude = round(as.numeric(r1$`location/latitude`[2:length(r1$`location/latitude`)]), 3),
     elevation = round(as.numeric(r1$`location/_geopoint_household_altitude`[2:length(r1$`location/_geopoint_household_altitude`)]), 0),
     geo_uncertainty = round(as.numeric(r1$`location/_geopoint_household_precision`[2:length(r1$`location/_geopoint_household_precision`)]), 1),
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])])
   )
   
   # Process land preparation and description data
   d2 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     previous_crop = ifelse(sub(" .*", "", r1$`land_preparation/previous_crop`[2:length(r1$`land_preparation/previous_crop`)]) == "other", NA, sub(" .*", "", r1$`land_preparation/previous_crop`[2:length(r1$`land_preparation/previous_crop`)])),
     irrigated = ifelse(r1$`land_preparation/irrigation_technique`[2:length(r1$`land_preparation/irrigation_technique`)] == "rainfed", FALSE, TRUE),
     previous_crop_residue_management	= r1$`land_preparation/vegetation_clearing`[2:length(r1$`land_preparation/vegetation_clearing`)],
     land_prep_method = r1$`land_preparation/land_preparation`[2:length(r1$`land_preparation/land_preparation`)],
     land_prep_implement = r1$`land_preparation/tillage_technique_1`[2:length(r1$`land_preparation/tillage_technique_1`)],
     crop = r1$`planting/planting_1/crop_cultivated`[2:length(r1$`planting/planting_1/crop_cultivated`)],
     variety = r1$`planting/planting_1/variety`[2:length(r1$`planting/planting_1/variety`)],
     planting_date = as.character(as.Date(as.integer(r1$`planting/planting_1/planting_date`[2:length(r1$`planting/planting_1/planting_date`)]), origin = "1900-01-01")),
     seed_density	= r1$`planting/planting_1/planting_density`[2:length(r1$`planting/planting_1/planting_density`)],
     row_spacing = r1$`planting/planting_1/row_spacing`[2:length(r1$`planting/planting_1/row_spacing`)],
     plant_spacing = r1$`planting/planting_1/plant_spacing`[2:length(r1$`planting/planting_1/plant_spacing`)],
     fertilizer_type = gsub(" ", ";", r1$`fertilizer_1/fertilizer_name_1`[2:length(r1$`fertilizer_1/fertilizer_name_1`)])
   )
   
   # Process plot management data
   d2p1 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Blue 1 Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_con/plot_lenght_2_m_con`[2:length(r1$`layout/plot_layout_con/plot_lenght_2_m_con`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_con/plot_width_2_m_con`[2:length(r1$`layout/plot_layout_con/plot_width_2_m_con`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_con/plot_area_con`[2:length(r1$`layout/plot_layout_con/plot_area_con`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p1`[2:length(r1$`fertilizer_1/urea_amount_p1`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p1`[2:length(r1$`fertilizer_1/tsp_amount_p1`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p1`[2:length(r1$`fertilizer_1/mop_amount_p1`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p1`[2:length(r1$`fertilizer_1/npk_amount_p1`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p1`[2:length(r1$`fertilizer_1/urea_amount_p1`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p1`[2:length(r1$`fertilizer_1/npk_amount_p1`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p1`[2:length(r1$`fertilizer_1/tsp_amount_p1`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p1`[2:length(r1$`fertilizer_1/mop_amount_p1`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p1`[2:length(r1$`fertilizer_1/urea_amount_p1`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p1`[2:length(r1$`fertilizer_1/tsp_amount_p1`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p1`[2:length(r1$`fertilizer_1/mop_amount_p1`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p1`[2:length(r1$`fertilizer_1/npk_amount_p1`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   d2p2 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Yellow Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_plot2/plot_lenght_2_m_p2`[2:length(r1$`layout/plot_layout_plot2/plot_lenght_2_m_p2`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_plot2/plot_width_2_m_p2`[2:length(r1$`layout/plot_layout_plot2/plot_width_2_m_p2`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_plot2/plot_area_p2`[2:length(r1$`layout/plot_layout_plot2/plot_area_p2`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p2`[2:length(r1$`fertilizer_1/urea_amount_p2`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p2`[2:length(r1$`fertilizer_1/tsp_amount_p2`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p2`[2:length(r1$`fertilizer_1/mop_amount_p2`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p2`[2:length(r1$`fertilizer_1/npk_amount_p2`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p2`[2:length(r1$`fertilizer_1/urea_amount_p2`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p2`[2:length(r1$`fertilizer_1/npk_amount_p2`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p2`[2:length(r1$`fertilizer_1/tsp_amount_p2`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p2`[2:length(r1$`fertilizer_1/mop_amount_p2`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p2`[2:length(r1$`fertilizer_1/urea_amount_p2`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p2`[2:length(r1$`fertilizer_1/tsp_amount_p2`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p2`[2:length(r1$`fertilizer_1/mop_amount_p2`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p2`[2:length(r1$`fertilizer_1/npk_amount_p2`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   d2p3 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Red Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_plot3/plot_lenght_2_m_p3`[2:length(r1$`layout/plot_layout_plot3/plot_lenght_2_m_p3`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_plot3/plot_width_2_m_p3`[2:length(r1$`layout/plot_layout_plot3/plot_width_2_m_p3`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_plot3/plot_area_p3`[2:length(r1$`layout/plot_layout_plot3/plot_area_p3`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p3`[2:length(r1$`fertilizer_1/urea_amount_p3`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p3`[2:length(r1$`fertilizer_1/tsp_amount_p3`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p3`[2:length(r1$`fertilizer_1/mop_amount_p3`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p3`[2:length(r1$`fertilizer_1/npk_amount_p3`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p3`[2:length(r1$`fertilizer_1/urea_amount_p3`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p3`[2:length(r1$`fertilizer_1/npk_amount_p3`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p3`[2:length(r1$`fertilizer_1/tsp_amount_p3`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p3`[2:length(r1$`fertilizer_1/mop_amount_p3`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p3`[2:length(r1$`fertilizer_1/urea_amount_p3`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p3`[2:length(r1$`fertilizer_1/tsp_amount_p3`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p3`[2:length(r1$`fertilizer_1/mop_amount_p3`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p3`[2:length(r1$`fertilizer_1/npk_amount_p3`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   d2p4 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Purple Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_plot4/plot_lenght_2_m_p4`[2:length(r1$`layout/plot_layout_plot4/plot_lenght_2_m_p4`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_plot4/plot_width_2_m_p4`[2:length(r1$`layout/plot_layout_plot4/plot_width_2_m_p4`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_plot4/plot_area_p4`[2:length(r1$`layout/plot_layout_plot4/plot_area_p4`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p4`[2:length(r1$`fertilizer_1/urea_amount_p4`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p4`[2:length(r1$`fertilizer_1/tsp_amount_p4`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p4`[2:length(r1$`fertilizer_1/mop_amount_p4`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p4`[2:length(r1$`fertilizer_1/npk_amount_p4`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p4`[2:length(r1$`fertilizer_1/urea_amount_p4`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p4`[2:length(r1$`fertilizer_1/npk_amount_p4`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p4`[2:length(r1$`fertilizer_1/tsp_amount_p4`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p4`[2:length(r1$`fertilizer_1/mop_amount_p4`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p4`[2:length(r1$`fertilizer_1/urea_amount_p4`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p4`[2:length(r1$`fertilizer_1/tsp_amount_p4`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p4`[2:length(r1$`fertilizer_1/mop_amount_p4`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p4`[2:length(r1$`fertilizer_1/npk_amount_p4`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   d2p5 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Blue 2 Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_plot5/plot_lenght_2_m_p5`[2:length(r1$`layout/plot_layout_plot5/plot_lenght_2_m_p5`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_plot5/plot_width_2_m_p5`[2:length(r1$`layout/plot_layout_plot5/plot_width_2_m_p5`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_plot5/plot_area_p5`[2:length(r1$`layout/plot_layout_plot5/plot_area_p5`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p5`[2:length(r1$`fertilizer_1/urea_amount_p5`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p5`[2:length(r1$`fertilizer_1/tsp_amount_p5`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p5`[2:length(r1$`fertilizer_1/mop_amount_p5`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p5`[2:length(r1$`fertilizer_1/npk_amount_p5`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p5`[2:length(r1$`fertilizer_1/urea_amount_p5`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p5`[2:length(r1$`fertilizer_1/npk_amount_p5`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p5`[2:length(r1$`fertilizer_1/tsp_amount_p5`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p5`[2:length(r1$`fertilizer_1/mop_amount_p5`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p5`[2:length(r1$`fertilizer_1/urea_amount_p5`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p5`[2:length(r1$`fertilizer_1/tsp_amount_p5`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p5`[2:length(r1$`fertilizer_1/mop_amount_p5`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p5`[2:length(r1$`fertilizer_1/npk_amount_p5`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   d2p6 <- data.frame(
     trial_id = ifelse(is.na(r1[[5]][2:length(r1[[5]])]), r1[[6]][2:length(r1[[6]])], r1[[5]][2:length(r1[[5]])]),
     treatment = "Green Plot",
     plot_length = as.numeric(r1$`layout/plot_layout_plot6/plot_lenght_2_m_p6`[2:length(r1$`layout/plot_layout_plot6/plot_lenght_2_m_p6`)]),
     plot_width = as.numeric(r1$`layout/plot_layout_plot6/plot_width_2_m_p6`[2:length(r1$`layout/plot_layout_plot6/plot_width_2_m_p6`)]),
     plot_area = as.numeric(r1$`layout/plot_layout_plot6/plot_area_p6`[2:length(r1$`layout/plot_layout_plot6/plot_area_p6`)]),
     fertilizer_date = NA,
     fertilizer_amount = as.integer(r1$`fertilizer_1/urea_amount_p6`[2:length(r1$`fertilizer_1/urea_amount_p6`)]) +
       as.integer(r1$`fertilizer_1/tsp_amount_p6`[2:length(r1$`fertilizer_1/tsp_amount_p6`)]) +
       as.integer(r1$`fertilizer_1/mop_amount_p6`[2:length(r1$`fertilizer_1/mop_amount_p6`)]) +
       as.integer(r1$`fertilizer_1/npk_amount_p6`[2:length(r1$`fertilizer_1/npk_amount_p6`)]),
     N_fertilizer = as.integer(r1$`fertilizer_1/urea_amount_p6`[2:length(r1$`fertilizer_1/urea_amount_p6`)]) * 0.46 + as.integer(r1$`fertilizer_1/npk_amount_p6`[2:length(r1$`fertilizer_1/npk_amount_p6`)]) * 0.15,
     P_fertilizer = as.integer(r1$`fertilizer_1/tsp_amount_p6`[2:length(r1$`fertilizer_1/tsp_amount_p6`)]) * 0.19,
     K_fertilizer = as.integer(r1$`fertilizer_1/mop_amount_p6`[2:length(r1$`fertilizer_1/mop_amount_p6`)]) * 0.5,
     fertilizer_price = (as.integer(r1$`fertilizer_1/urea_amount_p6`[2:length(r1$`fertilizer_1/urea_amount_p6`)]) / 50) * as.integer(r1$`fertilizer_price/urea_price`[2:length(r1$`fertilizer_price/urea_price`)]) +
       (as.integer(r1$`fertilizer_1/tsp_amount_p6`[2:length(r1$`fertilizer_1/tsp_amount_p6`)]) / 50) * as.integer(r1$`fertilizer_price/tsp_price`[2:length(r1$`fertilizer_price/tsp_price`)]) +
       (as.integer(r1$`fertilizer_1/mop_amount_p6`[2:length(r1$`fertilizer_1/mop_amount_p6`)]) / 50) * as.integer(r1$`fertilizer_price/mop_price`[2:length(r1$`fertilizer_price/mop_price`)]) +
       (as.integer(r1$`fertilizer_1/npk_amount_p6`[2:length(r1$`fertilizer_1/npk_amount_p6`)]) / 50) * as.integer(r1$`fertilizer_price/npk_price`[2:length(r1$`fertilizer_price/npk_price`)])
   )
   
   # Process weeding data
   r2 <- r0[r0$`intro/event` %in% c("event2", "event4", "event5", "event6", "event7"), ]
   
   dw <- data.frame(
     trial_id = ifelse(is.na(r2$`intro/household_id`[2:length(r2$`intro/household_id`)]), r2$`intro/household_id`[2:length(r2$`intro/household_id`)], r2$`intro/household_id`[2:length(r2$`intro/household_id`)]),
     weed_species = as.character(r2$`weeding/weed_type`[2:length(r2$`weeding/weed_type`)]),
     weeding_done = ifelse(as.integer(r2$`weeding/weeding_number`[2:length(r2$`weeding/weeding_number`)]) > 0, TRUE, FALSE),
     weeding_times = as.integer(r2$`weeding/weeding_number`[2:length(r2$`weeding/weeding_number`)]),
     weeding_implement = ifelse(as.character(r2$`weeding/weeding_technique_1`[2:length(r2$`weeding/weeding_technique_1`)]) == "manual_weeding", "manual"),
     weeding_dates = gsub(" " , ";", trimws(gsub("NA", "",
                                                 paste(as.character(as.Date(as.integer(r2[[384]][2:length(r2[[384]])]), origin = "1900-01-01")),
                                                       as.character(as.Date(as.integer(r2[[386]][2:length(r2[[386]])]), origin = "1900-01-01")),
                                                       as.character(as.Date(as.integer(r2[[388]][2:length(r2[[388]])]), origin = "1900-01-01"))))))
   )
   
   # Process harvest data
   r3 <- r0[r0$`intro/event` == "event7", ]
   
   d3p1 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p1`[2:length(r3$`harvest_section/fresh_weight_p1`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   d3p2 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p2`[2:length(r3$`harvest_section/fresh_weight_p2`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   d3p3 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p3`[2:length(r3$`harvest_section/fresh_weight_p3`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   d3p4 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p4`[2:length(r3$`harvest_section/fresh_weight_p4`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   d3p5 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p5`[2:length(r3$`harvest_section/fresh_weight_p5`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   d3p6 <- data.frame(
     trial_id = ifelse(is.na(r3[[5]][2:length(r3[[5]])]), r3[[6]][2:length(r3[[6]])], r3[[5]][2:length(r3[[5]])]),
     harvest_date = as.character(as.Date(as.integer(r3$`harvest_section/harvest_date`[2:length(r3$`harvest_section/harvest_date`)]), origin = "1900-01-01")),
     fw_yield = as.numeric(r3$`harvest_section/fresh_weight_p6`[2:length(r3$`harvest_section/fresh_weight_p6`)]),
     crop_price = as.integer(r3$`harvest_section/product_price`[2:length(r3$`harvest_section/product_price`)]) / 1000
   )
   
   # Merge d2 (management) & d3 (yield)
   dp1 <- merge(d2, merge(d2p1, d3p1, "trial_id"), "trial_id")
   dp2 <- merge(d2, merge(d2p2, d3p2, "trial_id"), "trial_id")
   dp3 <- merge(d2, merge(d2p3, d3p3, "trial_id"), "trial_id")
   dp4 <- merge(d2, merge(d2p4, d3p4, "trial_id"), "trial_id")
   dp5 <- merge(d2, merge(d2p5, d3p5, "trial_id"), "trial_id")
   dp6 <- merge(d2, merge(d2p6, d3p6, "trial_id"), "trial_id")
   
   dp <- carobiner::bindr(dp1, dp2, dp3, dp4, dp5, dp6)
   
   # Join with the general info
   d <- merge(merge(d1, dp, "trial_id"), dw, "trial_id")
   
   # Remove duplicates
   d <- d[!duplicated(d), ]
   
   # Fix yield
   d$fw_yield <- (d$fw_yield/d$plot_area)*10000 # Convert from kg to kg/ha
   d$fw_yield <- ifelse(d$fw_yield > 30000, 0, d$fw_yield) # Exclude wrong values
   
   # Fix crop price
   d$crop_price <- ifelse(d$crop_price < 1, d$crop_price * 1000,
                          ifelse(d$crop_price < 10, d$crop_price * 100,
                                 ifelse(d$crop_price < 100, d$crop_price * 10, d$crop_price)))
   
   # Fix fertilizer price
   d$fertilizer_price <- ifelse(d$fertilizer_amount == 0, 0, d$fertilizer_price / d$fertilizer_amount)
   
   d$currency <- "NGN"

   carobiner::write_files(meta, d, path=path)
   
}

# R script for EiA version of"carob"

## ISSUES
# ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...
  "
   
   uri <- "nQP55u8hzVTUSZPgrY9ZGwKd"
   group <- "eia"
   
   meta <- data.frame(
      # Need to fill-in metadata...
      uri = uri,
      dataset_id = uri,
      authors = "Isaiah Nyagumbo; Patricia Masikati; Mazvita Chiduwa; John Omondi",
      data_institute = "CIMMYT; IITA; ICRAF-CIFOR",
      title = NA,
      description ="Soilidaridad Soybean Use Case Validations for soybean and maize in 2023-2024",
      license = NA,
      group = group,
      publication=NA,
      usecase_code = "USC016",
      usecase_name = "CH-CerLeg-Solidaridad",
      activity = 'validation',
      carob_contributor = 'Eduardo Garcia Bendito',
      project = 'Excellence in Agronomy',
      data_type = "on-farm experiment",
      carob_date="2025-01-09",
      treatment_vars = "variety;seed_density;N_fertilizer;P_fertilizer;K_fertilizer",
      response_vars= "fwy_residue;fw_yield;fwy_total",
      notes = NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Chinyanja-Solidaridad-Soy-Validation/", full.names = T))
   
   # Retrieve relevant file(s)
   f1 <- ff[basename(ff) == "20241216152659_EiA_Solidaridad_Validation_raw_2023.xlsx"]
   f2 <- ff[basename(ff) == "20241216152659_EiA_Solidaridad_Validation_raw_2024.xlsx"]
   
   # Read relevant file(s)
   r1 <- carobiner::read.excel(f1, fix_names = TRUE)[-c(1),]
   r2 <- carobiner::read.excel(f2, fix_names = TRUE)[-c(1),]
   
   d1 <- data.frame(
      country = ifelse(grepl("MW", r1$intro.enumerator_id_1, fixed = T), "Malawi",
                       ifelse(grepl("MZ", r1$intro.enumerator_id_1, fixed = T), "Mozambique",
                              ifelse(grepl("ZM", r1$intro.enumerator_id_1, fixed = T), "Zambia", r1$intro.enumerator_id_1))),
      longitude = round(as.numeric(r1$location.longitude), 3),
      latitude = round(as.numeric(r1$location.latitude), 3),
      elevation = round(as.numeric(r1$location._geopoint_household_altitude), 0),
      geo_uncertainty = round(as.numeric(r1$location._geopoint_household_precision), 1),
      geo_from_source = TRUE,
      trial_id = ifelse(is.na(r1$intro.barcodehousehold), r1$intro.barcodehousehold_1, r1$intro.barcodehousehold),
      currency= r1$location.currency
   )
   
   # Filter to single trial_id records, with lowest geo_uncertinty
   ### CN: What if we have the same trial_id but with different longitude and latitude?
   ## For example, the trial_id SDHHMW000033 has two different longitude and latitude entries. We chose the one with the minimum geo_uncertainty (min(d1$geo_uncertainty)==3.9). Are we not losing information by ignoring the other coordinate?
   d1 <- do.call(rbind, lapply(split(d1, d1$trial_id), function(d1) {d1[d1$geo_uncertainty == min(d1$geo_uncertainty), , drop = FALSE]}))
   row.names(d1) <- 1:nrow(d1)
   d1 <- d1[!duplicated(d1) & complete.cases(d1),]
   
   # Process land preparation and description data
   d2 <- data.frame(
      trial_id = ifelse(is.na(r1$intro.barcodehousehold), r1$intro.barcodehousehold_1, r1$intro.barcodehousehold),
      time = as.POSIXct(as.numeric(r1$X_submission_time)*(60*60*24), origin="1899-12-30", tz="UTC"),
      previous_crop = ifelse(sub(" .*", "", r1$land_preparation.previous_crop) == "other", NA, sub(" .*", "", r1$land_preparation.previous_crop)),
      previous_fertilizer = ifelse(r1$land_preparation.inorganic_fertilizer_previous_season == "no", FALSE, TRUE),
      previous_OM = ifelse(r1$land_preparation.organic_fertilizer_previous_season == "no", FALSE, TRUE),
      irrigated = ifelse(r1$land_preparation.irrigation_technique == "rainfed", FALSE, TRUE),
      previous_crop_residue_management	= r1$land_preparation.residue_management ,
      previous_crop_residue_perc = r1$land_preparation.residue_remaining,
      land_prep_method = r1$land_preparation.land_preparation,
      land_prep_implement = ifelse(grepl("manual",r1$land_preparation.land_preparation), "manual", "herbicide")
   )
   d2$previous_crop_residue_perc <- ifelse(d2$previous_crop_residue_perc == "0", 0,
                                           ifelse(d2$previous_crop_residue_perc == "1_49", 33.3,
                                                  ifelse(d2$previous_crop_residue_perc == "50_99", 66.6, 100)))
   
   # Keep last submission only
   d2 <- d2[order(d2$trial_id, -as.numeric(d2$time)), ]
   d2 <- d2[!duplicated(d2$trial_id), ]
   d2$time <- NULL
   
   # Process plot management data
   d2p <- data.frame(
      trial_id = ifelse(is.na(r1$intro.barcodehousehold), r1$intro.barcodehousehold_1, r1$intro.barcodehousehold),
      treatment_p1 = "Farmer Practices",
      plot_length_p1 = as.numeric(r1$layout_p1.plot_lenght_m_p1),
      plot_width_p1 = as.numeric(r1$layout_p1.plot_width_m_p1),
      plot_area_p1 = as.numeric(r1$layout_p1.plot_area_p1) ,
      crop_p1 = r1$planting_p1.crop_cultivated_p1,
      variety_p1 = r1$planting_p1.variety_p1 ,
      planting_date_p1 = as.character(as.Date(as.integer(r1$planting_p1.planting_date_p1), origin = "1900-01-01")),
      seed_density_p1 = as.numeric(r1$planting_p1.planting_population_p1) ,
      planting_method_p1 = r1$planting_p1.planting_technique_p1,
      intercrop_p1 = ifelse(sub(" .*", "", r1$planting_p1.intercrop_p1) == "other", NA, sub(" .*", "", r1$planting_p1.intercrop_p1)),
      row_spacing_p1 = as.numeric(r1$planting_p1.row_spacing_p1),
      plant_spacing_p1 = as.numeric(r1$planting_p1.plant_spacing_p1),
      fertilizer_type_p1 = gsub("other", "unknown", gsub(" ", ";", r1$fertilizer_p1.fertilizer_name_p1)),
      fertilizer_date_p1 = as.character(as.Date(as.integer(r1$fertilizer_p1.fertilizer_date_p1), origin = "1900-01-01")),
      fertilizer_amount_p1 = rowSums(data.frame(lapply(r1[1:nrow(r1), c("fertilizer_p1.urea_amount_p1", "fertilizer_p1.tsp_amount_p1",
                                                                        "fertilizer_p1.dap_amount_p1", "fertilizer_p1.mop_amount_p1",
                                                                        "fertilizer_p1.npk_amount_p1", "fertilizer_p1.soymix_amount_p1",
                                                                        "fertilizer_p1.compoundD_amount_p1", "fertilizer_p1.superD_amount_p1")],
                                                       function(x) as.numeric(as.character(x)))),
                                     na.rm = TRUE),
      N_fertilizer_p1 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p1.urea_amount_p1)* 0.46,
                                                 as.integer(r1$fertilizer_p1.dap_amount_p1)* 0.18,
                                                 as.integer(r1$fertilizer_p1.npk_amount_p1)* 0.1,
                                                 as.integer(r1$fertilizer_p1.soymix_amount_p1) * 0.07,
                                                 as.integer(r1$fertilizer_p1.compoundD_amount_p1) * 0.1,
                                                 as.integer(r1$fertilizer_p1.superD_amount_p1)* 0.08)),
                                na.rm = TRUE),
      P_fertilizer_p1 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p1.tsp_amount_p1)* 0.19,
                                                 as.integer(r1$fertilizer_p1.dap_amount_p1)* 0.21,
                                                 (as.integer(r1$fertilizer_p1.npk_amount_p1) * 0.2) * 0.436,
                                                 (as.integer(r1$fertilizer_p1.soymix_amount_p1)* 0.2) * 0.436,
                                                 as.integer(r1$fertilizer_p1.compoundD_amount_p1) * 0.2,
                                                 as.integer(r1$fertilizer_p1.superD_amount_p1) * 0.21)),
                                na.rm = TRUE),
      K_fertilizer_p1 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p1.mop_amount_p1) * 0.498,
                                                 (as.integer(r1$.fertilizer_p1.npk_amount_p1) * 0.1) * 0.8,
                                                 (as.integer(r1$fertilizer_p1.soymix_amount_p1) * 0.13) * 0.8,
                                                 as.integer(r1$fertilizer_p1.compoundD_amount_p1) * 0.1,
                                                 as.integer(r1$fertilizer_p1.superD_amount_p1) * 0.07)),
                                na.rm = TRUE),
      fertilizer_price_p1 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p1.urea_amount_p1) * as.integer(r1$.fertilizer_price.urea_price),
                                                     as.integer(r1$fertilizer_p1.tsp_amount_p1) * as.integer(r1$fertilizer_price.tsp_price),
                                                     as.integer(r1$fertilizer_p1.dap_amount_p1) * as.integer(r1$fertilizer_price.dap_price),
                                                     as.integer(r1$fertilizer_p1.mop_amount_p1) * as.integer(r1$fertilizer_price.mop_price),
                                                     as.integer(r1$fertilizer_p1.npk_amount_p1) * as.integer(r1$fertilizer_price.npk_price),
                                                     as.integer(r1$fertilizer_p1.compoundD_amount_p1) * as.integer(r1$fertilizer_price.compoundD_price),
                                                     as.integer(r1$fertilizer_p1.superD_amount_p1) * as.integer(r1$fertilizer_price.superD_price))),
                                    na.rm = TRUE),
      treatment_p2 = "Standardized Farmer Practices",
      plot_length_p2 = as.numeric(r1$layout_p2.plot_lenght_m_p2),
      plot_width_p2 = as.numeric(r1$layout_p2.plot_width_m_p2),
      plot_area_p2 = as.numeric(r1$layout_p2.plot_area_p2),
      crop_p2 = r1$planting_p2.crop_cultivated_p2,
      variety_p2 = r1$planting_p2.variety_p2 ,
      planting_date_p2 = as.character(as.Date(as.integer(r1$planting_p2.planting_date_p2), origin = "1900-01-01")),
      seed_density_p2 = as.numeric(r1$planting_p2.planting_population_p2),
      planting_method_p2 = r1$planting_p2.planting_technique_p2,
      intercrop_p2 = ifelse(sub(" .*", "", r1$planting_p2.intercrop_p2) == "other", NA, sub(" .*", "", r1$planting_p2.intercrop_p2)),
      row_spacing_p2 = as.integer(r1$planting_p2.row_spacing_p2),
      plant_spacing_p2 = as.integer(r1$planting_p2.plant_spacing_p2),
      
      fertilizer_type_p2 = gsub("other", "unknown", gsub(" ", ";", r1$fertilizer_p2.fertilizer_name_p2)),
      fertilizer_date_p2 = as.character(as.Date(as.integer(r1$fertilizer_p2.fertilizer_date_p2), origin = "1900-01-01")),
      fertilizer_amount_p2 = rowSums(data.frame(lapply(r1[1:nrow(r1), c("fertilizer_p2.urea_amount_p2", "fertilizer_p2.tsp_amount_p2",
                                                                        "fertilizer_p2.dap_amount_p2", "fertilizer_p2.mop_amount_p2",
                                                                        "fertilizer_p2.npk_amount_p2", "fertilizer_p2.soymix_amount_p2",
                                                                        "fertilizer_p2.compoundD_amount_p2", "fertilizer_p2.superD_amount_p2")],
                                                       function(x) as.numeric(as.character(x)))),
                                     na.rm = TRUE),
      N_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p2.urea_amount_p2) * 0.46,
                                                 as.integer(r1$fertilizer_p2.dap_amount_p2) * 0.18,
                                                 as.integer(r1$fertilizer_p2.npk_amount_p2) * 0.1,
                                                 as.integer(r1$fertilizer_p2.soymix_amount_p2) * 0.07,
                                                 as.integer(r1$fertilizer_p2.compoundD_amount_p2) * 0.1,
                                                 as.integer(r1$fertilizer_p2.superD_amount_p2) * 0.08)),
                                na.rm = TRUE),
      P_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p2.tsp_amount_p2) * 0.19,
                                                 as.integer(r1$fertilizer_p2.dap_amount_p2) * 0.21,
                                                 (as.integer(r1$fertilizer_p2.npk_amount_p2) * 0.2) * 0.436,
                                                 (as.integer(r1$fertilizer_p2.soymix_amount_p2) * 0.2) * 0.436,
                                                 as.integer(r1$fertilizer_p2.compoundD_amount_p2) * 0.2,
                                                 as.integer(r1$fertilizer_p2.superD_amount_p2) * 0.21)),
                                na.rm = TRUE),
      K_fertilizer_p2 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p2.mop_amount_p2) * 0.498,
                                                 (as.integer(r1$fertilizer_p2.npk_amount_p2) * 0.1) * 0.8,
                                                 (as.integer(r1$fertilizer_p2.soymix_amount_p2) * 0.13) * 0.8,
                                                 as.integer(r1$fertilizer_p2.compoundD_amount_p2) * 0.1,
                                                 as.integer(r1$fertilizer_p2.superD_amount_p2) * 0.07)),
                                na.rm = TRUE),
      fertilizer_price_p2 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p2.urea_amount_p2) * as.integer(r1$fertilizer_price.urea_price),
                                                     as.integer(r1$fertilizer_p2.tsp_amount_p2) * as.integer(r1$fertilizer_price.tsp_price),
                                                     as.integer(r1$fertilizer_p2.dap_amount_p2) * as.integer(r1$fertilizer_price.dap_price),
                                                     as.integer(r1$fertilizer_p2.mop_amount_p2) * as.integer(r1$fertilizer_price.mop_price),
                                                     as.integer(r1$fertilizer_p2.npk_amount_p2) * as.integer(r1$fertilizer_price.npk_price),
                                                     as.integer(r1$fertilizer_p2.compoundD_amount_p2) * as.integer(r1$fertilizer_price.compoundD_price),
                                                     as.integer(r1$fertilizer_p2.superD_amount_p2) * as.integer(r1$fertilizer_price.superD_price))),
                                    na.rm = TRUE),
      treatment_p3 = "MVP",
      plot_length_p3 = as.numeric(r1$layout_p3.plot_lenght_m_p3),
      plot_width_p3 = as.numeric(r1$layout_p3.plot_width_m_p3),
      plot_area_p3 = as.numeric(r1$layout_p3.plot_area_p3),
      crop_p3 = r1$planting_p3.crop_cultivated_p3,
      variety_p3 = r1$planting_p3.variety_p3,
      planting_date_p3 = as.character(as.Date(as.integer(r1$planting_p3.planting_date_p3), origin = "1900-01-01")),
      seed_density_p3 = as.numeric(r1$planting_p3.planting_population_p3),
      planting_method_p3 = r1$planting_p3.planting_technique_p3,
      intercrop_p3 = ifelse(sub(" .*", "", r1$planting_p3.intercrop_p3) == "other", NA, sub(" .*", "", r1$planting_p3.intercrop_p3)),
      row_spacing_p3 = as.integer(r1$planting_p3.row_spacing_p3),
      plant_spacing_p3 = as.integer(r1$planting_p3.plant_spacing_p3),
      
      fertilizer_type_p3 = gsub("other", "unknown", gsub(" ", ";", r1$fertilizer_p3.fertilizer_name_p3)),
      fertilizer_date_p3 = as.character(as.Date(as.integer(r1$fertilizer_p3.fertilizer_date_p3), origin = "1900-01-01")),
      fertilizer_amount_p3 = rowSums(data.frame(lapply(r1[1:nrow(r1), c("fertilizer_p3.urea_amount_p3", "fertilizer_p3.tsp_amount_p3",
                                                                        "fertilizer_p3.dap_amount_p3", "fertilizer_p3.mop_amount_p3",
                                                                        "fertilizer_p3.npk_amount_p3", "fertilizer_p3.soymix_amount_p3",
                                                                        "fertilizer_p3.compoundD_amount_p3", "fertilizer_p3.superD_amount_p3")],
                                                       function(x) as.numeric(as.character(x)))),
                                     na.rm = TRUE),
      N_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p3.urea_amount_p3) * 0.46,
                                                 as.integer(r1$fertilizer_p3.dap_amount_p3) * 0.18,
                                                 as.integer(r1$fertilizer_p3.npk_amount_p3) * 0.1,
                                                 as.integer(r1$fertilizer_p3.soymix_amount_p3) * 0.07,
                                                 as.integer(r1$fertilizer_p3.compoundD_amount_p3) * 0.1,
                                                 as.integer(r1$fertilizer_p3.superD_amount_p3) * 0.08)),
                                na.rm = TRUE),
      P_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p3.tsp_amount_p3) * 0.19,
                                                 as.integer(r1$fertilizer_p3.dap_amount_p3) * 0.21,
                                                 (as.integer(r1$fertilizer_p3.npk_amount_p3) * 0.2) * 0.436,
                                                 (as.integer(r1$fertilizer_p3.soymix_amount_p3) * 0.2) * 0.436,
                                                 as.integer(r1$fertilizer_p3.compoundD_amount_p3) * 0.2,
                                                 as.integer(r1$fertilizer_p3.superD_amount_p3) * 0.21)),
                                na.rm = TRUE),
      K_fertilizer_p3 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p3.mop_amount_p3) * 0.498,
                                                 (as.integer(r1$fertilizer_p3.npk_amount_p3) * 0.1) * 0.8,
                                                 (as.integer(r1$fertilizer_p3.soymix_amount_p3) * 0.13) * 0.8,
                                                 as.integer(r1$fertilizer_p3.compoundD_amount_p3) * 0.1,
                                                 as.integer(r1$fertilizer_p3.superD_amount_p3) * 0.07)),
                                na.rm = TRUE),
      fertilizer_price_p3 = rowSums(data.frame(cbind(as.integer(r1$fertilizer_p3.urea_amount_p3) * as.integer(r1$fertilizer_price.urea_price),
                                                     as.integer(r1$fertilizer_p3.tsp_amount_p3) * as.integer(r1$fertilizer_price.tsp_price),
                                                     as.integer(r1$fertilizer_p3.dap_amount_p3) * as.integer(r1$fertilizer_price.dap_price),
                                                     as.integer(r1$fertilizer_p3.mop_amount_p3) * as.integer(r1$fertilizer_price.mop_price),
                                                     as.integer(r1$fertilizer_p3.npk_amount_p3) * as.integer(r1$fertilizer_price.npk_price),
                                                     as.integer(r1$fertilizer_p3.compoundD_amount_p3) * as.integer(r1$fertilizer_price.compoundD_price),
                                                     as.integer(r1$fertilizer_p3.superD_amount_p3) * as.integer(r1$fertilizer_price.superD_price))),
                                    na.rm = TRUE),
      time = as.POSIXct(as.numeric(r1$X_submission_time ) * (60*60*24), origin="1899-12-30", tz="UTC")
   )
   
   d2p <- reshape(d2p,
                  varying = list(
                     paste0("treatment_p", 1:3),
                     paste0("plot_length_p", 1:3),
                     paste0("plot_width_p", 1:3),
                     paste0("plot_area_p", 1:3),
                     paste0("crop_p", 1:3),
                     paste0("variety_p", 1:3),
                     paste0("intercrop_p", 1:3),
                     paste0("planting_date_p", 1:3),
                     paste0("seed_density_p", 1:3),
                     paste0("planting_method_p", 1:3),
                     paste0("row_spacing_p", 1:3),
                     paste0("plant_spacing_p", 1:3),
                     paste0("fertilizer_type_p", 1:3),
                     paste0("fertilizer_date_p", 1:3),
                     paste0("fertilizer_amount_p", 1:3),
                     paste0("N_fertilizer_p", 1:3),
                     paste0("P_fertilizer_p", 1:3),
                     paste0("K_fertilizer_p", 1:3),
                     paste0("fertilizer_price_p", 1:3)
                  ),
                  v.names = c("treatment", "plot_length", "plot_width", "plot_area",
                              "crop", "variety", "intercrop", "planting_date", "seed_density",
                              "planting_method", "row_spacing", "plant_spacing",
                              "fertilizer_type", "fertilizer_date", "fertilizer_amount",
                              "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_price"),
                  timevar = "plot_name",
                  times = c("p1", "p2", "p3"),
                  direction = "long")
   
   row.names(d2p) <- 1:nrow(d2p)
   d2p$id <- NULL
   
   # Keep last submission only
   d2p <- d2p[tapply(1:nrow(d2p), d2p[c(1,3)], function(ii) ii[which.max(d2p$time[ii])]),]
   d2p <- d2p[!duplicated(d2p),]
   d2p$time <- NULL
   
   d2p$row_spacing <- ifelse(d2p$row_spacing > 1, d2p$row_spacing/10, d2p$row_spacing)
   d2p$row_spacing <- ifelse(d2p$row_spacing > 1, d2p$row_spacing/10, d2p$row_spacing)
   d2p$fertilizer_price <- d2p$fertilizer_price/d2p$fertilizer_amount
   
   d2 <- merge(d2, d2p, "trial_id")
   
   # Format harvest
   
   d3p <- data.frame(
      trial_id = ifelse(is.na(r2$intro.barcodehousehold), r2$intro.barcodehousehold_1, r2$intro.barcodehousehold),
      treatment = ifelse(r2$event == "event8a", "Farmer Practices",
                         ifelse(r2$event == "event8b", "Standardized Farmer Practices", "MVP")),
      harvest_date_p1A = as.character(as.Date(as.integer(r2$harvest_1a.soy_harvest_date_1A), origin = "1900-01-01")),
      fwy_residue_p1A = as.numeric(r2$harvest_1a.soy_fresh_biomass_g_1A),
      fw_yield_p1A = as.numeric(r2$harvest_1a.soy_fresh_w_pod_g_1A),
      crop_price_p1A = as.numeric(r2$harvest_1a.soy_price),
      harvest_date_p1B = as.character(as.Date(as.integer(r2$harvest_1b.soy_harvest_date_1B), origin = "1900-01-01")),
      fwy_residue_p1B = as.numeric(r2$harvest_1b.soy_fresh_biomass_g_1B),
      fw_yield_p1B = as.numeric(r2$harvest_1b.soy_fresh_w_pod_g_1B),
      crop_price_p1B = as.numeric(r2$harvest_1a.soy_price),
      harvest_date_p1C = as.character(as.Date(as.integer(r2$harvest_1c.soy_harvest_date_1C), origin = "1900-01-01")),
      fwy_residue_p1C = as.numeric(r2$harvest_1c.soy_fresh_biomass_g_1C),
      fw_yield_p1C = as.numeric(r2$harvest_1c.soy_fresh_w_pod_g_1C),
      crop_price_p1C = as.numeric(r2$harvest_1a.soy_price),
      harvest_date_p2 = as.character(as.Date(as.integer(r2$harvest_2.soy_harvest_date_2), origin = "1900-01-01")),
      fwy_residue_p2 = as.numeric(r2$harvest_2.soy_fresh_biomass_g_2),
      fw_yield_p2 = as.numeric(r2$harvest_2.soy_fresh_w_pod_g_2),
      crop_price_p2 = as.numeric(r2$harvest_2.soy_price),
      harvest_date_p3 = as.character(as.Date(as.integer(r2$harvest_3.soy_harvest_date_3), origin = "1900-01-01")),
      fwy_residue_p3 = as.numeric(r2$harvest_3.soy_fresh_biomass_g_3),
      fw_yield_p3 = as.numeric(r2$harvest_3.soy_fresh_w_pod_g_3),
      crop_price_p3 = as.numeric(r2$harvest_3.soy_price),
      pest_species= r2$trial_rating.pest_name,
      disease= r2$trial_rating.disease_name
   )
   
   d3p1 <- reshape(d3p[d3p$treatment %in% c("Farmer Practices"), c("trial_id", "treatment","disease","pest_species", colnames(d3p)[grepl(paste0(c("1A", "1B", "1C"), collapse = "|"), colnames(d3p))])],
                   varying = list(
                      paste0("harvest_date_p", c("1A", "1B", "1C")),
                      paste0("fwy_residue_p", c("1A", "1B", "1C")),
                      paste0("fw_yield_p", c("1A", "1B", "1C")),
                      paste0("crop_price_p", c("1A", "1B", "1C"))
                   ),
                   v.names = c("harvest_date", "fwy_residue", "fw_yield", "crop_price"),
                   timevar = "plot_name",
                   times = c("p1A", "p1B", "p1C"),
                   direction = "long")
   d3p1$id <- NULL
   row.names(d3p1) <- 1:nrow(d3p1)
   d3p11 <- unique(d3p1[,c("trial_id", "treatment")])
   d3p11 <- merge(d3p11, by.x = c("trial_id", "treatment"),
                  aggregate(d3p1$harvest_date, by = list(d3p1$trial_id, d3p1$treatment), FUN = "max"), by.y = c(1,2))
   d3p11 <- merge(d3p11, by.x = c("trial_id", "treatment"),
                  aggregate(d3p1$fwy_residue, by = list(d3p1$trial_id, d3p1$treatment), FUN = "mean"), by.y = c(1,2))
   d3p11 <- merge(d3p11, by.x = c("trial_id", "treatment"),
                  aggregate(d3p1$fw_yield, by = list(d3p1$trial_id, d3p1$treatment), FUN = "mean"), by.y = c(1,2))
   d3p11 <- merge(d3p11, by.x = c("trial_id", "treatment"),
                  aggregate(d3p1$crop_price, by = list(d3p1$trial_id, d3p1$treatment), FUN = "mean"), by.y = c(1,2))
   colnames(d3p11) <- c("trial_id", "treatment", "harvest_date", "fwy_residue", "fw_yield", "crop_price")
   
   d3p2 <- reshape(d3p[d3p$treatment %in% c("Standardized Farmer Practices"), c("trial_id", "treatment","disease","pest_species", colnames(d3p)[grepl(paste0(c("p2"), collapse = "|"), colnames(d3p))])],
                   varying = list(
                      paste0("harvest_date_p", 2),
                      paste0("fwy_residue_p", 2),
                      paste0("fw_yield_p", 2),
                      paste0("crop_price_p", 2)
                   ),
                   v.names = c("harvest_date", "fwy_residue", "fw_yield", "crop_price"),
                   timevar = "plot_name",
                   times = c("p2"),
                   direction = "long")
   d3p2$id <- NULL
   row.names(d3p2) <- 1:nrow(d3p2)
   
   d3p3 <- reshape(d3p[d3p$treatment %in% c("MVP"), c("trial_id", "treatment","disease","pest_species", colnames(d3p)[grepl(paste0(c("p3"), collapse = "|"), colnames(d3p))])],
                   varying = list(
                      paste0("harvest_date_p", 3),
                      paste0("fwy_residue_p", 3),
                      paste0("fw_yield_p", 3),
                      paste0("crop_price_p", 3)
                   ),
                   v.names = c("harvest_date", "fwy_residue", "fw_yield", "crop_price"),
                   timevar = "plot_name",
                   times = c("p3"),
                   direction = "long")
   d3p3$id <- NULL
   row.names(d3p3) <- 1:nrow(d3p3)
   
   # EGB:
   # # For plot 1 there is A, B and C. Are those replicates?
   # # For plots 2 and 3, some trial_ids are duplicated, probably due to errors in the data entry. Perhaps they can be treated as replicates and aggregated (mean date and mean yields)?
   
   ################################################################################
   
   # Start putting things together
   d3p <- carobiner::bindr(d3p11, d3p2, d3p3)
   d3p$plot_name <- NULL
   
   d3p$fwy_total <- ((d3p$fwy_residue/10)*10000) + ((d3p$fw_yield/10)*10000)
   d3p$fwy_residue <- ifelse(d3p$fwy_total > 5000 & d3p$fwy_residue > 10, d3p$fwy_residue/10, d3p$fwy_residue)
   d3p$fw_yield <- ifelse(d3p$fwy_total > 5000 & d3p$fw_yield > 10, d3p$fw_yield/10, d3p$fw_yield)
   d3p$fwy_total <- ((d3p$fwy_residue/10)*10000) + ((d3p$fw_yield/10)*10000)
   d3p$fwy_residue <- ifelse(d3p$fwy_total > 5000 & d3p$fwy_residue > 10, d3p$fwy_residue/10, d3p$fwy_residue)
   d3p$fw_yield <- ifelse(d3p$fwy_total > 5000 & d3p$fw_yield > 10, d3p$fw_yield/10, d3p$fw_yield)
   d3p$fwy_total <- ((d3p$fwy_residue/10)*10000) + ((d3p$fw_yield/10)*10000)
   d3p$fw_yield <- (d3p$fw_yield/10)*10000
   d3p$fwy_residue <- (d3p$fwy_residue/10)*10000
   
   d2 <- merge(d2[,colnames(d2)[!(colnames(d2) %in% c("plot_name"))]], d3p[,colnames(d3p)[!(colnames(d3p) %in% c("plot_name"))]], c("trial_id", "treatment"))
   
   # Merge with overall data
   d <- merge(d1, d2, "trial_id")
   d$fertilizer_type <- ifelse(grepl("compoundD", d$fertilizer_type), "D-compound", d$fertilizer_type)
   d$planting_method <- ifelse(grepl("line_sowing", d$planting_method), "line sowing", d$planting_method)
   
   carobiner::write_files(meta, d, path = path)
   
}
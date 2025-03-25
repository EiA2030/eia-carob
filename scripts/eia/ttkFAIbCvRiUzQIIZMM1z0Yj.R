# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "ttkFAIbCvRiUzQIIZMM1z0Yj"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication= NA,
      authors ="Mary Jane; John Doe",
      data_institute ="CARI",
      title = NA,
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC001",
      usecase_name = 'WA-Rice-ATAFI/MOVE',
      activity = 'addon',
      treatment_vars= "none",
      response_vars= "none",
      project = 'Excellence in Agronomy ',
      data_type = "survey",
      carob_date="2024-10-07",
      notes= "crop yield of rice is too high"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-AddOn/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_ATAFI CARI_Nigeria_Addon_2022_beingcleaned.xlsx"]
   
   ## Process geo data and some crop managment practices 
   r1 <- carobiner::read.excel(f,sheet = "data")[-c(1),]
   names(r1) <- gsub("_index", "index", names(r1))
   d <- data.frame(
      country = r1$country,
      on_farm = FALSE,
      is_survey = TRUE,
      adm1=r1$admin1,
      adm2=r1$admin2,
      location=r1$village,
      # longitude=as.numeric(r1$longitude),
      # latitude=as.numeric(r1$latitude),
      currency=ifelse(grepl("naira", r1$local_currency), "NGN", "ETB") ,
      trial_id =  r1$barcodehousehold,
      #technology_use= r1$use_case_technology, ## Not a carob variable
      #fertilizer_amount= as.numeric(r1$fertiliser_amount),
      # fertilizer_type= r1$fertiliser_type,
      irrigation_dates= r1$Irrigation_months, 
      # irrigation_source= r1$Irrigation_source,
      # irrigated= ifelse(grepl("No", r1$land_irrigated), FALSE,
      #                   ifelse(grepl("Yes", r1$land_irrigated), TRUE, NA)), 
      #land_prep_method=r1$tillage_power,
      index=r1$index,
      fertilizer_constraint= r1$constraint_fertilizers ## Not a carob variable
   )
   
   ## Adding farmer gender
   r2 <- carobiner::read.excel(f,sheet = "hh_members_details_repeat")[-c(1),]
   names(r2) <- gsub("_parent_index", "index", names(r2))
   r2 <- r2[, c("hh_position", "person_gender", "person_age", "index")]
   colnames(r2) <- c("farmer_position", "farmer_gender", "farmer_age", "index")
   r2$farmer_gender <- ifelse(r2$farmer_gender == "F", "female",
                              ifelse(r2$farmer_gender == "M", "male", r2$farmer_gender))
   r2 <- data.frame(
     index = unique(r2$index),
     farmer_gender = tapply(r2$farmer_gender, r2$index, function(x) paste0(x, collapse = ";")),
     farmer_age = tapply(r2$farmer_age, r2$index, function(x) paste0(x, collapse = ";"))
   )
   d <- merge(d, r2, by= "index", all.x= TRUE)
   
   ### Processing plot size data
   r3 <- carobiner::read.excel(f,sheet = "hh_plots_repeat")[-c(1),]
   names(r3) <- gsub("_parent_index", "index", names(r3))
   r3 <- data.frame(
     plot_area= ifelse(grepl("acres", r3$unitland), as.numeric(r3$plot_size)*4047, as.numeric(r3$plot_size)*10000),
     plot_tenure = r3$plot_tenure_status,
     index= r3$index
   )
   r31 <- data.frame(
     index = unique(r3$index),
     farmland = tapply(r3$plot_area, r3$index, function(x) sum(x)),
     cropland = tapply(r3$plot_area, r3$index, function(x) sum(x))
   )
   d <- merge(merge(d, r3, by="index", all.x=TRUE), r31, by = "index", all.x = TRUE)
   
   ### Processing yield data
   r4 <- carobiner::read.excel(f,sheet = "crop_repeat")[-c(1),]
   names(r4) <- gsub("_parent_index", "index", names(r4))
   r4 <- data.frame(
     crop = tolower(r4$crop_name),
     season = r4$season_grown,
     # yield = ifelse(grepl("sacks_100kg", r4$crop_yield_units), as.numeric(r4$crop_yield)*100,
     #                ifelse(grepl("sacks_50kg", r4$crop_yield_units), as.numeric(r4$crop_yield)*50,
     #                       ifelse(grepl("tonnes", r4$crop_yield_units), as.numeric(r4$crop_yield)*1000,
     #                              ifelse(grepl("other", r4$crop_yield_units), as.numeric(r4$crop_yield)*100,as.numeric(r4$crop_yield))))),
     # crop_price = ifelse(grepl("price_per_bag_50kg", r4$crop_sold_price_quantityunits), as.numeric(r4$crop_sold_income)/50,
     #                     ifelse(grepl("100kg|100k", r4$crop_price_quantityunits_other), as.numeric(r4$crop_sold_income)/100,
     #                            ifelse(grepl("200kg", r4$crop_price_quantityunits_other), as.numeric(r4$crop_sold_income)/200, as.numeric(r4$crop_sold_income)))),
     crop_residue_type = r4$crop_residue_use,
     crop_residue_used = ifelse(is.na(r4$crop_residue_use), FALSE, TRUE),
     residue_prevcrop_used = grepl("soil", r4$crop_residue_use),
     previous_crop_burnt = grepl("burn", r4$crop_residue_use),
     previous_crop_residue_management = gsub(" ", ";", r4$crop_residue_use),
     index = r4$index
   )
   
   d <- merge(d, r4, by="index", all.x=TRUE)
   
   ## Adding variety, disease, pest and previous crop
   r5 <- carobiner::read.excel(f,sheet = "plot_information_repeat")[-c(1),]
   names(r5) <- gsub("_parent_index", "index", names(r5))
   names(r5) <- gsub("_index", "plot_index", names(r5))
   r5 <- data.frame(
     longitude = as.numeric(r5$plot_longitude),
     latitude = as.numeric(r5$plot_latitude),
     elevation = as.numeric(r5$plot_altitude_m),
     geo_uncertainty = as.numeric(r5$`_geopoint_plot_precision`),
     geo_from_source = TRUE,
     landscape_position = r5$position,
     previous_crop = tolower(r5$previous_crop),
     labour_gender = gsub("_unpaid", "", gsub("_paid", "", gsub(" ", ";", r5$land_preparation_labor))),
     land_prep_cost_male_labor = as.numeric(r5$land_prep_cost_male_labor),
     planting_cost_male_labor = as.numeric(r5$planting_cost_male_labor),
     irrigation_cost_male_labor = as.numeric(r5$irrigation_cost_male_labor),
     seed_source = r5$source_material,
     seed_density = ifelse(grepl("50", r5$planting_material_unit_name), as.numeric(r5$planting_material_rate) * 50,
                           ifelse(grepl("10", r5$planting_material_unit_name), as.numeric(r5$planting_material_rate) * 10,
                                  ifelse(grepl("5", r5$planting_material_unit_name), as.numeric(r5$planting_material_rate) * 5, as.numeric(r5$planting_material_rate)))), # kg/ha
     variety= r5$variety,
     seed_cost = r5$planting_material_cost,
     row_spacing= as.numeric(r5$distance_lines),
     planting_method = r5$seeding_method,
     planting_date = as.character(as.Date(as.numeric(r5$planting_date), origin= "1899-12-30")),
     transplanting_date = as.character(as.Date(as.numeric(r5$planting_date), origin= "1899-12-30") + as.numeric(r5$seeling_age_days)),
     irrigated = ifelse(r5$irrigation_plot == "Yes", TRUE, FALSE),
     irrigation_number = as.integer(r5$irrigation_number),
     irrigation_amount = as.numeric(r5$irrigation_amount_mm) * as.integer(r5$irrigation_number), # So many questions about this....
     irrigation_source = r5$irrigation_source_plot,
     harvest_date = as.character(as.Date(as.numeric(r5$harvesting_date), origin= "1899-12-30")),
     yield = ifelse(grepl("100", r5$primary_harvest_unit), as.numeric(r5$primary_yield) * 100,
                    ifelse(grepl("50", r5$primary_harvest_unit), as.numeric(r5$primary_yield) * 50,
                           ifelse(grepl("tonne", r5$primary_harvest_unit), as.numeric(r5$primary_yield) * 1000, as.numeric(r5$primary_yield)))), # kg/ha
     yield_part = ifelse(r5$primary_product == "grains", "grain", r5$primary_product),
     crop_price = ifelse(grepl("100", r5$selling_unit), as.numeric(r5$selling_primary_price) / 100,
                         ifelse(grepl("50", r5$selling_unit), as.numeric(r5$selling_primary_price) / 50,
                                ifelse(grepl("tonne", r5$selling_unit), as.numeric(r5$selling_primary_price) / 1000, as.numeric(r5$selling_primary_price)))),
     plot_index = r5$plot_index,
     index = r5$index
   )
   r5$labour_price <- rowSums(r5[, c("land_prep_cost_male_labor", "planting_cost_male_labor", "irrigation_cost_male_labor")], na.rm = TRUE)
   r5$land_prep_cost_male_labor <- r5$planting_cost_male_labor <- r5$irrigation_cost_male_labor <- NULL
   r5$seed_cost <- as.numeric(r5$seed_cost) / as.numeric(r5$seed_density)
   r5$planting_method[grepl("manual", r5$planting_method)] <- "manual"
   r5$planting_method[grepl("_", r5$planting_method)] <- "mechanized"
   r5$crop_price <- ifelse(r5$crop_price > 10000, r5$crop_price/100, r5$crop_price)
   r5$crop_price <- ifelse(r5$crop_price < 1, r5$crop_price * 1000, r5$crop_price)
   r5$crop_price <- ifelse(r5$crop_price < 10, r5$crop_price * 100, r5$crop_price)
   r5$crop_price <- ifelse(r5$crop_price < 100, r5$crop_price * 10, r5$crop_price)
   
   ### Processing tillage data
   r6 <- carobiner::read.excel(f,sheet = "tillage_info")[-c(1),]
   names(r6) <- gsub("_parent_index", "plot_index", names(r6))
   r6 <- data.frame(
     land_prep_implement = r6$tillage_method,
     tillage_labour = as.numeric(r6$tillage_cost_male_labor),
     plot_index = r6$plot_index
   )
   r6$land_prep_implement[grep("4wheel_tractor", r6$land_prep_implement)] <- "4 wheel tractor"
   r6$land_prep_implement[grep("2wheel_tractor", r6$land_prep_implement)] <- "2 wheel tractor"
   r6$land_prep_implement[grep("mechanical", r6$land_prep_implement)] <- "unknown"
   
   r6 <- data.frame(
     plot_index = unique(r6$plot_index),
     land_prep_implement = tapply(r6$land_prep_implement, r6$plot_index, function(x) paste0(x, collapse = ";")),
     tillage_labour = tapply(r6$tillage_labour, r6$plot_index, function(x) sum(x, na.rm = T))
   )
   
   r5 <- merge(r5, r6, by = "plot_index", all.x=TRUE)
   
   r5$labour_price <- rowSums(r5[, c("labour_price", "tillage_labour")], na.rm = TRUE)
   r5$tillage_labour <- NULL
   
   ### Processing inorganic fertilizer data
   r7 <- carobiner::read.excel(f,sheet = "inorganic_inputs_repeat")[-c(1),]
   names(r7) <- gsub("_index", "index", names(r7))
   names(r7) <- gsub("_parent", "plot_", names(r7))
   r7 <- data.frame(
     # fertilizer_year = as.character(format(as.Date(as.numeric(r7$inorganic_date), origin= "1899-12-30"), "%Y")),
     # fertilizer_doy = as.character(format(as.Date(as.numeric(r7$inorganic_date), origin= "1899-12-30"), "%j")),
     # N_splits = as.integer(r7$inorganic_inputs_details_repeat_count),
     fertilizer_labour = as.numeric(r7$inorganic_cost_male_labor),
     plot_index = r7$plot_index,
     index = r7$index
   )

   r8 <- carobiner::read.excel(f,sheet = "inorganic_inputs_details_repeat")[-c(1),]
   names(r8) <- gsub("_parent_index", "index", names(r8))
   r8 <- data.frame(
     fertilizer_type = r8$inorganic_ids,
     fertilizer_amount = ifelse(grepl("100", r8$inorganic_input_unit), as.numeric(r8$inorganic_inputs_amount) * 100,
                                ifelse(grepl("50", r8$inorganic_input_unit), as.numeric(r8$inorganic_inputs_amount) * 50,
                                       as.numeric(r8$inorganic_inputs_amount))),
     fertilizer_price = ifelse(grepl("100", r8$inorganic_input_unit), as.numeric(r8$inorganic_inputs_costs) / 100,
                               ifelse(grepl("50", r8$inorganic_input_unit), as.numeric(r8$inorganic_inputs_costs) / 50,
                                      as.numeric(r8$inorganic_inputs_costs))),
     index = r8$index
   )
   r8$N_fertilizer <- NA
   r8$N_fertilizer[r8$fertilizer_type == "urea"] <- r8$fertilizer_amount[r8$fertilizer_type == "urea"] * 0.46
   r8$N_fertilizer[r8$fertilizer_type == "NPK15"] <- r8$fertilizer_amount[r8$fertilizer_type == "NPK15"] * 0.15
   r8$N_fertilizer[r8$fertilizer_type == "NPK201010"] <- r8$fertilizer_amount[r8$fertilizer_type == "NPK201010"] * 0.2
   r8$N_fertilizer[r8$fertilizer_type == "NPS"] <- r8$fertilizer_amount[r8$fertilizer_type == "NPS"] * 0.23
   r8$N_fertilizer[r8$fertilizer_type == "ammonium_sulphate"] <- r8$fertilizer_amount[r8$fertilizer_type == "ammonium_sulphate"] * 0.21
   r8$P_fertilizer <- NA
   r8$P_fertilizer[r8$fertilizer_type == "NPK15"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPK15"] * 0.15) * 0.43646
   r8$P_fertilizer[r8$fertilizer_type == "NPK201010"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPK201010"] * 0.1) * 0.43646
   r8$P_fertilizer[r8$fertilizer_type == "NPS"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPS"] * 0.21) * 0.43646
   r8$K_fertilizer <- NA
   r8$K_fertilizer[r8$fertilizer_type == "NPK15"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPK15"] * 0.15) / 1.21
   r8$K_fertilizer[r8$fertilizer_type == "NPK201010"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPK201010"] * 0.1) / 1.21
   r8$S_fertilizer <- NA
   r8$S_fertilizer[r8$fertilizer_type == "NPS"] <- (r8$fertilizer_amount[r8$fertilizer_type == "NPS"] * 0.04) / 0.3
   r8$S_fertilizer[r8$fertilizer_type == "ammonium_sulphate"] <- (r8$fertilizer_amount[r8$fertilizer_type == "ammonium_sulphate"] * 0.24) / 0.3
   r8$fertilizer_type[grep("NPK", r8$fertilizer_type)] <- "NPK"
   r8$fertilizer_type[grep("ammonium_sulphate", r8$fertilizer_type)] <- "DAS"
   r8$fertilizer_price <- paste0(r8$fertilizer_type, "=", r8$fertilizer_price)
   
   r8 <- data.frame(
     index = unique(r8$index),
     fertilizer_type = tapply(r8$fertilizer_type, r8$index, function(x) paste0(x, collapse = ";")),
     fertilizer_amount = tapply(r8$fertilizer_amount, r8$index, function(x) sum(x, na.rm = T)),
     fertilizer_price = tapply(r8$fertilizer_price, r8$index, function(x) paste0(x, collapse = ";")),
     N_fertilizer = tapply(r8$N_fertilizer, r8$index, function(x) sum(x, na.rm = T)),
     P_fertilizer = tapply(r8$P_fertilizer, r8$index, function(x) sum(x, na.rm = T)),
     K_fertilizer = tapply(r8$K_fertilizer, r8$index, function(x) sum(x, na.rm = T)),
     S_fertilizer = tapply(r8$S_fertilizer, r8$index, function(x) sum(x, na.rm = T))
   )
   
   r7 <- merge(r7, r8, by = "index", all.x = TRUE)
   
   r7 <- data.frame(
     plot_index = unique(r7$plot_index),
     fertilizer_labour = tapply(r7$fertilizer_labour, r7$plot_index, function(x) sum(x, na.rm = T)),
     fertilizer_type = tapply(r7$fertilizer_type, r7$plot_index, function(x) paste0(x, collapse = ";")),
     fertilizer_amount = tapply(r7$fertilizer_amount, r7$plot_index, function(x) sum(x, na.rm = T)),
     fertilizer_price = tapply(r7$fertilizer_price, r7$plot_index, function(x) paste0(x, collapse = ";")),
     N_fertilizer = tapply(r7$N_fertilizer, r7$plot_index, function(x) sum(x, na.rm = T)),
     P_fertilizer = tapply(r7$P_fertilizer, r7$plot_index, function(x) sum(x, na.rm = T)),
     K_fertilizer = tapply(r7$K_fertilizer, r7$plot_index, function(x) sum(x, na.rm = T)),
     S_fertilizer = tapply(r7$S_fertilizer, r7$plot_index, function(x) sum(x, na.rm = T))
   )
   
   r5 <- merge(r5, r7, by = "plot_index", all.x=TRUE)
   
   r5$labour_price <- rowSums(r5[, c("labour_price", "fertilizer_labour")], na.rm = TRUE)
   r5$fertilizer_labour <- NULL
   
   ### Processing organic fertilizer data
   r8 <- carobiner::read.excel(f,sheet = "organic_inputs_repeat")[-c(1),]
   names(r8) <- gsub("_index", "index", names(r8))
   names(r8) <- gsub("_parent", "plot_", names(r8))
   r8 <- data.frame(
     fertilizer_date = as.character(as.Date(as.numeric(r8$organic_date), origin= "1899-12-30")),
     OM_labour = as.numeric(r8$organic_cost_male_labor),
     plot_index = r8$plot_index,
     index = r8$index
   )
   
   r9 <- carobiner::read.excel(f,sheet = "organic_inputs_details_repeat")[-c(1),]
   names(r9) <- gsub("_parent_index", "index", names(r9))
   r9 <- data.frame(
     OM_type = r9$organic_ids,
     OM_amount = ifelse(grepl("100", r9$organic_input_unit), as.numeric(r9$organic_input_amount) * 100,
                                ifelse(grepl("50", r9$organic_input_unit), as.numeric(r9$organic_input_amount) * 50,
                                       as.numeric(r9$organic_input_amount))),
     OM_source = r9$organic_input_source,
     OM_price = ifelse(grepl("100", r9$organic_input_unit), as.numeric(r9$organic_input_cost) / 100,
                       ifelse(grepl("50", r9$organic_input_unit), as.numeric(r9$organic_input_cost) / 50,
                              as.numeric(r9$organic_input_cost))),
     index = r9$index
   )
   r9$OM_type[grep("plantBiomass", r9$OM_type)] <- "foliage"
   r9$OM_type[grep("compost", r9$OM_type)] <- "compost"
   r9$OM_type[grep("manure", r9$OM_type)] <- "farmyard manure"
   r9$OM_type[grep("other_input", r9$OM_type)] <- "unknown"
   r9$OM_type[!grepl(paste0(c("unknown", "farmyard manure", "compost", "foliage"), collapse = "|"), r9$OM_type)] <- NA
   
   r9 <- data.frame(
     index = unique(r9$index),
     OM_type = tapply(r9$OM_type, r9$index, function(x) paste0(x, collapse = ";")),
     OM_amount = tapply(r9$OM_amount, r9$index, function(x) sum(x, na.rm = T)),
     OM_price = tapply(r9$OM_price, r9$index, function(x) paste0(x, collapse = ";"))
   )
   
   r8 <- merge(r8, r9, by = "index", all.x = TRUE)
   
   r8 <- data.frame(
     plot_index = unique(r8$plot_index),
     OM_labour = tapply(r8$OM_labour, r8$plot_index, function(x) sum(x, na.rm = T)),
     OM_type = tapply(r8$OM_type, r8$plot_index, function(x) paste0(x, collapse = ";")),
     OM_amount = tapply(r8$OM_amount, r8$plot_index, function(x) sum(x, na.rm = T)),
     OM_price = tapply(r8$OM_price, r8$plot_index, function(x) paste0(x, collapse = ";"))
   )
   
   r5 <- merge(r5, r8, by = "plot_index", all.x=TRUE)
   
   r5$labour_price <- rowSums(r5[, c("labour_price", "OM_labour")], na.rm = TRUE)
   r5$OM_labour <- NULL
   
   d <- merge(d, r5, by = "index", all.x = TRUE)
   
   d$index <- d$plot_index <- NULL
   
   d$seed_density <- d$seed_density/d$plot_area # kg/m2

   carobiner::write_files(meta, d, path=path)
}

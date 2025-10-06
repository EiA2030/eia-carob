# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. planting_time, first_rain_date, gemination, pod_density are not yet the carob variable and perhaps should be included later


carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "Hk4EgIldoEBiXc1qTtuCnw9N"
   group <- "eia"
   
   meta <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      uri = uri,
      authors ="Tofa Abdullahi Ibrahim, Helen Peters, Alpha kamara, Christine Kreye",
      publication= NA,
      data_organization = "IITA",
      data_citation = NA,
      title = "Use Case 1.5: SAA - fertilizer investment prioritization for maize, cassava and rice",
      description = "This Use Case will validate a gender-responsive digital agricultural advisory service that will help extension agents to integrate fertilizer recommendations with relevant agronomic advisories while also expanding extension outreach. The Use Case will serve maize, rice, and cassava growers using a digital tool that provides tailored agronomic recommendations and advice on investment prioritization between the three crops. Approximately 2,000 smallholders will be targeted per value chain in the first year of the tool&rsquo;s pilot testing. A further 5,000 &ndash; 10,000 smallholders will be targeted in the second year.",
      group = group,
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC008",
      usecase_name = "NG-Akilimo-SAA",
      activity = 'MELIA',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_type = "survey",
      carob_date = "2025-10-06",
      treatment_vars = "N_fertilizer",
      response_vars = "yield"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-MELIA/", full.names = T))
   
   
   f <- ff[basename(ff) == "EiA_Telephone_survey_2025_01_21_07_37_20_829587.xlsx"]

   ### Read file
   r <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"))
   
   r <- r[r$`Select the Use Case` == "sasakawa_ng",] # Include maize, but there is only yield data...
   
   rice <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73, 124,126,128, 130, 136, 141, 142, 148, 153, 155, 157, 159, 161,162,163,164, 857, 858, 859, 860, 861, 862)],
                                   from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,124,126,128, 130, 136, 141, 142, 148, 153, 155, 157, 159,  161,162,163, 164, 857, 858, 859, 860, 861, 862)],
                                   to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "planting_date_wet", "planting_date_dry","variety_type", "land_prep_method", "herbicide_used", "fertilizer_used", "fertilizer_type","N_splits", "fertilizer_dap1", "fertilizer_dap2", "fertilizer_dap3", "fertilizer_dap4", "NPK151515_50kg_bags", "NPK201010_50kg_bags", "urea_50kg_bags", "fertilizer_other", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))
   
   cassava <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,295,296,300,307,309,310,315,316,383,404,406,407,409,413,857, 858, 859, 860, 861, 862)],
                                      from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,295,296,300,307,309,310,315,316,383,404,406,407,409,413,857, 858, 859, 860, 861, 862)],
                                      to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "intercrops","variety_type","fertilizer_used","fertilizer_type","fertilizer_amount_NPK15","fertilizer_amount_urea","fertilizer_dap_NPK15","fertilizer_dap_urea", "land_prep_method","plant_density","herbicide_used","herbicide_product","weeding_done","planting_date", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))
   
   maize <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,857, 858, 859, 860, 861, 862)],
                                      from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,857, 858, 859, 860, 861, 862)],
                                      to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))

  # Process rice
   rice <- rice[rice$crop == "Rice",]
   rice <- rice[!grepl("NA", rownames(rice)),]
   rice$trial_id <- 1:nrow(rice)
   rice$is_survey <- TRUE
   rice$on_farm <- FALSE
   rice$crop_cut <- FALSE
   rice$geo_from_source <- FALSE
   rice$currency <- "NGN"
   rice$crop <- tolower(rice$crop)
   rice$farmland_owned <- ifelse(rice$farmland_owned_units == "acre", as.numeric(rice$farmland_owned) * 0.4, as.numeric(rice$farmland_owned))
   rice$cropland_used <- ifelse(rice$farmland_owned_units == "acre", as.numeric(rice$cropland_used) * 0.4, as.numeric(rice$cropland_used))
   rice$treatment <- ifelse(!is.na(rice$plot_area), "treatment", "no-treatment")
   rice$planting_date_wet <- ifelse(is.na(rice$planting_date_wet), rice$planting_date_dry, rice$planting_date_wet)
   rice$planting_date <- as.Date(NA)
   for (i in 1:length(rice$planting_date_wet)) {
     if(!is.na(rice$planting_date_wet)[i]) {
       split_values <- strsplit(rice$planting_date_wet[i], "-")[[1]]
       rice$planting_date[i] <- as.Date(mean(c(as.Date(split_values[1], "%b%d"), as.Date(split_values[2], "%b%d"))))
     }
   }
   rice$planting_date <- as.character(rice$planting_date)
   rice$land_prep_method[grep("Irregular_seeding", rice$land_prep_method)] <- "broadcasting"
   rice$land_prep_method[grep("No_bunding_leveling", rice$land_prep_method)] <- "none"
   rice$land_prep_method[grep("Irregular_seeding", rice$land_prep_method)] <- "none"
   rice$land_prep_method[grep("Well_bunding_leveling", rice$land_prep_method)] <- "basins"
   rice$land_prep_method[grep("Random_bunding_leveling", rice$land_prep_method)] <- "basins"
   rice$herbicide_used <- ifelse(rice$herbicide_used == "yes", TRUE, FALSE)
   rice$fertilizer_used <- ifelse(rice$fertilizer_used == "yes", TRUE, FALSE)
   rice$N_fertilizer <- rice$P_fertilizer <- rice$K_fertilizer <- NA
   rice$N_fertilizer <- rowSums(data.frame((rice$NPK151515_50kg_bags * 50) * 0.15,
                                           (rice$NPK201010_50kg_bags * 50) * 0.2,
                                           (rice$urea_50kg_bags * 50) * 0.46), na.rm = T)
   rice$P_fertilizer <- rowSums(data.frame((rice$NPK151515_50kg_bags * 50) * 0.15,
                                           (rice$NPK201010_50kg_bags * 50) * 0.1), na.rm = T)
   rice$K_fertilizer <- rowSums(data.frame((rice$NPK151515_50kg_bags * 50) * 0.15,
                                           (rice$NPK201010_50kg_bags * 50) * 0.1), na.rm = T)
   rice$fertilizer_dap1[is.na(rice$fertilizer_dap1) | rice$fertilizer_dap1 == "other"] <- ""
   rice$fertilizer_dap2[is.na(rice$fertilizer_dap2) | rice$fertilizer_dap2 == "other"] <- ""
   rice$fertilizer_dap3[is.na(rice$fertilizer_dap3) | rice$fertilizer_dap3 == "other"] <- ""
   rice$fertilizer_dap4[is.na(rice$fertilizer_dap4) | rice$fertilizer_dap4 == "other"] <- ""
   for (i in 1:length(rice$fertilizer_dap1)) {
     if(!rice$fertilizer_dap1[i] == "") {
       split_values <- strsplit(gsub("days", "", rice$fertilizer_dap1[i]), "-")[[1]]
       rice$fertilizer_dap1[i] <- mean(as.integer(split_values[1]), as.integer(split_values[2]))
     }
   }
   for (i in 1:length(rice$fertilizer_dap2)) {
     if(!rice$fertilizer_dap2[i] == "") {
       split_values <- strsplit(gsub("days", "", rice$fertilizer_dap2[i]), "-")[[1]]
       rice$fertilizer_dap2[i] <- mean(as.integer(split_values[1]), as.integer(split_values[2]))
     }
   }
   for (i in 1:length(rice$fertilizer_dap3)) {
     if(!rice$fertilizer_dap3[i] == "") {
       split_values <- strsplit(gsub("days", "", rice$fertilizer_dap3[i]), "-")[[1]]
       rice$fertilizer_dap3[i] <- mean(as.integer(split_values[1]), as.integer(split_values[2]))
     }
   }
   for (i in 1:length(rice$fertilizer_dap4)) {
     if(!rice$fertilizer_dap4[i] == "") {
       split_values <- strsplit(gsub("days", "", rice$fertilizer_dap4[i]), "-")[[1]]
       rice$fertilizer_dap4[i] <- mean(as.integer(split_values[1]), as.integer(split_values[2]))
     }
   }
   rice$fertilizer_dap <- NA
   rice$fertilizer_dap <- paste(c(rice$fertilizer_dap1), c(rice$fertilizer_dap2), c(rice$fertilizer_dap3), c(rice$fertilizer_dap4), sep = ";")
   rice$fertilizer_dap <- gsub("(?<=\\d);+", ";", sub("^;+|;+$$", "", gsub("^;+$$", "", rice$fertilizer_dap)), perl = TRUE)
   rice$fertilizer_type[grepl("rea", rice$fertilizer_type)] <- "NPK; urea"
   rice$fertilizer_type[grepl("NPK", rice$fertilizer_type)] <- "NPK"
   rice$yield <- ifelse(is.na(rice$last_yield),
                        (as.numeric(rice$future_yield) * as.numeric(rice$unit_kg)) / ifelse(!is.na(rice$plot_area), as.numeric(rice$plot_area), as.numeric(rice$cropland_used)),
                        (as.numeric(rice$last_yield) * as.numeric(rice$unit_kg)) / ifelse(!is.na(rice$plot_area), as.numeric(rice$plot_area), as.numeric(rice$cropland_used)))
   rice$yield_part <- "grain"
   rice$yield_moisture <- 20
   rice$yield_isfresh <- FALSE
   rice$crop_price <- as.numeric(rice$crop_price) / as.numeric(rice$unit_kg)
   rice <- rice[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment",
                   "planting_date", "herbicide_used", "fertilizer_used", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer",
                   "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   rice <- rice[!is.na(rice$yield), ]
   
   # Process cassava
   cassava <- cassava[cassava$crop == "Cassava",]
   cassava <- cassava[!grepl("NA", rownames(cassava)),]
   cassava$trial_id <- 1:nrow(cassava)
   cassava$is_survey <- TRUE
   cassava$on_farm <- FALSE
   cassava$crop_cut <- FALSE
   cassava$geo_from_source <- FALSE
   cassava$currency <- "NGN"
   cassava$crop <- tolower(cassava$crop)
   cassava$farmland_owned <- ifelse(cassava$farmland_owned_units == "acre", as.numeric(cassava$farmland_owned) * 0.4, as.numeric(cassava$farmland_owned))
   cassava$cropland_used <- ifelse(cassava$farmland_owned_units == "acre", as.numeric(cassava$cropland_used) * 0.4, as.numeric(cassava$cropland_used))
   cassava$treatment <- ifelse(!is.na(cassava$plot_area), "treatment", "no-treatment")
   cassava$intercrops <- ifelse(cassava$intercrops == "yes", "maize", NA)
   cassava$planting_date <- as.character(as.Date(cassava$planting_date))
   cassava$row_spacing <- NA
   cassava$plant_spacing <- NA
   for (i in 1:length(cassava$plant_density)) {
     if(!is.na(cassava$plant_density[i])) {
       split_values <- strsplit(cassava$plant_density[i], "x")[[1]]
       cassava$row_spacing[i] <- as.numeric(gsub("m", "", split_values[1]))
       cassava$plant_spacing[i] <- as.numeric(gsub("m", "", split_values[2]))
     }
   }
   cassava$herbicide_used <- ifelse(cassava$herbicide_used == "yes", TRUE, FALSE)
   cassava$weeding_method <- cassava$weeding_done
   cassava$weeding_done <- ifelse(!is.na(cassava$weeding_done), TRUE, FALSE)
   cassava$weeding_implement <- ifelse(cassava$weeding_method == "manual", "manual", NA)
   cassava$fertilizer_used <- ifelse(cassava$fertilizer_used == "yes", TRUE, FALSE)
   cassava$N_fertilizer <- ifelse(!is.na(cassava$fertilizer_amount_NPK15), as.numeric(cassava$fertilizer_amount_NPK15) * 0.15, 0)
   cassava$P_fertilizer <- ifelse(!is.na(cassava$fertilizer_amount_NPK15), as.numeric(cassava$fertilizer_amount_NPK15) * 0.15, 0)
   cassava$K_fertilizer <- ifelse(!is.na(cassava$fertilizer_amount_NPK15), as.numeric(cassava$fertilizer_amount_NPK15) * 0.15, 0)
   cassava$fertilizer_type[grepl("rea", cassava$fertilizer_type)] <- "NPK; urea"
   cassava$fertilizer_type[grepl("NPK", cassava$fertilizer_type)] <- "NPK"
   cassava$yield <- ifelse(is.na(cassava$last_yield),
                           (as.numeric(cassava$future_yield) * as.numeric(cassava$unit_kg)) / ifelse(!is.na(cassava$plot_area), as.numeric(cassava$plot_area), as.numeric(cassava$cropland_used)),
                           (as.numeric(cassava$last_yield) * as.numeric(cassava$unit_kg)) / ifelse(!is.na(cassava$plot_area), as.numeric(cassava$plot_area), as.numeric(cassava$cropland_used)))
   cassava$yield <- ifelse(cassava$yield > 10000, cassava$yield/10, cassava$yield)
   cassava$yield_part <- "roots"
   cassava$yield_moisture <- 0
   cassava$yield_isfresh <- FALSE
   cassava$crop_price <- ifelse(is.na(cassava$last_yield),
                                as.numeric(cassava$crop_price) / as.numeric(cassava$unit_kg),
                                as.numeric(cassava$crop_price) / as.numeric(cassava$unit_kg))
   
   cassava <- cassava[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment",
                         "planting_date", "intercrops", "row_spacing", "plant_spacing", #"herbicide_used", "weeding_done", "weeding_method", "weeding_implement",
                         "fertilizer_used", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   cassava <- cassava[!is.na(cassava$yield), ]
   
   # Process maize (only yield)
   maize <- maize[maize$crop == "Maize",]
   maize <- maize[!grepl("NA", rownames(maize)),]
   maize$trial_id <- 1:nrow(maize)
   maize$is_survey <- TRUE
   maize$on_farm <- FALSE
   maize$crop_cut <- FALSE
   maize$geo_from_source <- FALSE
   maize$currency <- "NGN"
   maize$crop <- tolower(maize$crop)
   maize$farmland_owned <- ifelse(maize$farmland_owned_units == "acre", as.numeric(maize$farmland_owned) * 0.4, as.numeric(maize$farmland_owned))
   maize$cropland_used <- ifelse(maize$farmland_owned_units == "acre", as.numeric(maize$cropland_used) * 0.4, as.numeric(maize$cropland_used))
   maize$treatment <- ifelse(!is.na(maize$plot_area), "treatment", "no-treatment")

   maize$yield <- ifelse(is.na(maize$last_yield),
                         (as.numeric(maize$future_yield) * as.numeric(maize$unit_kg)) / ifelse(!is.na(maize$plot_area), as.numeric(maize$plot_area), as.numeric(maize$cropland_used)),
                         (as.numeric(maize$last_yield) * as.numeric(maize$unit_kg)) / ifelse(!is.na(maize$plot_area), as.numeric(maize$plot_area), as.numeric(maize$cropland_used)))
   maize$yield_part <- "grain"
   maize$yield_moisture <- 17.5
   maize$yield_isfresh <- FALSE
   maize$crop_price <- as.numeric(maize$crop_price) / as.numeric(maize$unit_kg)
   maize <- maize[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment", "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   maize <- maize[!is.na(maize$yield), ]
   
   d <- carobiner::bindr(maize, cassava, rice)
   d$irrigated <- FALSE
   d$fertilizer_type <- gsub("other", "unknown", d$fertilizer_type)
   d$trial_id <- as.character(d$trial_id)
   

   carobiner::write_files(meta, d, path=path)
   
}

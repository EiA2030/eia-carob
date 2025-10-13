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
   
   uri <- "aIsZXXUzEPkOWX3H4L9WA5Ud"
   group <- "eia"
   
   meta <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      uri = uri,
      authors ="Gizaw Desta",
      publication= NA,
      data_organization = "ICRISAT",
      data_citation = NA,
      title = "Use Case 1.3: Gvt Ethiopia - landscape specific soil fertility enhancement.",
      description = "This use case will facilitate the development of an App-based digital decision support tool to guide extension agents and farmers in targeting site-specific fertilizer recommendations. The tool will also have district maps for wheat, tef, and sorghum systems to run on a prediction engine that uses advanced analytical techniques to generate fertilizer recommendations that will be of value even at the national level, where its advisories will be a reference point for the national soil health investment and the national soil strategy document of the Ministry of Agriculture of Federal Government of Ethiopia. The advisory tools will target 500 extension agents and 20,000 farmers in 15 woredas spread across five regions of Ethiopia - Amhara, Tigray, SNNP, and Oromia. The farming systems of the regions above are primarily wheat-, sorghum, and teff-based. The ultimate goal of this use case is to scale the advisory tools to 5 million farmers by 2030. prioritize fertilizer investments in the most responsive landscapes and farming systems. The advisory tool will run on a prediction engine that uses advanced analytical techniques to generate fertilizer recommendations that will be of value even at the national level, where its advisories will be a reference point for the national soil health investment and the national soil strategy document of the Ministry of Agriculture of Federal Government of Ethiopia. The advisory tools will target 500 extension agents and 20,000 farmers in 15 woredas spread across five regions of Ethiopia - Amhara, Tigray, SNNP, and Oromia. The farming systems of the regions above are primarily wheat-, sorghum, and teff-based. The ultimate goal of this use case is to scale the advisory tools to 5 million farmers by 2030.",
      group = group,
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC007",
      usecase_name = "ET-HighMix-Gvt ETH",
      activity = 'MELIA',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_type = "survey",
      carob_date = "2025-10-09",
      treatment_vars = "N_fertilizer",
      response_vars = "yield"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-MELIA/", full.names = T))
   
   
   f <- ff[basename(ff) == "EiA_Telephone_survey_2025_01_21_07_37_20_829587.xlsx"]

   ### Read file
   r <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"))
   
   r <- r[r$`Select the Use Case` == "Fertilizer_Ethiopia",] # Include maize, but there is only yield data...
   
   wheat <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,481,483,484,485,486,487,488,495,496,497,499,504,857,858,859,860,861,862)],
                                    from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,481,483,484,485,486,487,488,495,496,497,499,504,857,858,859,860,861,862)],
                                    to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "land_prep_number", "slope_position", "planting_date", "variety_type", "land_prep_method", "seed_rate", "fertilizer_type", "fertilizer_NPS", "fertilizer_urea", "N_splits", "weeding_dates", "crop_rotation", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))

   teff <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,520,522,523,524,525,526,527,534,535,536,538,543,857,858,859,860,861,862)],
                                   from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,520,522,523,524,525,526,527,534,535,536,538,543,857,858,859,860,861,862)],
                                   to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "land_prep_number", "slope_position", "planting_date", "variety_type", "land_prep_method", "seed_rate", "fertilizer_type", "fertilizer_NPS", "fertilizer_urea", "N_splits", "weeding_dates", "crop_rotation", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))

   sorghum <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,559,561,562,563,564,565,566,573,574,575,577,582,857,858,859,860,861,862)],
                                      from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,559,561,562,563,564,565,566,573,574,575,577,582,857,858,859,860,861,862)],
                                      to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area", "land_prep_number", "slope_position", "planting_date", "variety_type", "land_prep_method", "seed_rate", "fertilizer_type", "fertilizer_NPS", "fertilizer_urea", "N_splits", "weeding_dates", "crop_rotation", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))

   #Process wheat
   wheat <- wheat[wheat$crop == "Wheat", ]
   wheat <- wheat[!grepl("NA", rownames(wheat)),]
   wheat$trial_id <- 1:nrow(wheat)
   wheat$is_survey <- TRUE
   wheat$on_farm <- FALSE
   wheat$crop_cut <- FALSE
   wheat$geo_from_source <- FALSE
   wheat$currency <- "ETB"
   wheat$crop <- tolower(wheat$crop)
   wheat$farmland_owned <- ifelse(wheat$farmland_owned_units == "timad", as.numeric(wheat$farmland_owned) * 0.25, as.numeric(wheat$farmland_owned))
   wheat$farmland_owned <- ifelse(wheat$farmland_owned_units == "meter_square", as.numeric(wheat$farmland_owned) * 0.0001, as.numeric(wheat$farmland_owned))
   wheat$farmland_owned <- ifelse(wheat$farmland_owned_units == "acre", as.numeric(wheat$farmland_owned) * 0.4, as.numeric(wheat$farmland_owned))
   wheat$cropland_used <- ifelse(wheat$farmland_owned_units == "timad", as.numeric(wheat$cropland_used) * 0.25, as.numeric(wheat$cropland_used))
   wheat$cropland_used <- ifelse(wheat$farmland_owned_units == "meter_square", as.numeric(wheat$cropland_used) * 0.0001, as.numeric(wheat$cropland_used))
   wheat$cropland_used <- ifelse(wheat$farmland_owned_units == "acre", as.numeric(wheat$cropland_used) * 0.4, as.numeric(wheat$cropland_used))
   wheat$plot_area <- ifelse(wheat$farmland_owned_units == "timad", as.numeric(wheat$plot_area) * 0.25, as.numeric(wheat$plot_area))
   wheat$plot_area <- ifelse(wheat$farmland_owned_units == "meter_square", as.numeric(wheat$plot_area) * 0.0001, as.numeric(wheat$plot_area))
   wheat$plot_area <- ifelse(wheat$farmland_owned_units == "acre", as.numeric(wheat$plot_area) * 0.4, as.numeric(wheat$plot_area))
   wheat$treatment <- ifelse(is.na(wheat$plot_area) | wheat$plot_area == 0, "Didn't use the advisory", "Used the advisory")
   # EGB:
   # # This is an assumption based on the seasonality in Ethiopia
   wheat$planting_date <- as.character(as.Date(ifelse(wheat$planting_date == "Early_planting", as.character("2023-05-15"),
                                                      ifelse(wheat$planting_date == "Mid_planting", as.character("2023-06-01"), "2023-06-15"))))
   wheat$seed_rate <- ifelse(grepl("100-125kg", wheat$seed_rate), 100,
                             ifelse(grepl("above_150kg", wheat$seed_rate), 150,
                                    ifelse(grepl("150kg", wheat$seed_rate), 200, 50)))
   wheat$planting_method[grep("Broadcasting", wheat$land_prep_method)] <- "broadcasting"
   wheat$planting_method[grep("Row_planting", wheat$land_prep_method)] <- "line sowing"
   wheat$planting_method[grep("bothrow_pandbroadcasting", wheat$land_prep_method)] <- "broadcasting"
   wheat$tillage_number <- ifelse(wheat$land_prep_number != "other", as.integer(wheat$land_prep_number), 0)
   wheat$fertilizer_used <- TRUE
   wheat$fertilizer_type <- ifelse(grepl(" ", wheat$fertilizer_type), "urea;NPS", wheat$fertilizer_type)
   wheat$fertilizer_amount <- rowSums(wheat[, c("fertilizer_NPS", "fertilizer_urea")])
   # EGB
   # # NPS Blend composition can be found in: https://www.sciencedirect.com/science/article/pii/S2405844024115356#:~:text=Therefore%2C%20in%20order%20to%20improve,crops%20solely%20using%20inorganic%20fertilizers.
   wheat$N_fertilizer <- ifelse(!is.na(wheat$fertilizer_NPS), wheat$fertilizer_NPS * 0.19, 0) + ifelse(!is.na(wheat$fertilizer_urea), wheat$fertilizer_urea * 0.46, 0)
   wheat$P_fertilizer <- ifelse(!is.na(wheat$fertilizer_NPS), wheat$fertilizer_NPS * 0.38, 0)
   wheat$K_fertilizer <- 0
   wheat$S_fertilizer <- ifelse(!is.na(wheat$fertilizer_NPS), wheat$fertilizer_NPS * 0.07, 0)
   wheat$N_splits <- ifelse(wheat$N_splits == "One_third", 3,
                            ifelse(wheat$N_splits == "Half", 2, wheat$N_splits))
   wheat$weeding_done <- ifelse(!is.na(wheat$weeding_dates), TRUE, FALSE)
   wheat$weeding_dates <- as.character(as.Date(ifelse(wheat$weeding_dates == "2-3WAP", as.Date(wheat$planting_date, "%Y-%m-%d") + 15,
                                                      ifelse(wheat$weeding_dates == "3-4WAP", as.Date(wheat$planting_date, "%Y-%m-%d") + 25, as.Date(wheat$planting_date, "%Y-%m-%d") + 35))))
   wheat$yield <- (as.numeric(wheat$last_yield) * as.numeric(wheat$unit_kg)) / ifelse(wheat$plot_area != 0, as.numeric(wheat$plot_area), as.numeric(wheat$cropland_used))
   wheat$yield_part <- "grain"
   wheat$yield_moisture <- 19
   wheat$yield_isfresh <- FALSE
   wheat$crop_price <- as.numeric(wheat$crop_price) / as.numeric(wheat$unit_kg)
   
   wheat <- wheat[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment", "slope_position", "tillage_number", "variety_type", "planting_method", "planting_date", "seed_rate", "fertilizer_used", "fertilizer_type", "fertilizer_amount", "N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer", "N_splits", "weeding_done", "weeding_dates", "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   
   #Process teff
   teff <- teff[teff$crop == "Teff", ]
   teff <- teff[!grepl("NA", rownames(teff)),]
   teff$trial_id <- 1:nrow(teff)
   teff$is_survey <- TRUE
   teff$on_farm <- FALSE
   teff$crop_cut <- FALSE
   teff$geo_from_source <- FALSE
   teff$currency <- "ETB"
   teff$crop <- tolower(teff$crop)
   teff$farmland_owned <- ifelse(teff$farmland_owned_units == "timad", as.numeric(teff$farmland_owned) * 0.25, as.numeric(teff$farmland_owned))
   teff$farmland_owned <- ifelse(teff$farmland_owned_units == "meter_square", as.numeric(teff$farmland_owned) * 0.0001, as.numeric(teff$farmland_owned))
   teff$farmland_owned <- ifelse(teff$farmland_owned_units == "acre", as.numeric(teff$farmland_owned) * 0.4, as.numeric(teff$farmland_owned))
   teff$cropland_used <- ifelse(teff$farmland_owned_units == "timad", as.numeric(teff$cropland_used) * 0.25, as.numeric(teff$cropland_used))
   teff$cropland_used <- ifelse(teff$farmland_owned_units == "meter_square", as.numeric(teff$cropland_used) * 0.0001, as.numeric(teff$cropland_used))
   teff$cropland_used <- ifelse(teff$farmland_owned_units == "acre", as.numeric(teff$cropland_used) * 0.4, as.numeric(teff$cropland_used))
   teff$plot_area <- ifelse(teff$farmland_owned_units == "timad", as.numeric(teff$plot_area) * 0.25, as.numeric(teff$plot_area))
   teff$plot_area <- ifelse(teff$farmland_owned_units == "meter_square", as.numeric(teff$plot_area) * 0.0001, as.numeric(teff$plot_area))
   teff$plot_area <- ifelse(teff$farmland_owned_units == "acre", as.numeric(teff$plot_area) * 0.4, as.numeric(teff$plot_area))
   teff$treatment <- ifelse(is.na(teff$plot_area) | teff$plot_area == 0, "Didn't use the advisory", "Used the advisory")
   # EGB:
   # # This is an assumption based on the seasonality in Ethiopia
   teff$planting_date <- as.character(as.Date(ifelse(teff$planting_date == "Early_planting", as.character("2023-05-15"),
                                                      ifelse(teff$planting_date == "Mid_planting", as.character("2023-06-01"), "2023-06-15"))))
   teff$seed_rate <- ifelse(grepl("100-125kg", teff$seed_rate), 100,
                             ifelse(grepl("above_150kg", teff$seed_rate), 150,
                                    ifelse(grepl("150kg", teff$seed_rate), 200, 50)))
   teff$planting_method[grep("Broadcasting", teff$land_prep_method)] <- "broadcasting"
   teff$planting_method[grep("Row_planting", teff$land_prep_method)] <- "line sowing"
   teff$planting_method[grep("bothrow_pandbroadcasting", teff$land_prep_method)] <- "broadcasting"
   teff$tillage_number <- ifelse(teff$land_prep_number != "other", as.integer(teff$land_prep_number), 0)
   teff$fertilizer_used <- TRUE
   teff$fertilizer_type <- ifelse(grepl(" ", teff$fertilizer_type), "urea;NPS", teff$fertilizer_type)
   teff$fertilizer_amount <- rowSums(teff[, c("fertilizer_NPS", "fertilizer_urea")])
   # EGB
   # # NPS Blend composition can be found in: https://www.sciencedirect.com/science/article/pii/S2405844024115356#:~:text=Therefore%2C%20in%20order%20to%20improve,crops%20solely%20using%20inorganic%20fertilizers.
   teff$N_fertilizer <- ifelse(!is.na(teff$fertilizer_NPS), teff$fertilizer_NPS * 0.19, 0) + ifelse(!is.na(teff$fertilizer_urea), teff$fertilizer_urea * 0.46, 0)
   teff$P_fertilizer <- ifelse(!is.na(teff$fertilizer_NPS), teff$fertilizer_NPS * 0.38, 0)
   teff$K_fertilizer <- 0
   teff$S_fertilizer <- ifelse(!is.na(teff$fertilizer_NPS), teff$fertilizer_NPS * 0.07, 0)
   teff$N_splits <- ifelse(teff$N_splits == "One_third", 3,
                            ifelse(teff$N_splits == "Half", 2, teff$N_splits))
   teff$weeding_done <- ifelse(!is.na(teff$weeding_dates), TRUE, FALSE)
   teff$weeding_dates <- as.character(as.Date(ifelse(teff$weeding_dates == "2-3WAP", as.Date(teff$planting_date, "%Y-%m-%d") + 15,
                                                      ifelse(teff$weeding_dates == "3-4WAP", as.Date(teff$planting_date, "%Y-%m-%d") + 25, as.Date(teff$planting_date, "%Y-%m-%d") + 35))))
   teff$yield <- (as.numeric(teff$last_yield) * as.numeric(teff$unit_kg)) / ifelse(teff$plot_area != 0, as.numeric(teff$plot_area), as.numeric(teff$cropland_used))
   teff$yield_part <- "grain"
   teff$yield_moisture <- 19
   teff$yield_isfresh <- FALSE
   teff$crop_price <- as.numeric(teff$crop_price) / as.numeric(teff$unit_kg)
   
   teff <- teff[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment", "slope_position", "tillage_number", "variety_type", "planting_method", "planting_date", "seed_rate", "fertilizer_used", "fertilizer_type", "fertilizer_amount", "N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer", "N_splits", "weeding_done", "weeding_dates", "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   
   #Process sorghum
   sorghum <- sorghum[sorghum$crop == "Sorghum", ]
   sorghum <- sorghum[!grepl("NA", rownames(sorghum)),]
   sorghum$trial_id <- 1:nrow(sorghum)
   sorghum$is_survey <- TRUE
   sorghum$on_farm <- FALSE
   sorghum$crop_cut <- FALSE
   sorghum$geo_from_source <- FALSE
   sorghum$currency <- "ETB"
   sorghum$crop <- tolower(sorghum$crop)
   sorghum$farmland_owned <- ifelse(sorghum$farmland_owned_units == "timad", as.numeric(sorghum$farmland_owned) * 0.25, as.numeric(sorghum$farmland_owned))
   sorghum$farmland_owned <- ifelse(sorghum$farmland_owned_units == "meter_square", as.numeric(sorghum$farmland_owned) * 0.0001, as.numeric(sorghum$farmland_owned))
   sorghum$farmland_owned <- ifelse(sorghum$farmland_owned_units == "acre", as.numeric(sorghum$farmland_owned) * 0.4, as.numeric(sorghum$farmland_owned))
   sorghum$cropland_used <- ifelse(sorghum$farmland_owned_units == "timad", as.numeric(sorghum$cropland_used) * 0.25, as.numeric(sorghum$cropland_used))
   sorghum$cropland_used <- ifelse(sorghum$farmland_owned_units == "meter_square", as.numeric(sorghum$cropland_used) * 0.0001, as.numeric(sorghum$cropland_used))
   sorghum$cropland_used <- ifelse(sorghum$farmland_owned_units == "acre", as.numeric(sorghum$cropland_used) * 0.4, as.numeric(sorghum$cropland_used))
   sorghum$plot_area <- ifelse(sorghum$farmland_owned_units == "timad", as.numeric(sorghum$plot_area) * 0.25, as.numeric(sorghum$plot_area))
   sorghum$plot_area <- ifelse(sorghum$farmland_owned_units == "meter_square", as.numeric(sorghum$plot_area) * 0.0001, as.numeric(sorghum$plot_area))
   sorghum$plot_area <- ifelse(sorghum$farmland_owned_units == "acre", as.numeric(sorghum$plot_area) * 0.4, as.numeric(sorghum$plot_area))
   sorghum$treatment <- ifelse(is.na(sorghum$plot_area) | sorghum$plot_area == 0, "Didn't use the advisory", "Used the advisory")
   # EGB:
   # # This is an assumption based on the seasonality in Ethiopia
   sorghum$planting_date <- as.character(as.Date(ifelse(sorghum$planting_date == "Early_planting", as.character("2023-05-15"),
                                                     ifelse(sorghum$planting_date == "Mid_planting", as.character("2023-06-01"), "2023-06-15"))))
   sorghum$seed_rate <- ifelse(grepl("100-125kg", sorghum$seed_rate), 100,
                            ifelse(grepl("above_150kg", sorghum$seed_rate), 150,
                                   ifelse(grepl("150kg", sorghum$seed_rate), 200, 50)))
   sorghum$planting_method[grep("Broadcasting", sorghum$land_prep_method)] <- "broadcasting"
   sorghum$planting_method[grep("Row_planting", sorghum$land_prep_method)] <- "line sowing"
   sorghum$planting_method[grep("bothrow_pandbroadcasting", sorghum$land_prep_method)] <- "broadcasting"
   sorghum$tillage_number <- ifelse(sorghum$land_prep_number != "other", as.integer(sorghum$land_prep_number), 0)
   sorghum$fertilizer_used <- TRUE
   sorghum$fertilizer_type <- ifelse(grepl(" ", sorghum$fertilizer_type), "urea;NPS", sorghum$fertilizer_type)
   sorghum$fertilizer_amount <- rowSums(sorghum[, c("fertilizer_NPS", "fertilizer_urea")])
   # EGB
   # # NPS Blend composition can be found in: https://www.sciencedirect.com/science/article/pii/S2405844024115356#:~:text=Therefore%2C%20in%20order%20to%20improve,crops%20solely%20using%20inorganic%20fertilizers.
   sorghum$N_fertilizer <- ifelse(!is.na(sorghum$fertilizer_NPS), sorghum$fertilizer_NPS * 0.19, 0) + ifelse(!is.na(sorghum$fertilizer_urea), sorghum$fertilizer_urea * 0.46, 0)
   sorghum$P_fertilizer <- ifelse(!is.na(sorghum$fertilizer_NPS), sorghum$fertilizer_NPS * 0.38, 0)
   sorghum$K_fertilizer <- 0
   sorghum$S_fertilizer <- ifelse(!is.na(sorghum$fertilizer_NPS), sorghum$fertilizer_NPS * 0.07, 0)
   sorghum$N_splits <- ifelse(sorghum$N_splits == "One_third", 3,
                           ifelse(sorghum$N_splits == "Half", 2, sorghum$N_splits))
   sorghum$weeding_done <- ifelse(!is.na(sorghum$weeding_dates), TRUE, FALSE)
   sorghum$weeding_dates <- as.character(as.Date(ifelse(sorghum$weeding_dates == "2-3WAP", as.Date(sorghum$planting_date, "%Y-%m-%d") + 15,
                                                     ifelse(sorghum$weeding_dates == "3-4WAP", as.Date(sorghum$planting_date, "%Y-%m-%d") + 25, as.Date(sorghum$planting_date, "%Y-%m-%d") + 35))))
   sorghum$yield <- (as.numeric(sorghum$last_yield) * as.numeric(sorghum$unit_kg)) / ifelse(sorghum$plot_area != 0, as.numeric(sorghum$plot_area), as.numeric(sorghum$cropland_used))
   sorghum$yield_part <- "grain"
   sorghum$yield_moisture <- 19
   sorghum$yield_isfresh <- FALSE
   sorghum$crop_price <- as.numeric(sorghum$crop_price) / as.numeric(sorghum$unit_kg)
   
   sorghum <- sorghum[,c("trial_id", "country", "adm1", "location", "is_survey", "on_farm", "crop_cut", "geo_from_source", "gender", "crop", "farmland_owned", "cropland_used", "plot_area", "treatment", "slope_position", "tillage_number", "variety_type", "planting_method", "planting_date", "seed_rate", "fertilizer_used", "fertilizer_type", "fertilizer_amount", "N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer", "N_splits", "weeding_done", "weeding_dates", "yield", "yield_part", "yield_moisture", "yield_isfresh", "crop_price")]
   
   d <- carobiner::bindr(wheat, teff, sorghum)
   d$irrigated <- FALSE
   d$fertilizer_type <- gsub("other", "unknown", d$fertilizer_type)
   d$trial_id <- as.character(d$trial_id)
   
   # EGB:
   # # Fix treatments for very small plot_area (treatments)
   d$treatment <- ifelse(d$plot_area < 0.125, "Didn't use the advisory", "Used the advisory")
   d$plot_area <- ifelse(d$plot_area < 0.125, 0, d$plot_area)
   
   d$N_splits <- as.integer(d$N_splits)

   carobiner::write_files(meta, d, path=path)
   
}

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
   
   uri <- "uGSZBoxcQvKaOxMuol29Zk2y"
   group <- "eia"
   
   meta <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      uri = uri,
      authors ="?",
      publication= NA,
      data_institute = "?",
      title = NA,
      group = group,
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC001",
      usecase_name = 'WA-Rice-ATAFI/MOVE',
      activity = 'MELIA',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_type = "survey",
      # response_vars= "yield",
      # treatment_vars ="N_fertilizer;P_fertilizer;K_fertilizer;intercrops",
      carob_date="2025-02-21"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-MELIA/", full.names = T))
   
   
   f <- ff[basename(ff) == "EiA_Telephone_survey_2025_01_21_07_37_20_829587.xlsx"]

   ### Read file
   r <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"))
   
   r1 <- carobiner::change_names(r[, c(9,10,18,20,21,22,28,30,35,69,70,72,73,124,126,128, 130, 136, 139, 141, 142, 148, 155, 161,162,163, 857, 858, 859, 860, 861, 862)],
                                 from = colnames(r)[c(9,10,18,20,21,22,28,30,35,69,70,72,73,124,126,128, 130, 136, 139, 141, 142, 148, 155, 161,162,163, 857, 858, 859, 860, 861, 862)],
                                 to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "location", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "plot_area","planting_date_wet", "planting_date_dry","variety_type", "land_prep_method", "herbicide_used", "herbicide_implement", "fertilizer_used", "fertilizer_type","N_splits", "fertilizer_dap", "NPK151515_50kg_bags", "NPK201010_50kg_bags", "urea_50kg_bags", "crop_units", "unit_kg", "future_yield", "last_yield", "proportion_sold", "crop_price"))
   
   d1 <- r1[r1$usecase_name == "atafi",]
   d1$currency <- ifelse(d1$currency == "Nigerian Naira (NGN)", "NGN", d1$currency)
   d1$is_survey <- TRUE
   d1$on_farm <- FALSE
   d1$treatment <- ifelse(!is.na(d1$plot_area), "Implemented", "Not Implemented")
   d1$geo_from_source <- FALSE
   d1$crop <- tolower(d1$crop)
   d1$planting_date <- gsub("-.*", "\\1", ifelse(is.na(d1$planting_date_wet), d1$planting_date_dry, d1$planting_date_wet))
   d1$planting_date <- as.character(as.Date(paste0("2023-", substr(gsub("\\d","", d1$planting_date), 1, 3), "-", sprintf("%02d", as.integer(gsub("\\D","", d1$planting_date)))), "%Y-%b-%d"))
   d1$farming_system <- ifelse(is.na(d1$planting_date_dry), "Lowland", "Highland")
   d1$area <- ifelse(d1$farmland_owned_units == "acre", as.numeric(d1$cropland_used) * 0.4047, as.numeric(d1$cropland_used))
   d1$crop_weight <- ifelse(is.na(d1$last_yield), d1$future_yield, d1$last_yield) * d1$unit_kg
   d1$yield <- d1$crop_weight / ifelse(!is.na(d1$plot_area), d1$plot_area, d1$area)
   d1$yield_part <- "grain"
   d1$fertilizer_used <- ifelse(d1$fertilizer_used == "yes", TRUE, FALSE)
   d1$fertilizer_type <- gsub("NPK; NPK", "NPK", gsub("npk", "NPK", sapply(sapply(sapply(strsplit(d1$fertilizer_type, " "), function(x) gsub(":.*","\\1",as.character(x))), tolower), paste0,collapse="; ")))
   d1$fertilizer_type[d1$fertilizer_type == "NA"] <- NA
   d1$fertilizer_dap <- gsub("-.*", "\\1", d1$fertilizer_dap)
   d1$N_splits <- sapply(strsplit(d1$N_splits, " "), length)
   d1$N_fertilizer <- (ifelse(is.na(d1$NPK151515_50kg_bags), 0, d1$NPK151515_50kg_bags * 50 * 0.15) + ifelse(is.na(d1$NPK201010_50kg_bags), 0, d1$NPK201010_50kg_bags * 50 * 0.2) + ifelse(is.na(d1$urea_50kg_bags), 0, d1$urea_50kg_bags * 50 * 0.46)) / ifelse(!is.na(d1$plot_area), d1$plot_area, d1$area)
   d1$P_fertilizer <- (ifelse(is.na(d1$NPK151515_50kg_bags), 0, d1$NPK151515_50kg_bags * 50 * 0.15 * 0.436) + ifelse(is.na(d1$NPK201010_50kg_bags), 0, d1$NPK201010_50kg_bags * 50 * 0.1 * 0.436)) / ifelse(!is.na(d1$plot_area), d1$plot_area, d1$area)
   d1$K_fertilizer <- (ifelse(is.na(d1$NPK151515_50kg_bags), 0, d1$NPK151515_50kg_bags * 50 * 0.15 * 0.83) + ifelse(is.na(d1$NPK201010_50kg_bags), 0, d1$NPK201010_50kg_bags * 50 * 0.1 * 0.83)) / ifelse(!is.na(d1$plot_area), d1$plot_area, d1$area)
   d1$herbicide_used <- ifelse(d1$herbicide_used == "yes", TRUE, FALSE)
   d1$crop_price <- d1$crop_price / d1$unit_kg
   
   # Questions:
   # 1. Assumption: NA records in "What is the area of the ${Crop} field (in ${unitArea}) in which the ${UseCase} recommendation was applied?" were considered reference; and the others as treatment
   # 2. Due to mixing future ("How many ${unitName}s of raw ${Crop} are expected to be harvested in your ${Crop} field?") and past ("How many ${unitName}s of raw ${Crop} were harvested last time ${Crop} was grown in your field?")
   #    I used both.
   # 3. What is the correct area to use?
   
   d <- d1[,c("country", "adm1", "currency", "date", "gender", "usecase_name", "event_date", "is_survey", "on_farm", "location", "geo_from_source",
              "farming_system", "farmland_owned", "area", "crop", "variety_type", "planting_date", "land_prep_method",
              "treatment", "herbicide_used", "herbicide_implement", "fertilizer_used", "fertilizer_type", "fertilizer_dap", "N_splits",
              "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "yield_part", "proportion_sold", "crop_price")]
   
   d <- carobiner::change_names(d, from = "area", to = "plot_area")
   
   # # For other use cases I need to change the relevant columns.
   # USC008 <- r[r$usecase_name == "sasakawa_ng",]
   # USC013 <- r[r$usecase_name == "mercy_corps",]
   # USC007 <- r[r$usecase_name == "Fertilizer_Ethiopia",]
   # USC006 <- r[r$usecase_name == "Digital_Green",]
   # AKILIMO <- r[r$usecase_name == "AKILIMO",]
   # USC009 <- r[r$usecase_name == "GAIP_Ghana",]

   carobiner::write_files(meta, d, path=path)
   
}

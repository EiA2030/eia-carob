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
   
   uri <- "xgBDAj4HkerT34oWs9WxSV9z"
   group <- "eia"
   
   meta <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      uri = uri,
      authors ="Kalpana Sharma; Elly Atieno",
      publication= NA,
      data_organization = "CIP",
      data_citation = NA,
      title = "Use Case 2.9: MercyCorps AgriFin -  multi-crop agronomic content dissemination",
      description = "Excellence in Agronomy will assemble knowledge and expertise from 5 CGIAR Centers, to provide tools, and insights onto the Sprout Platform. The following solutions will be developed static content service, dynamic digital weather, content on demand",
      group = group,
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC013",
      usecase_name = "NG-Akilimo-MC Sprout",
      activity = 'MELIA',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_type = "survey",
      # response_vars= "yield",
      # treatment_vars ="N_fertilizer;P_fertilizer;K_fertilizer;intercrops",
      carob_date = "2025-10-03",
      treatment_vars = "N_fertilizer;seed_rate",
      response_vars = "yield"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-MELIA/", full.names = T))
   
   
   f <- ff[basename(ff) == "EiA_Telephone_survey_2025_01_21_07_37_20_829587.xlsx"]

   ### Read file
   r <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"))
   
   r1 <- carobiner::change_names(r[, c(9,10,18,20,21,22,34,35,69,70,72,90,92,96,104,106,858,860,861,862)],
                                 from = colnames(r)[c(9,10,18,20,21,22,34,35,69,70,72,90,92,96,104,106,858,860,861,862)],
                                 to = c("country", "currency", "usecase_name", "date", "gender", "adm1", "event_date", "crop", "farmland_owned", "farmland_owned_units", "cropland_used", "land_prep_implement", "seed_rate", "transplanting_days", "fertilizer_type", "harvest_days", "yield_bunch_weight", "yield_bunch_total", "yield_sold", "crop_price_bunch"))
   
   d1 <- r1[r1$usecase_name == "mercy_corps",]
   
   d1$country <- d1$country
   d1$adm1 <- d1$adm1
   d1$currency <- ifelse(d1$currency == "Nigerian Naira (NGN)", "NGN", d1$currency)
   d1$gender <- ifelse(d1$gender == "M", "male", "female")
   d1$is_survey <- TRUE
   d1$on_farm <- FALSE
   d1$geo_from_source <- FALSE
   d1$farmland_owned[grep("acre", d1$farmland_owned_units)] <- d1$farmland_owned[grep("acre", d1$farmland_owned_units)] * 0.4
   
   # d1$treatment <- ifelse(!is.na(d1$plot_area), "Implemented", "Not Implemented")
   
   d1$crop <- tolower(d1$crop)
   d1$cropland_used[grep("acre", d1$farmland_owned_units)] <- as.numeric(d1$cropland_used[grep("acre", d1$farmland_owned_units)]) * 0.4
   d1$cropland_used <- as.numeric(d1$cropland_used)
   d1$land_prep_implement <- ifelse(d1$land_prep_implement == "tractor", "4 wheel tractor",
                                    ifelse(d1$land_prep_implement == "harrow", "chain harrow", d1$land_prep_implement))
   d1$seed_rate <- as.numeric(substr(d1$seed_rate, 1, 1))
   d1$planting_date <- as.character(as.Date(d1$event_date))
   d1$transplanting_date <- as.character(as.Date(d1$planting_date) + as.numeric(substr(d1$transplanting_days, 1, 1)) * 7)
   d1$harvest_date <- as.character(as.Date(d1$planting_date) + as.numeric(d1$harvest_days) * 7)
   d1$irrigated <- FALSE
   d1$fertilizer_amount <- as.numeric(gsub("kg", "", gsub("_urea_acre", "", d1$fertilizer_type))) * 0.4
   d1$N_fertilizer <- d1$fertilizer_amount * 0.46
   d1$P_fertilizer = 0
   d1$K_fertilizer = 0
   d1$fertilizer_type <- "urea"
   d1$yield <- (d1$yield_bunch_weight * d1$yield_bunch_total) / d1$cropland_used
   d1$yield_moisture <- 20 # Assumed
   d1$yield_part <- "bunch"
   d1$yield_isfresh <- FALSE
   d1$crop_cut <- FALSE
   d1$crop_price <- as.numeric(d1$crop_price_bunch) / as.numeric(d1$yield_bunch_weight)
   d1$crop_price <- ifelse(d1$crop_price < 1, d1$crop_price * 1000, d1$crop_price)
   
   d <- d1[,c("country", "adm1", "currency", "gender", "is_survey", "on_farm", "crop_cut", "geo_from_source", "farmland_owned", "cropland_used", "crop",
              "land_prep_implement", "seed_rate", "planting_date", "transplanting_date", "harvest_date", "irrigated", "fertilizer_type", "fertilizer_amount", "N_fertilizer", "P_fertilizer", "K_fertilizer",
              "yield", "yield_moisture", "yield_part", "yield_isfresh", "crop_price")]
   
   d <- d[!is.na(d$yield),]

   carobiner::write_files(meta, d, path=path)
   
}

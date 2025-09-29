# R script for EiA version of"carob"

## ISSUES
# Energy of irrigation available, but how to add it?
# Added ET0 but it is not a standard term
# Nitrogen use efficiency (NUE) is pre-calculated

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "dvcGW3xxMuK6QCHu1up1lMzu"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Samar Ataher; Ajit Govind; Vinay Nangia",
    data_organization = "ICARDA",
    data_citation = "NA",
    title = "Excellence in Agronomy Egypt Government Use Case Validation",
    description = "The EiA will develop and validate a digital advisory&nbsp;tool that will offer wheat farmers crucial agronomic&nbsp;information, including fertilizer use, water usage, crop&nbsp;rotations, etc during incubation phase. This digitalization&nbsp;of agriculture research and advisory will serve extension,&nbsp;youth and women friendly tool to provide site-specific&nbsp;agronomic recommendations generated from various farm analytics corroborated by data from farmers and&nbsp;<br />\r\nextension agents for last mile delivery to millions of&nbsp;farmers by 2030",
    license = "Unknown",
    group = group,
    publication = NA,
    usecase_code = "USC004",
    usecase_name = "EG-Irrigated-Gvt",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "on-farm experiment", 
    carob_date = "2025-09-29",
    treatment_vars = "N_fertilizer;seed_rate;irrigation_method;irrigation_number;irrigation_amount;eto",
    response_vars = "yield",
    notes = NA
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Egypt-Government-Validation/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "EiA_Egypt-Governement_Validation_raw_2024.xlsx"]
  # Read relevant file
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    trial_id = as.character(r$record_id),
    country = r$country,
    adm1 = r$adm1,
    adm2 = r$adm2,
    adm3 = r$adm3,
    longitude = r$longitude,
    latitude = r$latitude,
    cropland_used = r$cropland_used,
    plot_area = r$plot_area,
    soil_texture = tolower(r$soil_texture),
    soil_pH = r$soil_pH,
    soil_EC = r$soil_EC,
    previous_crop = tolower(r$previous_crop),
    season = r$season,
    crop = r$crop,
    variety = r$variety,
    treatment = r$treatment,
    planting_method = ifelse(grepl("Broadcasting", r$treatment), "broadcasting", "line sowing"),
    land_prep_method = ifelse(grepl("Raised", r$treatment), "raised beds", "conventional"),
    seed_rate = r$`seeding rate`,
    planting_date = as.character(as.Date(r[[21]])),
    harvest_date = as.character(as.Date(r$harvest_date)),
    eto = r$Total_Eto,
    N_fertilizer = r$N_fertilizer_unit,
    P_fertilizer = 0,
    K_fertilizer = 0,
    irrigation_method = ifelse(grepl("flood", r$irrigation_method), "flood", "furrow"),
    irrigation_number = as.integer(r$irrigation_number),
    irrigation_amount = r$irrigation_amount,
    yield = (r$yield * 1000) * 0.87, # Assuming 13% moisture
    yield_moisture = 13,
    yield_isfresh = TRUE,
    yield_part = "grain",
    crop_price = r[[31]] / r$yield,
    currency = "USD",
    # # d$Total_Eto <- d$WP <- d$N_NUE <- d$`Irrigation- energy _consumption` <- d$`gross-income` <- d$record_id <- NULL,
    on_farm = TRUE,
    geo_from_source = TRUE,
    is_survey = FALSE,
    irrigated = TRUE
  )
  
  d$previous_crop[d$previous_crop == "cane"] <- "sugarcane"
  d$previous_crop[d$previous_crop == "green beans"] <- "common bean"
  d$previous_crop[d$previous_crop == "seasame"] <- "sugarcane"
  d$previous_crop[d$previous_crop %in% c("soya bean", "soybeans")] <- "soybean"
  d$soil_texture[d$soil_texture %in% c("clay-loam")] <- "clay loam"

  # d[21] <- NULL
  # d$harvest_date <- as.Date(d$harvest_date)
  # d$eto <- d$Total_Eto
  # d[24] <- NULL
  # d$previous_crop <- tolower(d$previous_crop)
  # d$seed_rate <- d$`seeding rate`
  # d$N_fertilizer <- d$N_fertilizer_unit
  # d$N_fertilizer_unit <- NULL
  # d$P_fertilizer <- d$K_fertilizer <- 0
  # d$yield <- (d$yield*1000)*0.87 # Assuming 13% moisture
  # d$yield_moisture <- 13
  # d$yield_isfresh <- TRUE
  # d$crop_price <- d[31] / d$yield
  # d$currency <- "USD"
  # d$Total_Eto <- d$WP <- d$N_NUE <- d$`Irrigation- energy _consumption` <- d$`gross-income` <- d$record_id <- NULL
  # 
  # d$geo_from_source <- TRUE
  # d$is_survey <- FALSE
  # d$irrigated <- TRUE

  carobiner::write_files(meta, d, path = path)
  
}

carob_script(path)
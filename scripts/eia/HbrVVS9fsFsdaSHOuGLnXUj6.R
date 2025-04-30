# R script for EiA version of"carob"

## ISSUES
# ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "HbrVVS9fsFsdaSHOuGLnXUj6"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Samar Ataher; Ajit Govind",
    data_institute = "ICARDA",
    data_citation = "",
    title = NA,
    description ="Government of Egypt Use Case Validations for wheat in 2023-2024",
    license = NA,
    group = group,
    publication=NA,
    usecase_code = "USC004",
    usecase_name = "EG-Irrigated-Gvt",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "on-farm experiment",
    carob_date="2025-04-30",
    treatment_vars = "soil_pH;soil_EC;previous_crop;seed_density;irrigation_method;irrigation_amount",
    response_vars= "yield;gross_income",
    notes = ""
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Egypt-Government-Validation", full.names = T))
  
  # Retrieve relevant file(s)
  f <- ff[basename(ff) == "EiA_Egypt-Governement_Validation_raw_2024.xlsx"]

  # Read relevant file(s)
  r <- readxl::read_excel(f, sheet = "Validation")
  
  # EGB:
  # # The data is already carobized
  
  d <- data.frame(r[,3:ncol(r)])
  
  d$dataset_id <- uri
  d$trial_id <- as.character(match(d$longitude, unique(d$longitude)))
  d$geo_from_source <- TRUE
  d$on_farm <- as.logical(d$on_farm)
  d$is_survey <- FALSE
  d$previous_crop <- tolower(d$previous_crop)
  d$previous_crop[d$previous_crop == "cane"] <- "sugarcane"
  d$previous_crop[d$previous_crop == "green beans"] <- "green bean"
  d$previous_crop[d$previous_crop == "seasame"] <- "sesame"
  d$previous_crop[d$previous_crop %in% c("soya bean", "soybeans")] <- "soybean"
  d$previous_crop[d$previous_crop == "zucchini"] <- "squash"
  d$evapotranspiration <- d$Total_Eto
  d$irrigated <- TRUE
  d$irrigation_method[d$irrigation_method == "Surface- flood"] <- "uncontrolled flooding"
  d$irrigation_method[d$irrigation_method == "Surface-furrow"] <- "furrow"
  d$irrigation_number <- as.integer(d$irrigation_number)
  d$N_fertilizer <- d$N_fertilizer_unit
  d$yield <- d$yield * 1000 # To kg/ha
  d$soil_texture <- tolower(d$soil_texture)
  d$soil_texture[d$soil_texture == "clay-loam"] <- "clay loam"
  d$planting_date <- as.character(d$planting_date..)
  d$harvest_date <- as.character(d$harvest_date)
  d$seed_density <- d$seeding.rate
  d$N_fertilizer_unit <- d$Total_Eto <- d$WP <- d$N_NUE <- d$seeding.rate <- d$Irrigation..energy._consumption <- d$gross.income <- d$planting_date.. <- NULL
  # d$`Irrigation- energy _consumption` <- NULL
  # d$`gross-income` <- NULL
  # d$planting_date.. <- NULL

  carobiner::write_files(meta, d, path = path)
  
}

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
    treatment_vars = "soil_pH;soil_EC;previous_crop;seeding_rate;irrigation_method;irrigation_amount",
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
  
  d <- r[,3:ncol(r)]
  
  d$dataset_id <- uri
  d$geo_from_source <- TRUE
  d$evapotranspiration <- d$Total_Eto
  d$irrigated <- TRUE
  d$N_fertilizer <- d$N_fertilizer_unit
  d$yield <- d$yield * 1000 # To kg/ha
  d$N_fertilizer_unit <- d$Total_Eto <- NULL

  carobiner::write_files(meta, d, path = path)
  
}

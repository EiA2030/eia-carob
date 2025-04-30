# R script for EiA version of"carob"

## ISSUES
# ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "owJxZcOXxHx77d2W6m60nEOp"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Hung Van Nguyen",
    data_institute = "IRRI",
    data_citation = "",
    title = NA,
    description ="Vietnam DSRC Use Case Validations for rice",
    license = NA,
    group = group,
    publication=NA,
    usecase_code = "USC018",
    usecase_name = "SEA-DSRC-Vietnam",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "compilation",
    carob_date="2025-04-30",
    treatment_vars = "planting_date;seed_density;N_fertilizer;P_fertilizer;K_fertilizer;insecticide_times",
    response_vars= "yield",
    notes = ""
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Vietnam-DSRC-Validation", full.names = T))
  
  # Retrieve relevant file(s)
  f <- ff[basename(ff) == "EiA_DSRC_Vietnam_Validation_clean_2018_2024.xlsx"]

  # Read relevant file(s)
  r <- readxl::read_excel(f, sheet = 1)
  
  d <- data.frame(r[,3:ncol(r)])
  
  d$dataset_id <- uri
  d$trial_id <- as.character(d$Trial.ID)
  d$country <- "Vietnam"
  d$adm1 <- d$Province
  d$longitude <- NA
  d$latitude <- NA
  d$geo_from_source <- FALSE
  d$on_farm <- ifelse(d$Trial.type..on.station..on.farm. == "On-farm", TRUE, FALSE)
  d$is_survey <- FALSE
  d$crop <- "rice"
  d$yield_part <- "grain"
  d$planting_date <- as.character(substr(d$Year, 1, 4))
  d$harvest_date <- NA
  d$season <- d$Season
  d$season[d$season == "W-S"] <- "Winter-Spring"
  d$season[d$season == "S-A"] <- "Summer-Autumn"
  d$treatment <- d$Name
  d$seed_density <- as.integer(d$Seed.rate...kg.ha.)
  d$N_fertilizer <- d$Ferilizer.N...kg.ha.
  d$P_fertilizer <- d$Ferilizer.P2O5...kg.ha. * 0.436
  d$K_fertilizer <- d$Ferilizer.K2O...kg.ha. * 0.83
  d$irrigated <- FALSE
  d$insecticide_times <- as.integer(ceiling(as.numeric(d$No..of.pesticide.application)))
  d$yield <- d$Yield...t.ha. * 1000 # To kg/ha
  d$residue_prevcrop_used <- ifelse(d$Note %in% c("Straw removed", "Straw burned"), FALSE, TRUE)
  d$previous_crop_burnt <- ifelse(d$Note %in% c("Straw burned"), TRUE, FALSE)
  
  d <- d[,27:ncol(d)]

  carobiner::write_files(meta, d, path = path)
  
}

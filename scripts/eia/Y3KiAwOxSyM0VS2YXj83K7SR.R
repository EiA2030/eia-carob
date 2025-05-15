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
   
   uri <- "Y3KiAwOxSyM0VS2YXj83K7SR"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication= NA,
      authors ="Samar Ataher;Ajit Govind",
      data_institute ="ICARDA",
      title = "Use Case 1.9: Gvt Egypt -  yield gap monitoring and tailored agronomy advisory for irrigated wheat and faba bean.",
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC004",
      usecase_name = 'EG-Irrigated-Gvt',
      activity = 'validation',
      treatment_vars= "seed_rate",
      response_vars= "yield",
      project = 'Excellence in Agronomy',
      data_type = "experiment",
      carob_date="2025-05-15",
      notes= NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Egypt-Government-Validation/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_Egypt-Governement_Validation_raw_2024.xlsx"]
   
   ## Processing data 
   r <- carobiner::read.excel(f, fix_names = TRUE)
   d <- data.frame(
      country= r$country,
      crop = r$crop,
      previous_crop= ifelse(grepl("soya bean|soybeans", tolower(r$previous_crop)), "soybean",
                            ifelse(grepl("green beans", tolower(r$previous_crop)), "green bean", 
                                   ifelse(grepl("seasame", tolower(r$previous_crop)), "sesame",
                                          ifelse(grepl("cane", tolower(r$previous_crop)), "sugarcane",tolower(r$previous_crop))))),
      variety= r$variety,
      on_farm = TRUE ,
      is_survey = FALSE,
      adm1= r$adm1,
      adm2= r$adm2,
      adm3= r$adm3,
      latitude= r$latitude,
      longitude= r$longitude,
      trial_id = r$dataset_id,
      season= r$season,
      treatment= r$treatment,
      seed_rate= r$seeding.rate,
      fertilizer_type= "NPK",
      N_fertilizer= r$N_fertilizer_unit,
      P_fertilizer= 0,
      K_fertilizer= 0,
      yield= r$yield*1000, ## in Kg/ha
      planting_date= as.character(as.Date(r$planting_date)),
      harvest_date=  as.character(as.Date(r$harvest_date)),
      harvest_days= r$harvest_days,
      cropland_used= r$cropland_used,
      plot_area= r$plot_area,
      soil_texture= gsub("clay-loam","clay;loam",tolower(r$soil_texture)),
      soil_pH= r$soil_pH,
      soil_EC= r$soil_EC,
      Total_Eto= r$Total_Eto, ## Evapotranspiration
      irrigation_method= r$irrigation_method,
      irrigation_number= as.integer(r$irrigation_number),
      irrigation_amount= r$irrigation_amount,
      irrigated= TRUE,
      water_productivity= r$WP, ## Kg/m3 ?
      N_NUE= r$N_NUE, ## Nitrogen Use Efficiency
      energy_consumption= r$Irrigation.energy._consumption,
      Gross_income= r$gross.income,
      currency= "USD",
      geo_from_source = TRUE,
      yield_part= r$yield_part
   )
   
   d$irrigation_method <- gsub("Surface- flood", "surface-flooding", d$irrigation_method)
   d$irrigation_method <- gsub("Surface-furrow", "furrow", d$irrigation_method)
   
   carobiner::write_files(meta, d, path=path)
}

#carob_script(path)
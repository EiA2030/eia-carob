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
   uri <- "doi:Nigeria-ATAFI-Validations"
   group <- "eia"
   
   meta <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      authors = "Mary Jane;John Do",
      data_citation="...",
      data_institute = NA ,
      title = NA,
      description = "Nigeria-ATAFI-Validations",
      group = group,
      publication=NA,
      license = 'Some license here...',
      carob_contributor = 'Cedric Ngakou',
      project = 'Excellence in Agronomy; Nigeria-ATAFI-Validations',
      data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
      carob_date="2024-08-08",
      treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
      response_vars= "yield_moisture"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Nigeria-ATAFI-Validations/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "VAT_data_ATAFI use case.xlsx"]
   # Read relevant file
   r <- carobiner::read.excel.hdr(f,skip=0,na=c("n/a",NA))
   
   d <- data.frame(
      country= r$Country.name,
      adm1= r$First.level.administrative.unit,
      #year= r$Year,
      season= r$Season,
      latitude= r$Field.location.latitude,
      longitude= r$Field.location.longitude,
      land_prep_method= r$landPreparation_clearing,
      #r$number_Tillage,
      #r$number_Harrow,
      weeding_times= as.integer(r$number_Weeding),
      #production_sys= r$Production_System,
      variety= r$Variety.used,
      rep= as.integer(r$Replicate),
      plot_area= as.numeric(substr(r$planting_Density,1,3))**2,# m2
      planting_method= r$planting_Method,
      planting_date= as.character(r$sowing_date),
      transplanting_date= as.character(r$transplanting_Date),
      harvest_date= as.character(r$harvest_Date),
      #fert= r$Type.of.NPK.fertilizer.used,
      fertilizer_type= r$Nitrogenous.fertilizer.used,
      N_fertilizer=  r$N.applied.kg.ha,
      P_fertilizer= r$P.applied.kg.ha,
      K_fertilizer= r$Kapplied.kg.ha,
      fertilizer_price= as.character(r$Total.fertilizer.cost.USD.ha),
      yield_moisture= r$yield.at.14pct.moisture.content.kg.ha,
      NUE_fertilizer= r$NUE.kg.kg, # N Use Efficiency (kg/kg N ) = grain yield/N_fertilizer application rate
      PUE_fertilizer= r$PUE.kg.kg, # P Use Efficiency ( kg/kg P) 
      KUE_fertilizer= r$KUE.kg.kg, # K Use Efficiency (kg/kg K)
      Gross_revenue =r$Gross.revenu.USD.ha,# Gross.revenue.on.fertilizer.cost.USD.ha # Gross revenue on fertilizer cost = Gross revenue – total fertilizer cost
      currency= "USD",
      trial_id= r$Activity.name,
      treatment= r$Experimental.treatment.name
   )
   
   d$fertilizer_type <- paste0(d$fertilizer_type,";","NPK")
   d$crop <- "rice"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- TRUE
   d$yield_part <- "grain"
   d$inoculated <- FALSE
   d$fertilizer_used <- TRUE
   d$geo_from_source <- TRUE
   d$herbicide_used <- TRUE
   
   ## Fixing longitude and latitude 
   
   geo <- data.frame(
      adm1= c("Nasarawa", "Bama", "Tintimba", "Maninkoura", "Sokona", "Siranikélé", "Figuiracoro", "Baturu", "Lafiabougou", "Kangaré",
              "Dalabala", "Lenguétou", "Kondjiguila", "Carrière", "Sanancoroni", "Sélinkégny", "Dalaba", "Faraba Coura"),
      lat=c(8.4387868, 12.0388818, NA, NA, 12.5930941, 14.132, 11.9392877, NA, 12.6372638, 11.5844678, 11.6416998, NA, 11.6362747, 11.6208743,
            NA, 14.0959426, 13.0277401 , 10.5992276),
      lon=c(8.2382849, -4.4025334, NA, NA, -8.0257987, -7.0543, -8.2614852, NA, -8.0425675, -8.1608772, -8.1799051, NA, -8.2433188, -8.1902498,
            NA, -10.7818534, -7.6001249, -7.9327807)
   )
   
   ## Tintimba,Maninkoura,Baturu,Lenguétou, Sanancoroni are unknown location and we have  set longitude and latitude of these location to NA 
   
   d <- merge(d,geo,by="adm1",all.x= TRUE)
   
   d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
   d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
   
   d$lon <- d$lat <- NULL 
   
   carobiner::write_files(meta, d, path=path)
   
}

#carob_script(path)


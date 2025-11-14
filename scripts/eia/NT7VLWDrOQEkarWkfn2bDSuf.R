# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
#    "
# 	SOME DESCRIPTION GOES HERE...
# "
# 
#    uri <- "NT7VLWDrOQEkarWkfn2bDSuf"
#    group <- "eia"
# 
#    meta <- data.frame(
#       # Need to fill-in metadata...
#       uri = uri,
#       dataset_id = uri,
#       authors = "Ali Ibrahim; Saito Kazuki",
#       data_institute = "AfricaRice",
#       title = NA,
#       #description ="Nigeria-ATAFI-Validations",
#       license = NA,
#       group = group,
#       publication = NA,
#       usecase_code = "USC001",
#       usecase_name = "WA-Rice-ATAFI/MOVE",
#       activity = 'validation',
#       carob_contributor = 'Cedric Ngakou',
#       project = 'Excellence in Agronomy',
#       data_type = "on-farm experiment",
#       carob_date = "2024-08-08",
#       treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
#       response_vars = "yield_moisture",
#       modified_by = "Eduardo Garcia Bendito",
#       last_modified = "2025-01-21"
#    )
# 
#    # Manually build path (this can be automated...)
#    ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-ATAFI-Validations/", full.names = T))
# 
#    # Retrieve relevant file
#    f <- ff[basename(ff) == "VAT_data_ATAFI use case.xlsx"]
#    # Read relevant file
#    r <- carobiner::read.excel.hdr(f, skip=0, na=c("n/a", NA))
# 
#    d <- data.frame(
#       country = r$Country.name,
#       adm1 = r$First.level.administrative.unit,
#       #year= r$Year,
#       season = r$Season,
#       latitude = r$Field.location.latitude,
#       longitude = r$Field.location.longitude,
#       geo_from_source = TRUE,
#       land_prep_method = r$landPreparation_clearing,
#       #r$number_Tillage,
#       #r$number_Harrow,
#       weeding_times = as.integer(r$number_Weeding),
#       variety = r$Variety.used,
#       rep = as.integer(r$Replicate),
#       row_spacing = as.numeric(substr(r$planting_Density,1,3))*100,# cm
#       plant_spacing = as.numeric(substr(r$planting_Density,1,3))*100,# cm
#       planting_method = r$planting_Method,
#       planting_date = as.character(r$sowing_date),
#       transplanting_date = as.character(r$transplanting_Date),
#       harvest_date = as.character(r$harvest_Date),
#       fertilizer_type = paste(r$Nitrogenous.fertilizer.used, gsub(" .*", "", r$Type.of.NPK.fertilizer.used), sep = ";"),
#       N_fertilizer =  r$N.applied.kg.ha,
#       P_fertilizer = r$P.applied.kg.ha,
#       K_fertilizer = r$Kapplied.kg.ha,
#       yield = r$yield.at.14pct.moisture.content.kg.ha,
#       yield_moisture = 14,
#       # NUE_fertilizer = r$NUE.kg.kg, # N Use Efficiency (kg/kg N ) = grain yield/N_fertilizer application rate
#       # PUE_fertilizer = r$PUE.kg.kg, # P Use Efficiency ( kg/kg P)
#       # KUE_fertilizer = r$KUE.kg.kg, # K Use Efficiency (kg/kg K)
#       crop_price = (r$Gross.revenu.USD.ha) / (r$yield.at.14pct.moisture.content.kg.ha), # Converting gross revenue to standardized crop price (USD/kg)
#       currency = "USD",
#       trial_id = r$HHID,
#       treatment = r$Experimental.treatment.name
#    )
# 
#    d$fertilizer_amount = ((d$P_fertilizer * 2.29) / (as.integer(gsub("^[^-]*-", "\\1", gsub("^([^-]*-[^-]*)-.*", "\\1", r$Type.of.NPK.fertilizer.used))) / 100) + (d$N_fertilizer - (d$P_fertilizer * 2.29) / (as.integer(gsub("^[^-]*-", "\\1", gsub("^([^-]*-[^-]*)-.*", "\\1", r$Type.of.NPK.fertilizer.used))) / 100) * (as.integer(gsub("NPK ", "", gsub("^([^-]*)-.*", "\\1", r$Type.of.NPK.fertilizer.used))) / 100)) / 0.46)
#    d$fertilizer_price = (r$Total.fertilizer.cost.USD.ha / d$fertilizer_amount)
# 
#    d$crop <- "rice"
#    d$on_farm <- TRUE
#    d$is_survey <- FALSE
#    d$irrigated <- TRUE
#    d$yield_part <- "grain"
#    d$inoculated <- FALSE
#    d$fertilizer_used <- TRUE
#    d$geo_from_source <- TRUE
#    d$herbicide_used <- TRUE
# 
#    ## Fixing longitude and latitude
# 
#    geo <- data.frame(
#       adm1= c("Nasarawa", "Bama", "Tintimba", "Maninkoura", "Sokona", "Siranikélé", "Figuiracoro", "Baturu", "Lafiabougou", "Kangaré",
#               "Dalabala", "Lenguétou", "Kondjiguila", "Carrière", "Sanancoroni", "Sélinkégny", "Dalaba", "Faraba Coura"),
#       lat=c(8.4387868, 12.0388818, NA, NA, 12.5930941, 14.132, 11.9392877, NA, 12.6372638, 11.5844678, 11.6416998, NA, 11.6362747, 11.6208743,
#             NA, 14.0959426, 13.0277401 , 10.5992276),
#       lon=c(8.2382849, -4.4025334, NA, NA, -8.0257987, -7.0543, -8.2614852, NA, -8.0425675, -8.1608772, -8.1799051, NA, -8.2433188, -8.1902498,
#             NA, -10.7818534, -7.6001249, -7.9327807)
#    )
# 
#    ## Tintimba,Maninkoura,Baturu,Lenguétou, Sanancoroni are unknown locations. We have set longitude and latitude of theses locations to NA
# 
#    d <- merge(d,geo,by="adm1",all.x= TRUE)
# 
#    d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
#    d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
# 
#    d$lon <- d$lat <- NULL
# 
#    carobiner::write_files(meta, d, path=path)
   
   cat("A test!")
   
}

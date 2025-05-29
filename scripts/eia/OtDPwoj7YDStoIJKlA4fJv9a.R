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
   
   uri <- "OtDPwoj7YDStoIJKlA4fJv9a"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication= NA,
      authors ="Mina Devkota; Mohcin Elmarzak",
      data_institute ="ICARDA",
      title = "Gvt Morocco - mechanized conservation agriculture at scale in rainfed wheat systems",
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC017",
      usecase_name = 'MO-Wheat-Gvt',
      activity = 'validation',
      treatment_vars= "N_fertilizer; P_fertilizer; K_fertilizer",
      response_vars= "fw_yield; fwy_residue",
      project = 'Excellence in Agronomy',
      data_type = "experiment",
      carob_date="2025-05-29",
      notes= NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Morocco-Government-Validation/", full.names = T))
   
   # Retrieve relevant file
   ff <- ff[grepl("xlsx", basename(ff))]
   
   ## Processing data 
   proc <- function(f){
      r <- carobiner::read.excel(f, fix_names = TRUE, na=("-"))
      if(is.null(r$ID.Site)) r$ID.Site <- paste0("Mo",1:nrow(r))
      
      data.frame(
         trial_id= paste0(r$ID.Site,"-", r$Name_Farm),
         country= r$country,
         adm1= tolower(r$adm1),
         adm2= tolower(r$adm2),
         adm3= tolower(r$adm3),
         longitude= r$Coord.Y,
         latitude= r$Coord.X,
         soil_pH= r$pH,
         soil_SOM= r$MO,
         soil_P_available= r$P2O5/2.29,
         soil_K= r$K2O/1.2051,
         soil_EC= r$Ec,
         treatment= r$Practice,
         crop= tolower(gsub("DW|SW", "wheat", r$Crop)),
         plot_area= r$Area._ha,
         irrigated= ifelse(grepl("Rainfed", r$statut_hydric), TRUE, FALSE),
         previous_crop= tolower(gsub("DW|SW", "wheat", r$Prev_crop)),
         rain= r$Total.Rainfall.mm,
         N_fertilizer= r$Ferti_UF_N,
         P_fertilizer= r$Ferti_UF_P205/2.29,
         K_fertilizer= r$Ferti_UF_K2O/1.2051,
         seed_rate= r$Rate_sowing_kg.ha,
         plant_density= r$Density_.Plante.m.Grow*10000,#plants/ha # Need to be check
         fwy_residue= r$Y_Bio_Kg.ha.Maturité,
         fw_yield= r$Y_Grain_.Kg.ha.Récolte,
         planting_date= paste0((r$year)-1, "-11"),
         harvest_date= paste0(r$year, "-06"),
         geo_from_source = TRUE,
         herbicide_used= TRUE,
         herbicide_dates= paste0(r$year, "-03-15"), ## mid March
         yield_part= "grain"
      )
      
   }
   
   d <- lapply(ff, proc)
   d <- do.call(rbind, d)
   
   
   carobiner::write_files(meta, d, path=path)
}

#carob_script(path)
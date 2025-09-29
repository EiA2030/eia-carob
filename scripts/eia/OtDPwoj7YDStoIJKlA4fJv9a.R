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
      data_organization = "ICARDA",
      title = "Gvt Morocco - mechanized conservation agriculture at scale in rainfed wheat systems",
      description = "EiA CA results collected during last years in Morocco CA site used in the validation phase. The study was done in 40 sites with different soil and climate conditions. In bellow, we present the protocol of Data and soil collection",
      data_citation = NA,
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC017",
      usecase_name = 'MO-Wheat-Gvt',
      activity = 'validation',
      treatment_vars= "N_fertilizer; P_fertilizer; K_fertilizer",
      response_vars= "fw_yield;fwy_residue",
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
         soil_pH= r$pH,
         soil_SOM= r$MO,
         soil_P = r$P2O5/2.29,
         soil_K= r$K2O/1.2051,
         soil_EC= r$Ec,
         treatment= r$Practice,
         crop = ifelse(r$Crop == "DW", "durum wheat", "wheat"),
         plot_area= r$Area._ha,
         previous_crop = ifelse(r$Prev_crop == "DW", "durum wheat",
                                ifelse(r$Prev_crop == "SW", "wheat", tolower(r$Prev_crop))),
         irrigated= ifelse(grepl("Rainfed", r$statut_hydric), TRUE, FALSE),
         rain= r$Total.Rainfall.mm,
         N_fertilizer= r$Ferti_UF_N,
         P_fertilizer= r$Ferti_UF_P205/2.29,
         K_fertilizer= r$Ferti_UF_K2O/1.2051,
         seed_rate= r$Rate_sowing_kg.ha,
         plant_density= r$Density_.Plante.m.Grow*10000,#plants/ha # Need to be check
         land_prep_method = ifelse(r$Practice == "NT", "none", "conventional"),
         fwy_residue= r$Y_Bio_Kg.ha.Maturité,
         yield = r$Y_Grain_.Kg.ha.Récolte * 0.87, # Assuming 13% moisture
         yield_moisture = 13,
         yield_part= "grain",
         planting_date = as.Date(paste0((r$year)-1, "-11-01")),
         harvest_date = as.Date(paste0(r$year, "-06-01")),
         geo_from_source = TRUE,
         herbicide_used= TRUE,
         herbicide_dates= paste0(r$year, "-03-15"), ## mid March
         on_farm= TRUE, 
         is_survey= FALSE
      )
      
   }
   
   d <- lapply(ff, proc)
   d <- do.call(rbind, d)
   
   # Align locations to GADM
   locs <- data.frame(
     trial_id = c("Mo1-Jalil", "Mo2-Jalil", "Mo3-Benmchiche", 
                  "Mo4-Benmchiche", "Mo5-Daoudi", "Mo6-Daoudi", "Mo7-Sahel", "Mo8-Sahel", 
                  "Mo9-Benchrifa", "Mo10-Benchrifa", "Mo11-Nouini A", "Mo12-Nouini A", 
                  "Mo13-Zahraoui", "Mo14-Zahraoui", "Mo15-Abdellah", "Mo16-Abdellah", 
                  "Mo17-Adil", "Mo18-Adil", "Mo19-Ahmina", "Mo20-Ahmina", "Mo21-AhminaM", 
                  "Mo22-AhminaM", "Mo23-Elghali", "Mo24-Elghali", "Mo25-Mustapha", 
                  "Mo26-Mustapha", "Mo27-Friha", "Mo28-Friha", "Mo29-Dahou", "Mo30-Dahou", 
                  "Mo31-Belfkih", "Mo32-Belfkih", "Mo33-Fatima", "Mo34-Fatima", 
                  "Mo35-Belfellah", "Mo36-Belfellah", "Mo37-Tabeut", "Mo38-Tabeut", 
                  "Mo39-Zerouali", "Mo40-Zerouali", "Mo1-MACHHOUB", "Mo2-MACHHOUB", 
                  "Mo3-MOHAMED", "Mo4-MOHAMED", "Mo5-AHMED", "Mo6-AHMED", "Mo7-ABDELLAH", 
                  "Mo8-ABDELLAH", "Mo9-JAMAL", "Mo10-JAMAL", "Mo11-MOHCINE", "Mo12-MOHCINE", 
                  "Mo13-ABDESELAM", "Mo14-ABDESELAM", "Mo15-THAMI", "Mo16-THAMI"),
     longitude = c(-7.81823, -7.818032, -7.817845, -7.817723, 
                   -5.657799, -5.657778, -7.562276, -7.562544, -5.518362, -5.518078, 
                   -6.486, -6.485, -6.512967, -6.5128, -6.513738, -6.5139, -6.6381, 
                   -6.6381, -6.64564, -6.6458, -6.659737, -6.659737, -6.667417, 
                   -6.6671, -6.559782, -6.5595, -6.585725, -6.5859, -6.594404, -6.5945, 
                   -5.657305, -5.65321, -5.518362, -5.518078, -7.817594, -7.81749, 
                   -7.562589, -7.562599, -5.796, -5.795063, -6.977082, -6.977082, 
                   -4.430681, -4.430681, -4.7154138, -4.7154138, -5.5235, -5.5235, 
                   -5.75282, -5.75282, -7.319549, -7.319549, -5.49661, -5.49661, 
                   -5.6158859, -5.6158859),
     latitude = c(32.776743, 32.776895, 
                  32.777082, 32.777214, 34.349491, 34.348782, 33.075358, 33.07757, 
                  34.550927, 34.550993, 33.56, 33.56, 33.593617, 33.5937, 33.594101, 
                  33.5943, 33.5381, 33.5384, 33.5351, 33.535098, 33.540944, 33.5411, 
                  33.539748, 33.5399, 33.626978, 33.6271, 33.655167, 33.6551, 33.662837, 
                  33.6631, 34.35144, 34.32874, 34.550927, 34.553, 32.777395, 32.777571, 
                  33.076999, 33.076324, 33.868, 33.866646, 32.922613, 32.922613, 
                  34.1343, 34.1343, 34.0042975, 34.0042975, 33.22313, 33.22313, 
                  33.7569, 33.7569, 33.278606, 33.278606, 34.435461, 34.435461, 
                  34.84769, 34.84769),
     country = c("Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", 
                 "Morocco", "Morocco", "Morocco", "Morocco", "Morocco"),
     adm1 = c("Chaouia - Ouardigha", 
              "Chaouia - Ouardigha", "Chaouia - Ouardigha", "Chaouia - Ouardigha", 
              "Gharb - Chrarda - Béni Hssen", "Gharb - Chrarda - Béni Hssen", 
              "Chaouia - Ouardigha", "Chaouia - Ouardigha", "Gharb - Chrarda - Béni Hssen", 
              "Gharb - Chrarda - Béni Hssen", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Gharb - Chrarda - Béni Hssen", 
              "Gharb - Chrarda - Béni Hssen", "Gharb - Chrarda - Béni Hssen", 
              "Gharb - Chrarda - Béni Hssen", "Chaouia - Ouardigha", "Chaouia - Ouardigha", 
              "Chaouia - Ouardigha", "Chaouia - Ouardigha", "Rabat - Salé - Zemmour - Zaer", 
              "Rabat - Salé - Zemmour - Zaer", "Chaouia - Ouardigha", "Chaouia - Ouardigha", 
              "Taza - Al Hoceima - Taounate", "Taza - Al Hoceima - Taounate", 
              "Fès - Boulemane", "Fès - Boulemane", "Meknès - Tafilalet", 
              "Meknès - Tafilalet", "Rabat - Salé - Zemmour - Zaer", "Rabat - Salé - Zemmour - Zaer", 
              "Chaouia - Ouardigha", "Chaouia - Ouardigha", "Gharb - Chrarda - Béni Hssen", 
              "Gharb - Chrarda - Béni Hssen", "Tanger - Tétouan", "Tanger - Tétouan"),
     adm2 = c("Settat", "Settat", "Settat", "Settat", "Sidi Kacem", 
              "Sidi Kacem", "Settat", "Settat", "Sidi Kacem", "Sidi Kacem", 
              "Khémisset", "Khémisset", "Khémisset", "Khémisset", "Khémisset", 
              "Khémisset", "Khémisset", "Khémisset", "Khémisset", "Khémisset", 
              "Khémisset", "Khémisset", "Khémisset", "Khémisset", "Khémisset", 
              "Khémisset", "Khémisset", "Khémisset", "Khémisset", "Khémisset", 
              "Sidi Kacem", "Sidi Kacem", "Sidi Kacem", "Sidi Kacem", "Settat", 
              "Settat", "Settat", "Settat", "Khémisset", "Khémisset", "Settat", 
              "Settat", "Taza", "Taza", "Sefrou", "Sefrou", "Khénifra", "Khénifra", 
              "Khémisset", "Khémisset", "Settat", "Settat", "Sidi Kacem", 
              "Sidi Kacem", "Chefchaouen", "Chefchaouen"),
     adm3 = c("Settat", 
              "Settat", "Settat", "Settat", "Sidi Kacem", "Sidi Kacem", "Settat", 
              "Settat", "Had Kourt", "Had Kourt", "Rommani", "Rommani", "Rommani", 
              "Rommani", "Rommani", "Rommani", "Rommani", "Rommani", "Rommani", 
              "Rommani", "Rommani", "Rommani", "Rommani", "Rommani", "Rommani", 
              "Rommani", "Rommani", "Rommani", "Rommani", "Rommani", "Sidi Kacem", 
              "Sidi Kacem", "Had Kourt", "Had Kourt", "Settat", "Settat", "Settat", 
              "Settat", "Khemisset", "Khemisset", "Ben Ahmed", "Ben Ahmed", 
              "Tahla", "Tahla", "El Menzel", "El Menzel", "Khenifra", "Khenifra", 
              "Khemisset", "Khemisset", "El Gara", "El Gara", "Had Kourt", 
              "Had Kourt", "Moqrissat", "Moqrissat")
   )
   
   d <- merge(d, locs, "trial_id")
   
   #### Fixing previous crop
   d$previous_crop[grepl("fallow", d$previous_crop)] <- "none"
   
   carobiner::write_files(meta, d, path=path)
}

# carob_script(path)

# # Get GADM data
# x <- geodata::gadm("Morocco", level = 3, path = tempfile())
# xx <- st_as_sf(x)
# dput(st_drop_geometry(st_join(st_as_sf(d[,c(7,6,7,6)], coords = c("longitude", "latitude"), crs = 4326), xx))[,c(1,2,5,7,10,12)])

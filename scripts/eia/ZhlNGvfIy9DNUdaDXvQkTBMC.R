# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...


carob_script <- function(path) {
  
#   "
# 	SOME DESCRIPTION GOES HERE...
# "
  
  
#   uri <- "ZhlNGvfIy9DNUdaDXvQkTBMC"
#   group <- "eia"
  
#   meta <- data.frame(
#     # carobiner::read_metadata(uri, path, group, major=2, minor=0),
#     # uri = carobiner::simple_uri(uri),
#     uri = uri,
#     dataset_id = uri,
#     authors = "Christine Kreye",
#     data_institute = "IITA", 
#     title = NA,
#     publication=NA,
#     description = "Validations of the SAA Nigeria Use Case MVP",
#     group = group,
#     license ="none",
#     carob_contributor = 'Eduardo Garcia Bendito;Cedric Ngakou',
#     data_citation = '...',
#     treatment_vars="variety;crop;longitude;latitude" ,
#     response_vars= "yield" ,
#     project = 'Excellence in Agronomy',
#     usecase_code= "USC008",
#     usecase_name = 'NG-Akilimo-SAA',
#     activity = 'validation',
#     data_type = "experiment", 
#     carob_date="2024-05-22",
#     notes= "N_fertilizer,P_fertilizer and K_fertilizer rate are missing in the data"
#   )
  
#   # Manually build path (this can be automated...)
#   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-SAA-Validation", full.names = T))
  
#   # Maize
#   fmc <- ff[basename(ff) == "maizeClean.csv"]
#   fmp <- ff[basename(ff) == "plotDimS.csv"]
#   fmm <- ff[basename(ff) == "grainMoistureM.csv"]
#   fms <- ff[basename(ff) == "scoreHigh.csv"]
#   fmf <- ff[basename(ff) == "NEfert.csv"]
  
#   # Read relevant file(s)
#   rmc <- read.csv(fmc)
#   rmp <- read.csv(fmp)
#   rmm <- read.csv(fmm)
#   rms <- read.csv(fms)
#   rmf <- read.csv(fmf)
  
#   ## Processing maize data
  
#   # maizeClean
#   dmh <- data.frame(
#     trial_id = rmc$HHID,
#     event = rmc$event,
#     adm2 = rmc$lga,
#     year = rmc$Year,
#     country = "Nigeria",
#     plants_SSR = rmc$nrPlants_SSR,
#     plants_BRR = rmc$nrPlants_BRR,
#     plants_ZCC = rmc$nrPlants_ZCC,
#     cobs_SSR = rmc$maizeCobsFW_plot_SSR,
#     cobs_BRR = rmc$maizeCobsFW_plot_BRR,
#     cobs_ZCC = rmc$maizeCobsFW_plot_ZCC,
#     yield_SSR = rmc$maizeGrainFW_plot_SSR,
#     yield_BRR = rmc$maizeGrainFW_plot_BRR,
#     yield_ZCC = rmc$maizeGrainFW_plot_ZCC,
#     harvest_date = rmc$harvestDate
#   )
  
#   # Maize
#   # Event1: Planting
#   # Event2: 1st fertilizer application
#   # Event3: 2nd fertilizer application
#   # Event4: Monitoring flowering
#   # Event5: Harvesting
#   ## Selecting the harvest event
#   dmh <- dmh[dmh$event == "event5M",]
  
#   ## Adding plot size 
#   dmh1 <- rmc[rmc$event == "event1M", c("HHID", "plotL1_BRR", "plotW1_BRR", "plotL1_SSR", "plotW1_SSR", "plotL1_ZCC", "plotW1_ZCC")]
#   dmh1$plot_area_BRR <- (dmh1$plotL1_BRR * dmh1$plotW1_BRR)/10000
#   dmh1$plot_area_SSR <- (dmh1$plotL1_SSR * dmh1$plotW1_SSR)/10000
#   dmh1$plot_area_ZCC <- (dmh1$plotL1_ZCC * dmh1$plotW1_ZCC)/10000
#   dmh1 <- dmh1[,c(1,8,9,10)]
#   colnames(dmh1)[1] <- "trial_id"
#   dmh <- merge(dmh,dmh1, by=c("trial_id"), all.x= TRUE) 
  
#   ## grainMoistureM
  
#   dmh2 <- data.frame(
#     trial_id= rmm$HHID,
#     yield_moisture_BRR= rmm$moistureBRR,
#     yield_moisture_SSR= rmm$moistureSSR,
#     yield_moisture_ZCC= rmm$moistureZCC
#   )
  
#   dmh <- merge(dmh,dmh2, by=c("trial_id"), all.x= TRUE)
  
#   ## Maize fertilizer (NEfert)
  
#   dmh3 <- data.frame(
#     trial_id= rmf$HHID,
#     adm1= rmf$state,
#     longitude =rmf$Longitude,
#     latitude = rmf$Latitude,
#     elevation = rmf$Altitude,
#     geo_uncertainty = rmf$Accuracy,
#     geo_from_source = TRUE,
#     planting_date= rmf$plantingDate,
#     OM_used= ifelse(grepl("yes",rmf$orgApply1) | grepl("yes",rmf$orgApply2),TRUE,FALSE),
#     OM_type = paste0(ifelse(is.na(rmf$amountPoultryManure), "", "poultry manure"), ifelse(is.na(rmf$amountFYM), "", "farmyard manure")),
#     OM_amount = as.numeric(paste0(ifelse(is.na(rmf$amountPoultryManure), "", rmf$amountPoultryManure), ifelse(is.na(rmf$amountFYM), "", rmf$amountFYM))),
#     fertilizer_price= as.character(rowSums(rmf[,c("costUrea","costNPK")], na.rm = TRUE)),
#     crop_price = rmf$priceProduce,
#     currency = "NGN",
#     variety= rmf$variety,
#     row_spacing= substr(rmf$plantingDensity,1,4),
#     plant_spacing= substr(rmf$plantingDensity,8,11),
#     cropland_total = rmf$sizeFarm/10000,
#     # fertilizer_amount_BRR= rmf$NPKBRR + rmf$plotUreaSplit1BRR+ rmf$plotUreaSplit2BRR,
#     # fert= rmf$plotNPKBRR_BE + rmf$plotUreaBRR_BE,
#     # fertilizer_type_BRR= "NPK;urea"
#     npk_SSR = rmf$NPK,
#     npk_BRR = rowSums(rmf[,c("NPKBRR","NPKBRR_BE")], na.rm = TRUE),
#     npk_ZCC = 0,
#     urea_SSR = rmf$ureaTotal,
#     urea_BRR = rowSums(rmf[,c("ureaBRR","ureaBRR_BE")], na.rm = TRUE),
#     urea_ZCC = 0
#   )
  
#   # dmh3$fertilizer_amount_BRR[is.na(dmh3$fertilizer_amount_BRR)] <- dmh3$fert[is.na(dmh3$fertilizer_amount_BRR)]
#   dmh3$adm1[grep("BE",dmh3$adm1)] <- "Benue"
#   dmh3$adm1[grep("KD",dmh3$adm1)] <- "Kaduna"
#   dmh3$adm1[grep("KA",dmh3$adm1)] <- "Kano"
  
#   dm <- merge(dmh,dmh3, by=c("trial_id"), all.x= TRUE)
  
#   is.na(dm$OM_type) <- dm$OM_type == ""
  
#   dm <- reshape(dm, direction="long",
#                 varying=c("plants_SSR", "cobs_SSR", "yield_SSR", "plot_area_SSR", "yield_moisture_SSR", "npk_SSR", "urea_SSR",
#                           "plants_BRR", "cobs_BRR", "yield_BRR", "plot_area_BRR", "yield_moisture_BRR", "npk_BRR", "urea_BRR",
#                           "plants_ZCC", "cobs_ZCC", "yield_ZCC", "plot_area_ZCC", "yield_moisture_ZCC", "npk_ZCC", "urea_ZCC"),
#                 timevar="treatment",
#                 times= c("SSR", "BRR", "ZCC"),
#                 v.names= c("a", "b", "c", "d", "e", "f", "g"))
#   colnames(dm)[25:31] <- c("plants", "cobs", "yield", "plot_area", "yield_moisture", "npk", "urea")
#   dm$id <- NULL
  
#   # EGB:
#   # # Assuming NPK 15-15-15 (?)
#   dm$fertilizer_type <- ifelse(dm$treatment == "ZCC", NA, "urea; NPK")
#   dm$fertilizer_amount <- rowSums(cbind(dm$npk, dm$urea), na.rm = TRUE)
#   dm$N_fertilizer <- rowSums(cbind(dm$npk*0.15, dm$urea*0.46), na.rm = TRUE)
#   dm$P_fertilizer <- dm$npk*0.15
#   dm$K_fertilizer <- dm$npk*0.15
  
#   dm$yield <- dm$yield/dm$plot_area
#   dm$crop <- "maize"
#   dm$yield_part <- "seed"
#   dm$plant_density[!is.na(dm$plot_area)] <- (dm$plants[!is.na(dm$plot_area)]/dm$plot_area[!is.na(dm$plot_area)])
#   dm$cob_density[!is.na(dm$plot_area)] <- (dm$cobs[!is.na(dm$plot_area)]/dm$plot_area[!is.na(dm$plot_area)])
#   dm$planting_date <- as.character(as.Date(dm$planting_date, "%b %d, %Y"))
#   dm$harvest_date <- as.character(as.Date(dm$harvest_date, "%b %d, %Y"))
  
#   dm$cobs <- dm$plants <- dm$npk <- dm$urea <- dm$year <- dm$event <- NULL
  
#   ## Processing rice data
  
#   frc <- ff[basename(ff) == "riceClean.csv"]
#   frm <- ff[basename(ff) == "grainMoistureR.csv"]
#   frs <- ff[basename(ff) == "dRed.csv"]
#   frf <- ff[basename(ff) == "Ralfert.csv"]
  
#   ### read()
#   rrc <- read.csv(frc)
#   rrm <- read.csv(frm)
#   rrs <- read.csv(frs)
#   rrf <- read.csv(frf)
  
#   ## riceClean file 
  
#   drh <- data.frame(
#     trial_id= rrc$HHID,
#     event= rrc$event,
#     adm1 = carobiner::fix_name(rrc$stateEA, case = "first"),
#     adm2 = rrc$lga,
#     year= rrc$Year,
#     country= "Nigeria",
#     yield_SSR= rrc$riceGrainFW_plot_SSR,
#     yield_BRR= rrc$riceGrainFW_plot_BRR,
#     yield_ZCC= rrc$riceGrainFW_plot_ZCC,
#     harvest_date = rrc$harvestDate
#   )
  
#   # Rice
#   # Event1: Planting
#   # Event2: 1st fertilizer application
#   # Event3: 2nd fertilizer application
#   # Event4: 3rd fertilizer application
#   # Event5: Monitoring heading
#   # Event6: Harvesting
#   # Event7: Crop cut in the surrunding field
  
#   ## Selecting the harvest event
#   drh <- drh[drh$event == "event6R",]
  
#   ## Adding plot size 
#   drh1 <- rrc[rrc$event == "event1R", c("HHID", "plotL1_BRR", "plotW1_BRR", "plotL1_SSR", "plotW1_SSR", "plotL1_ZCC", "plotW1_ZCC", "riceSystem")]
#   drh1$plot_area_BRR <- (drh1$plotL1_BRR * drh1$plotW1_BRR)/10000
#   drh1$plot_area_SSR <- (drh1$plotL1_SSR * drh1$plotW1_SSR)/10000
#   drh1$plot_area_ZCC <- (drh1$plotL1_ZCC * drh1$plotW1_ZCC)/10000
#   drh1$farming_system <- ifelse(drh1$riceSystem == "rainfedLowland", "rainfed lowland", ifelse(drh1$riceSystem == "irrigated", "irrigated", ifelse(drh1$riceSystem == "", NA, NA)))
#   drh1 <- drh1[,c(1,9:12)]
#   colnames(drh1)[1] <- "trial_id"
#   drh <- merge(drh,drh1, by=c("trial_id"), all.x= TRUE)
#   # drh$plot_area_BRR <- ifelse(is.na(drh$plot_area_BRR), 0.0100, drh$plot_area_BRR)
#   # drh$plot_area_SSR <- ifelse(is.na(drh$plot_area_SSR), 0.0100, drh$plot_area_SSR)
#   # drh$plot_area_ZCC <- ifelse(is.na(drh$plot_area_ZCC), 0.0100, drh$plot_area_ZCC)
  
#   ## grainMoistureR
  
#   drh1 <- data.frame(
#     trial_id= rrm$HHID,
#     yield_moisture_BRR= rrm$moistureBRR,
#     yield_moisture_SSR= rrm$moistureSSR,
#     yield_moisture_ZCC= rrm$moistureZCC
#   )
  
#   drh <- merge(drh,drh1, by=c("trial_id"), all.x= TRUE)
  
#   ## fertilizer file for rice (Ralfert)
  
#   drh2 <- data.frame(
#     trial_id= rrf$HHID,
#     longitude = rrf$Longitude,
#     latitude = rrf$Latitude,
#     elevation= rrf$Altitude,
#     geo_uncertainty = rrf$Accuracy,
#     geo_from_source = TRUE,
#     season = rrf$season,
#     planting_date= rrf$plantingDate,
#     OM_used= ifelse(grepl("yes",rrf$orgApply1)|grepl("yes",rrf$orgApply2),TRUE,FALSE),
#     fertilizer_price= as.character(rowSums(rrf[,c("costUrea","costNPK")])),
#     crop_price = rrf$priceProduce,
#     currency = "NGN",
#     variety= rrf$variety,
#     row_spacing= as.numeric(ifelse(rrf$plantingDensity == "other", 0.5, substr(rrf$plantingDensity,1,4))),
#     plant_spacing= as.numeric(ifelse(rrf$plantingDensity == "other", 0.5, substr(rrf$plantingDensity,8,11))),
#     cropland_total = rrf$sizeFarm/10000,
#     previous_crop_residue_management = trimws(rrf$stubbleManage),
#     npk_SSR = rrf$rateNPK,
#     npk_BRR = rrf$NPKBRR,
#     npk_ZCC = 0,
#     urea_SSR = rrf$rateUrea,
#     urea_BRR = rrf$ureaBRR,
#     urea_ZCC = 0
#   )
  
#   dr <- merge(drh,drh2, by=c("trial_id"), all.x= TRUE)
  
#   is.na(dr$previous_crop_residue_management) <- dr$previous_crop_residue_management == ""
  
#   dr <- reshape(dr, direction="long",
#                 varying=c("yield_SSR", "plot_area_SSR", "yield_moisture_SSR", "npk_SSR", "urea_SSR",
#                           "yield_BRR", "plot_area_BRR", "yield_moisture_BRR", "npk_BRR", "urea_BRR",
#                           "yield_ZCC", "plot_area_ZCC", "yield_moisture_ZCC", "npk_ZCC", "urea_ZCC"),
#                 timevar="treatment",
#                 times= c("SSR", "BRR", "ZCC"),
#                 v.names= c("urea", "plot_area", "yield", "yield_moisture", "npk"))
#   dr$id <- NULL
  
#   # EGB:
#   # # Assuming NPK 15-15-15 (?)
#   dr$fertilizer_type <- ifelse(dr$treatment == "ZCC", NA, "urea; NPK")
#   dr$fertilizer_amount <- rowSums(cbind(dr$npk, dr$urea), na.rm = TRUE)
#   dr$N_fertilizer <- rowSums(cbind(dr$npk*0.15, dr$urea*0.46), na.rm = TRUE)
#   dr$P_fertilizer <- dr$npk*0.15
#   dr$K_fertilizer <- dr$npk*0.15
  
#   dr$yield <- dr$yield/dr$plot_area
#   dr$crop <- "rice"
#   dr$yield_part <- "seed"
#   dr$planting_date <- as.character(as.Date(dr$planting_date, "%b %d, %Y"))
#   dr$harvest_date <- as.character(as.Date(dr$harvest_date, "%b %d, %Y"))
  
#   dr$urea <- dr$npk <- NULL
  
#   ## Combine Maize data and Rice data 
#   d <- carobiner::bindr(dm,dr)
#   d$event <- d$year <- NULL
  
#   ### Adding variables 
#   d$is_survey <- FALSE
#   d$on_farm <- TRUE
#   d$yield_part <- "grain"
#   d$irrigated <- ifelse(d$farming_system == "irrigated", TRUE, FALSE)
#   d$row_spacing <- as.numeric(d$row_spacing)
#   d$plant_spacing <- as.numeric(d$plant_spacing)
  
#   carobiner::write_files(meta, d, path=path)

    cat("This is a test")
  
}

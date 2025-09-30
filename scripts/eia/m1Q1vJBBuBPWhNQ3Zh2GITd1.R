# R script for EiA version of"carob"

## ISSUES
# ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "m1Q1vJBBuBPWhNQ3Zh2GITd1"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Elke Vandamme; Bester Mudereri; Rhys Manners",
    data_organization = "CIP",
    data_citation = NA,
    title = "Use Case 1.2: SNS/RAB - optimized soil fertility enhancement in priority crops.",
    description ="The Excellence in Agronomy (EiA) Initiative has formed a collaboration with RAB and its Rwanda Soil Information System (RwaSIS) project to create a fertilizer recommendation tool for six key crops (cassava, maize, wheat, potato, rice, and beans) that seamlessly integrates with the Smart Nkunganire System (SNS). The SNS is an established digital platform, developed through a public-private partnership involving BKTechouse and RAB, boasting over 1.5 million registered users and currently employed for agro-input supply management.</p>\r\n\r\n<p>The fertilizer recommendation tool will offer improved, location-specific fertilizer recommendations, while being consistently maintained and updated by the Rwanda Agriculture and Animal Resources Board (RAB). Moreover, it will serve as a tangible example to facilitate the future inclusion of various other forms of agronomic advice within the SNS framework.",
    license = NA,
    group = group,
    publication= "hdl:10568/163020",
    usecase_code = "USC002",
    usecase_name = "CA-HighMix-SNS/RAB",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "on-farm experiment", 
    carob_date="2024-09-27",
    treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
    response_vars= "yield_moisture",
    notes = "Diseases, pest and stress not processed but available in the data"
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Rwanda-RAB-Validation/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "validationData.RDS"]
  # Read relevant file
  r <- readRDS(f)
  
  d <- data.frame(
    country= "Rwanda",
    longitude = as.numeric(r$intro._geopoint_household_longitude),
    latitude = as.numeric(r$intro._geopoint_household_latitude),
    elevation = as.numeric(r$intro._geopoint_household_altitude),
    geo_uncertainty = as.numeric(r$intro._geopoint_household_precision),
    geo_from_source = TRUE,
    trial_id = r$intro.barcodehousehold,
    treatment = NA,
    irrigated = ifelse(r$LandPreparation.landPreparation.irrigation_technique == "rainfed", FALSE, TRUE),
    on_farm = TRUE,
    is_survey = FALSE,
    package = r$packageNr,
    crop = ifelse(tolower(r$crop) == "potatoirish", "potato", ifelse(tolower(r$crop) == "beansbush", "common bean", tolower(r$crop))),
    variety = ifelse(r$planting.plantingDetails.variety == "n/a", NA, r$planting.plantingDetails.variety),
    previous_crop = ifelse(r$LandPreparation.landPreparation.previous_crop == "n/a", NA,
                           ifelse(r$LandPreparation.landPreparation.previous_crop %in% c("beansClimbing", "beansBush"), "common bean",
                                  ifelse(r$LandPreparation.landPreparation.previous_crop == "peas", "pea",
                                         ifelse(r$LandPreparation.landPreparation.previous_crop == "carrots", "carrot",
                                                ifelse(r$LandPreparation.landPreparation.previous_crop == "potatoIrish", "potato",
                                                       ifelse(r$LandPreparation.landPreparation.previous_crop == "passionfruit", "passion fruit",
                                                              ifelse(r$LandPreparation.landPreparation.previous_crop == "potatoSweet", "sweetpotato", NA))))))),
    residue_prevcrop_used = ifelse(r$LandPreparation.landPreparation.residue_management.removal %in% c("n/a", "TRUE"), FALSE, TRUE),
    previous_crop_residue_management = gsub("; ", "", paste0(ifelse(r$LandPreparation.landPreparation.residue_management.incorporation == TRUE, "incorporation", ""),
                                                             ifelse(r$LandPreparation.landPreparation.residue_management.spreading == TRUE, "spreading", ""),
                                                             ifelse(r$LandPreparation.landPreparation.residue_management.left_for_grazing == TRUE, "grazing", ""),
                                                             sep = "; ")),
    previous_crop_burnt = as.logical(ifelse(r$LandPreparation.landPreparation.residue_management.burning == "n/a", NA, r$LandPreparation.landPreparation.residue_management.burning)),
    previous_crop_residue_perc = as.numeric(ifelse(r$LandPreparation.landPreparation.residue_remaining == "0", 0,
                                                   ifelse(r$LandPreparation.landPreparation.residue_remaining == "lessThan50", 50,
                                                          ifelse(r$LandPreparation.landPreparation.residue_remaining == "moreThan50", 75,
                                                                 ifelse(r$LandPreparation.landPreparation.residue_remaining == "all100", 100, NA))))),
    fertilizer_used_previous_season = ifelse(r$LandPreparation.landPreparation.inorganic_fertilizer_previous_season == "yes", TRUE,
                                             ifelse(r$LandPreparation.landPreparation.inorganic_fertilizer_previous_season == "no", FALSE, NA)),
    OM_used_previous_season = ifelse(r$LandPreparation.landPreparation.organic_fertilizer_previous_season == "yes", TRUE,
                                     ifelse(r$LandPreparation.landPreparation.organic_fertilizer_previous_season == "no", FALSE, NA)),
    # EGB:
    # # Not sure if "manual" == "hoeing"
    land_prep_method = gsub("manual", "hoeing",gsub("n/a", "", gsub("; n/a", "", gsub("n/a; ", "", paste(r$LandPreparation.landPrepationDetails.tillage_technique_1,
                                                                                                         r$LandPreparation.landPrepationDetails.tillage_technique_2,
                                                                                                         r$LandPreparation.landPrepationDetails.tillage_technique_3, sep = "; "))))),
    planting_date = as.character(as.Date(r$planting.plantingDetails.planting_date, "%m/%d/%Y")),
    # EGB:
    # # Need to convert to plants/ha (How to convert seed/ha to plant/ha or kg/ha to plant/ha?)
    seed_density = ifelse(r$planting.plantingDetails.plant_density_unit == "plants/m2",
                          as.numeric(r$planting.plantingDetails.plant_density)*10000, NA),
    planting_method = ifelse(r$planting.plantingDetails.planting_technique %in% c("line_sowing", "line_planting"), "line sowing",
                             ifelse(r$planting.plantingDetails.planting_technique == "n/a", NA, r$planting.plantingDetails.planting_technique)),
    row_spacing = as.numeric(r$planting.densityDetails.row_spacing)*100,
    plant_spacing = as.numeric(r$planting.densityDetails.plant_spacing)*100,
    OM_used = ifelse(r$fertilizer_org_used == "yes", TRUE,
                     ifelse(r$fertilizer_org_used == "no", FALSE, NA)),
    OM_type = gsub("; ", "", paste(ifelse(r$organicInputsDetails.fertilizer_org_name.compost == "TRUE", "compost", ""),
                                   ifelse(r$organicInputsDetails.fertilizer_org_name.manure == "TRUE", "farmyard manure", ""),
                                   ifelse(r$organicInputsDetails.fertilizer_org_name.crop_residues == "TRUE", "leaf litter", ""), sep = "; ")),
    OM_date = as.character(as.Date(r$organicInputsDetails.fertilizer_org_date, "%m/%d/%Y")),
    plot_area.BR = as.numeric(r$plotDescription.plotLayout_BR.plot_area_control),
    plot_area.AEZ = as.numeric(r$plotDescription.plotLayout_AEZ.plot_area_aez),
    plot_area.SSR = as.numeric(r$plotDescription.plotLayout_SSR.plot_area_ssr),
    plot_area.Lime = as.numeric(r$plotDescription.plotLayout_Lime.plot_area_Lime),
    weeding_done = ifelse(as.integer(r$cropManagement.weeding.weeding_number) > 0, TRUE, FALSE),
    weeding_times = as.integer(r$cropManagement.weeding.weeding_number),
    weeding_dates = gsub("; $", "", gsub("; ; ", "", paste(ifelse(r$cropManagement.weeding.weeding_details.weeding_date_1 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_1, "%m/%d/%Y")), ""),
                                                           ifelse(r$cropManagement.weeding.weeding_details.weeding_date_2 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_2, "%m/%d/%Y")), ""),
                                                           ifelse(r$cropManagement.weeding.weeding_details.weeding_date_3 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_3, "%m/%d/%Y")), ""), sep = "; "))),
    weeding_method = gsub("manual_weeding", "manual", gsub("; $", "", gsub("; ; ", "", paste(ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_1 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_1, ""),
                                                                                             ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_2 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_2, ""),
                                                                                             ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_3 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_3, ""), sep = "; ")))),
    # EGB:
    # # Pest, disease and stresses are available in the data, but not processed here...
    harvest_date = as.character(as.Date(r$cropManagement.Harvest.harvest.harvest_date, "%m/%d/%Y")),
    fw_yield.potato.BR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_BR) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_BR),
    yield_marketable.potato.BR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_BR),
    yield_part.potato.BR = "tubers",
    fw_yield.potato.AEZ = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR1) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR1),
    yield_marketable.potato.AEZ = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR1),
    yield_part.potato.AEZ = "tubers",
    fw_yield.potato.SSR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR2) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR2),
    yield_marketable.potato.SSR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR2),
    yield_part.potato.SSR = "tubers",
    fw_yield.potato.Lime = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_Lime) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_Lime),
    yield_marketable.potato.Lime = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_Lime),
    yield_part.potato.Lime = "tubers",
    fw_yield.rice.BR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_BR),
    moisture_yield.rice.BR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_BR),
    yield_part.rice.BR = "grain",
    fw_yield.rice.AEZ = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR1),
    moisture_yield.rice.AEZ = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR1),
    yield_part.rice.AEZ = "grain",
    fw_yield.rice.SSR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR2),
    moisture_yield.rice.SSR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR2),
    yield_part.rice.SSR = "grain",
    fw_yield.rice.Lime = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_Lime),
    moisture_yield.rice.Lime = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_Lime),
    yield_part.rice.Lime = "grain",
    # fw_yield.maize.BR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_BR),
    # moisture_yield.maize.BR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_BR),
    # fw_yield.maize.AEZ = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_SSR1),
    # moisture_yield.maize.AEZ = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_SSR1),
    # fw_yield.maize.SSR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_SSR2),
    # moisture_yield.maize.SSR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_SSR2),
    # fw_yield.maize.Lime = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_Lime),
    # moisture_yield.maize.Lime = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_Lime),
    # fw_yield.bean.BR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_BR),
    # moisture_yield.bean.BR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_BR),
    # yield_part.bean.BR = "seed",
    # fw_yield.bean.AEZ = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_SSR1),
    # moisture_yield.bean.AEZ = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_SSR1),
    # yield_part.bean.AEZ = "seed",
    # fw_yield.bean.SSR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_SSR2),
    # moisture_yield.bean.SSR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_SSR2),
    # yield_part.bean.SSR = "seed",
    # fw_yield.bean.Lime = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_Lime),
    # moisture_yield.bean.Lime = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_Lime),
    # yield_part.bean.Lime = "seed",
    crop_price = as.numeric(r$cropManagement.product_price),
    currency = r$intro.local_currency
  )
  
  d$OM_type[grep("^\\s*$", d$OM_type, value = FALSE)] <- NA
  
  # Process treatments to long
  d1 <- reshape(d, direction="long",
                varying=c("fw_yield.rice.BR", "fw_yield.potato.BR", "moisture_yield.rice.BR", "yield_part.rice.BR", "yield_marketable.potato.BR", "yield_part.potato.BR", "plot_area.BR",
                          "fw_yield.rice.AEZ", "fw_yield.potato.AEZ", "moisture_yield.rice.AEZ", "yield_part.rice.AEZ", "yield_marketable.potato.AEZ", "yield_part.potato.AEZ", "plot_area.AEZ",
                          "fw_yield.rice.SSR", "fw_yield.potato.SSR", "moisture_yield.rice.SSR", "yield_part.rice.SSR", "yield_marketable.potato.SSR", "yield_part.potato.SSR", "plot_area.SSR",
                          "fw_yield.rice.Lime", "fw_yield.potato.Lime", "moisture_yield.rice.Lime", "yield_part.rice.Lime", "yield_marketable.potato.Lime", "yield_part.potato.Lime", "plot_area.Lime"),
                timevar="treatment",
                times= c("BR", "AEZ", "SSR", "Lime"),
                v.names= c("plot_area",
                           "yield_part.rice",
                           "moisture_yield.rice",
                           "fw_yield.rice",
                           "yield_part.potato",
                           "yield_marketable.potato",
                           "fw_yield.potato"))
  
  # Process crop to long
  d2 <- reshape(d1[,c(1:length(d1)-1)], direction = "long",
                varying = c("fw_yield.potato", "yield_part.potato", 
                            "fw_yield.rice", "yield_part.rice"),
                timevar = "crop",
                times = c("potato", "rice"),
                v.names = c("yield_part",
                            "fw_yield"))
  
  # Fix column names
  d2$yield_marketable <- d2$yield_marketable.potato
  d2$yield_marketable.potato <- NULL
  d2$yield_moisture <- d2$moisture_yield.rice
  d2$moisture_yield.rice <- NULL
  d2$id <- NULL
  
  # Fix future dates error in weeding dates
  d2$weeding_dates <- gsub("2024", "2023", d2$weeding_dates)
  
  dp <- unique(d2[!is.na(d2$previous_crop_burnt) & d2$crop == "potato", c(1:8, 13)])
  dr <- unique(d2[!is.na(d2$previous_crop_burnt) & d2$crop == "rice", c(1:8, 13)])
  dp1 <- unique(d2[!is.na(d2$previous_crop_burnt) & d2$crop == "potato", c(7:30)])
  dr1 <- unique(d2[!is.na(d2$previous_crop_burnt) & d2$crop == "rice", c(7:30)])
  dp2 <- unique(d2[!is.na(d2$harvest_date) & d2$crop == "potato" & !is.na(d2$fw_yield), c(7, 8, 13, 31:42)])
  dr2 <- unique(d2[!is.na(d2$harvest_date) & d2$crop == "rice" & !is.na(d2$fw_yield), c(7, 8, 13, 31:42)])
  dp <- merge(merge(dp, dp1, by = c("trial_id", "treatment", "crop"), all.x = TRUE), dp2, by = c("trial_id", "treatment", "crop"), all.y = TRUE)
  dr <- merge(merge(dr, dr1, by = c("trial_id", "treatment", "crop"), all.x = TRUE), dr2, by = c("trial_id", "treatment", "crop"), all.y = TRUE)
  
  d4 <- carobiner::bindr(dp, dr)
  
  d4$fw_yield <- d4$fw_yield * 1000
  d4$yield_marketable <- d4$yield_marketable * 1000
  
  d4$fertilizer_type <- NA
  d4$fertilizer_type[d4$crop == "potato" & d4$treatment == "BR"] <- "NPK"
  d4$fertilizer_type[d4$crop == "potato" & d4$treatment == "AEZ"] <- "DAP;NPK;urea"
  d4$fertilizer_type[d4$crop == "potato" & d4$treatment == "SSR"] <- "DAP;NPK;urea"
  d4$fertilizer_amount <- NA
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "BR"] <- 300
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "AEZ"] <- 350
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 1] <- 345
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 2] <- 314
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 3] <- 436
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 4] <- 542
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 5] <- 425
  d4$fertilizer_amount[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 6] <- 667
  d4$N_fertilizer <- NA
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "BR"] <- 51
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "AEZ"] <- 104
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 1] <- 106
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 2] <- 95
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 3] <- 137
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 4] <- 144
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 5] <- 121
  d4$N_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 6] <- 212
  d4$P_fertilizer <- NA
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "BR"] <- 22
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "AEZ"] <- 28
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 1] <- 28
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 2] <- 22
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 3] <- 33
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 4] <- 44
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 5] <- 32
  d4$P_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 6] <- 42
  d4$K_fertilizer <- NA
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "BR"] <- 42
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "AEZ"] <- 14
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 1] <- 11
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 2] <- 14
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 3] <- 13
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 4] <- 33
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 5] <- 22
  d4$K_fertilizer[d4$crop == "potato" & d4$treatment == "SSR" & d4$package == 6] <- 28
  
  d4$package <- NULL
  d4$country <- "Rwanda"
  
  d4$crop_price <- d4$crop_price/1000
  d4$crop_price[d4$crop == "potato"] <- 480
  d4$fertilizer_price <- 675
  
  d4$yield <- d4$fw_yield * (1 - ifelse(is.na(d4$yield_moisture), 20, d4$yield_moisture)/100) # Assuming 20% moisture content when it is missing
  d4$yield_moisture <- ifelse(is.na(d4$yield_moisture), 20, d4$yield_moisture)
  d4$yield_isfresh <- TRUE
  d4$fw_yield <- NULL
  
  # Some more fixes
  d4$previous_crop_residue_management <- ifelse(d4$previous_crop_residue_management == "", NA, d4$previous_crop_residue_management)
  d4$land_prep_method <- ifelse(d4$land_prep_method == "", NA, d4$land_prep_method)

  carobiner::write_files(meta, d4, path = path)
  
}

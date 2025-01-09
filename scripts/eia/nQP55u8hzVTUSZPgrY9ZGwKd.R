# R script for EiA version of"carob"

## ISSUES
# ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "nQP55u8hzVTUSZPgrY9ZGwKd"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Isaiah Nyagumbo; Patricia Masikati; Mazvita Chiduwa; John Omondi",
    data_institute = "CIMMYT; IITA; ICRAF-CIFOR",
    title = NA,
    description ="Soilidaridad Soybean Use Case Validations for soybean and maize in 2023-2024",
    license = NA,
    group = group,
    publication=NA,
    usecase_code = "USC016",
    usecase_name = "CH-CerLeg-SolidaridadB",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "on-farm experiment", 
    carob_date="2025-01-09",
    treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
    response_vars= "yield_moisture",
    notes = "land_prep_method might need review. seed_density units not standardized. Diseases, pest and stress not processed but available in the data"
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Chinyanja-Solidaridad-Soy-Validation/", full.names = T))
  
  # Retrieve relevant file(s)
  f1 <- ff[basename(ff) == "20241216152659_EiA_Solidaridad_Validation_raw_2023.xlsx"]
  f2 <- ff[basename(ff) == "20241216152659_EiA_Solidaridad_Validation_raw_2024.xlsx"]
  # Read relevant file(s)
  r1 <- readxl::read_excel(f1)
  r2 <- readxl::read_excel(f2)
  
  d1 <- data.frame(
    country = ifelse(grepl("MW", r1[[5]][2:length(r1[[5]])], fixed = T), "Malawi",
                     ifelse(grepl("MZ", r1[[5]][2:length(r1[[5]])], fixed = T), "Mozambique",
                            ifelse(grepl("ZM", r1[[5]][2:length(r1[[5]])], fixed = T), "Zambia", r1[[5]][2:length(r1[[5]])]))),
    longitude = round(as.numeric(r1[[13]][2:length(r1[[13]])]), 3),
    latitude = round(as.numeric(r1[[14]][2:length(r1[[14]])]), 3),
    elevation = round(as.numeric(r1[[15]][2:length(r1[[15]])]), 0),
    geo_uncertainty = round(as.numeric(r1[[16]][2:length(r1[[16]])]), 1),
    geo_from_source = TRUE,
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    treatment = "Plot 1",
    irrigated = ifelse(r1[[25]][2:length(r1[[25]])] == "rainfed", FALSE, TRUE),
    previous_crop = ifelse(sub(" .*", "", r1[[26]][2:length(r1[[26]])]) == "other", NA, sub(" .*", "", r1[[26]][2:length(r1[[26]])])),
    crop = r1[[120]][2:length(r1[[120]])],
    variety = r1[[156]][2:length(r1[[156]])],
    planting_date = as.character(as.Date(as.integer(r1[[157]][2:length(r1[[157]])]), origin = "1900-01-01")),
    # EGB:
    # # Need to convert to plants/ha (How to convert seed/ha to plant/ha or kg/ha to plant/ha?)
    seed_density = ifelse(r1[[159]][2:length(r1[[159]])] == "plants/m2",
                          as.numeric(r1[[160]][2:length(r1[[160]])])*10000, NA),
    planting_method = sub("_", " ", r1[[161]][2:length(r1[[161]])]),
    intercrops = ifelse(sub(" .*", "", r1[[165]][2:length(r1[[165]])]) == "other", NA, sub(" .*", "", r1[[165]][2:length(r1[[165]])])),
    row_spacing = ifelse(as.numeric(r1[[201]][2:length(r1[[201]])]) >= 1, as.numeric(r1[[201]][2:length(r1[[201]])]), as.numeric(r1[[201]][2:length(r1[[201]])])*100),
    plant_spacing = ifelse(as.numeric(r1[[202]][2:length(r1[[202]])])*100),
    plot_length = as.numeric(r1[[369]][2:length(r1[[369]])]),
    plot_width = as.numeric(r1[[370]][2:length(r1[[370]])]),
    plot_area = as.numeric(r1[[371]][2:length(r1[[371]])]),
    fertilizer_used = ifelse(r1[[405]][2:length(r1[[405]])] != "none", TRUE, FALSE),
    fertilizer_type = ifelse(r1[[405]][2:length(r1[[405]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[405]][2:length(r1[[405]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[418]][2:length(r1[[418]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[419:426]), 419:426]), 2, as.numeric), na.rm = TRUE),
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    fertilizer_price = (as.numeric(r1[[419]][2:length(r1[[419]])])/50)*as.numeric(r1[[471]][2:length(r1[[471]])]) +
      (as.numeric(r1[[420]][2:length(r1[[420]])])/50)*as.numeric(r1[[472]][2:length(r1[[472]])]) +
      (as.numeric(r1[[421]][2:length(r1[[421]])])/50)*as.numeric(r1[[473]][2:length(r1[[473]])]) +
      (as.numeric(r1[[422]][2:length(r1[[422]])])/50)*as.numeric(r1[[474]][2:length(r1[[474]])]) +
      (as.numeric(r1[[423]][2:length(r1[[423]])])/50)*as.numeric(r1[[475]][2:length(r1[[475]])]) +
      (as.numeric(r1[[424]][2:length(r1[[424]])])/50)*as.numeric(r1[[476]][2:length(r1[[476]])]) +
      (as.numeric(r1[[425]][2:length(r1[[425]])])/50)*as.numeric(r1[[477]][2:length(r1[[477]])]) +
      (as.numeric(r1[[426]][2:length(r1[[426]])])/50)*as.numeric(r1[[478]][2:length(r1[[478]])]),
    )
  d1p2 <- data.frame(
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    treatment = "Plot 2",
    crop = r1[[203]][2:length(r1[[203]])],
    variety = r1[[239]][2:length(r1[[239]])],
    planting_date = as.character(as.Date(as.integer(r1[[240]][2:length(r1[[240]])]), origin = "1900-01-01")),
    # EGB:
    # # Need to convert to plants/ha (How to convert seed/ha to plant/ha or kg/ha to plant/ha?)
    seed_density = ifelse(r1[[242]][2:length(r1[[242]])] == "plants/m2",
                          as.numeric(r1[[243]][2:length(r1[[243]])])*10000, NA),
    planting_method = sub("_", " ", r1[[244]][2:length(r1[[244]])]),
    intercrops = NA,
    row_spacing = ifelse(as.numeric(r1[[284]][2:length(r1[[284]])]) >= 1, as.numeric(r1[[284]][2:length(r1[[284]])]), as.numeric(r1[[284]][2:length(r1[[284]])])*100),
    plant_spacing = ifelse(as.numeric(r1[[285]][2:length(r1[[285]])])*100),
    plot_length = as.numeric(r1[[385]][2:length(r1[[385]])]),
    plot_width = as.numeric(r1[[386]][2:length(r1[[386]])]),
    plot_area = as.numeric(r1[[387]][2:length(r1[[387]])]),
    fertilizer_used = ifelse(r1[[427]][2:length(r1[[427]])] != "none", FALSE, TRUE),
    fertilizer_type = ifelse(r1[[427]][2:length(r1[[427]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[427]][2:length(r1[[427]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[440]][2:length(r1[[440]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[441:448]), 441:448]), 2, as.numeric), na.rm = TRUE),
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
  )
  d1p3 <- data.frame(
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    treatment = "Plot 3",
    crop = r1[[286]][2:length(r1[[286]])],
    variety = r1[[322]][2:length(r1[[322]])],
    planting_date = as.character(as.Date(as.integer(r1[[323]][2:length(r1[[323]])]), origin = "1900-01-01")),
    # EGB:
    # # Need to convert to plants/ha (How to convert seed/ha to plant/ha or kg/ha to plant/ha?)
    seed_density = ifelse(r1[[325]][2:length(r1[[325]])] == "plants/m2",
                          as.numeric(r1[[326]][2:length(r1[[326]])])*10000, NA),
    planting_method = sub("_", " ", r1[[327]][2:length(r1[[327]])]),
    intercrops = NA,
    row_spacing = ifelse(as.numeric(r1[[367]][2:length(r1[[367]])]) >= 1, as.numeric(r1[[367]][2:length(r1[[367]])]), as.numeric(r1[[367]][2:length(r1[[367]])])*100),
    plant_spacing = ifelse(as.numeric(r1[[368]][2:length(r1[[368]])])*100),
    plot_length = as.numeric(r1[[388]][2:length(r1[[388]])]),
    plot_width = as.numeric(r1[[389]][2:length(r1[[389]])]),
    plot_area = as.numeric(r1[[390]][2:length(r1[[390]])]),
    fertilizer_used = ifelse(r1[[449]][2:length(r1[[449]])] != "none", FALSE, TRUE),
    fertilizer_type = ifelse(r1[[449]][2:length(r1[[449]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[449]][2:length(r1[[449]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[462]][2:length(r1[[462]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[463:470]), 463:470]), 2, as.numeric), na.rm = TRUE),
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
  )
  
  
  # d <- data.frame(
  #   country= "Rwanda",
  #   longitude = as.numeric(r$intro._geopoint_household_longitude),
  #   latitude = as.numeric(r$intro._geopoint_household_latitude),
  #   elevation = as.numeric(r$intro._geopoint_household_altitude),
  #   geo_uncertainty = as.numeric(r$intro._geopoint_household_precision),
  #   geo_from_source = TRUE,
  #   trial_id = r$intro.barcodehousehold,
  #   treatment = NA,
  #   irrigated = ifelse(r$LandPreparation.landPreparation.irrigation_technique == "rainfed", FALSE, TRUE),
  #   on_farm = TRUE,
  #   is_survey = FALSE,
  #   crop = ifelse(tolower(r$crop) == "potatoirish", "potato", ifelse(tolower(r$crop) == "beansbush", "common bean", tolower(r$crop))),
  #   variety = ifelse(r$planting.plantingDetails.variety == "n/a", NA, r$planting.plantingDetails.variety),
  #   previous_crop = ifelse(r$LandPreparation.landPreparation.previous_crop == "n/a", NA,
  #                          ifelse(r$LandPreparation.landPreparation.previous_crop %in% c("beansClimbing", "beansBush"), "common bean",
  #                                 ifelse(r$LandPreparation.landPreparation.previous_crop == "peas", "pea",
  #                                        ifelse(r$LandPreparation.landPreparation.previous_crop == "carrots", "carrot",
  #                                               ifelse(r$LandPreparation.landPreparation.previous_crop == "potatoIrish", "potato",
  #                                                      ifelse(r$LandPreparation.landPreparation.previous_crop == "passionfruit", "passion fruit",
  #                                                             ifelse(r$LandPreparation.landPreparation.previous_crop == "potatoSweet", "sweetpotato", NA))))))),
  #   residue_prevcrop_used = ifelse(r$LandPreparation.landPreparation.residue_management.removal %in% c("n/a", "TRUE"), FALSE, TRUE),
  #   previous_crop_residue_management = gsub("; ", "", paste0(ifelse(r$LandPreparation.landPreparation.residue_management.incorporation == TRUE, "incorporation", ""),
  #                                                            ifelse(r$LandPreparation.landPreparation.residue_management.spreading == TRUE, "spreading", ""),
  #                                                            ifelse(r$LandPreparation.landPreparation.residue_management.left_for_grazing == TRUE, "grazing", ""),
  #                                                            sep = "; ")),
  #   previous_crop_burnt = as.logical(ifelse(r$LandPreparation.landPreparation.residue_management.burning == "n/a", NA, r$LandPreparation.landPreparation.residue_management.burning)),
  #   previous_crop_residue_perc = as.numeric(ifelse(r$LandPreparation.landPreparation.residue_remaining == "0", 0,
  #                                                  ifelse(r$LandPreparation.landPreparation.residue_remaining == "lessThan50", 50,
  #                                                         ifelse(r$LandPreparation.landPreparation.residue_remaining == "moreThan50", 75,
  #                                                                ifelse(r$LandPreparation.landPreparation.residue_remaining == "all100", 100, NA))))),
  #   fertilizer_used_previous_season = ifelse(r$LandPreparation.landPreparation.inorganic_fertilizer_previous_season == "yes", TRUE,
  #                                            ifelse(r$LandPreparation.landPreparation.inorganic_fertilizer_previous_season == "no", FALSE, NA)),
  #   OM_used_previous_season = ifelse(r$LandPreparation.landPreparation.organic_fertilizer_previous_season == "yes", TRUE,
  #                                    ifelse(r$LandPreparation.landPreparation.organic_fertilizer_previous_season == "no", FALSE, NA)),
  #   # EGB:
  #   # # Not sure if "manual" == "hoeing"
  #   land_prep_method = gsub("manual", "hoeing",gsub("n/a", "", gsub("; n/a", "", gsub("n/a; ", "", paste(r$LandPreparation.landPrepationDetails.tillage_technique_1,
  #                                                                                                        r$LandPreparation.landPrepationDetails.tillage_technique_2,
  #                                                                                                        r$LandPreparation.landPrepationDetails.tillage_technique_3, sep = "; "))))),
  #   planting_date = as.character(as.Date(r$planting.plantingDetails.planting_date, "%m/%d/%Y")),
  #   # EGB:
  #   # # Need to convert to plants/ha (How to convert seed/ha to plant/ha or kg/ha to plant/ha?)
  #   seed_density = ifelse(r$planting.plantingDetails.plant_density_unit == "plants/m2",
  #                         as.numeric(r$planting.plantingDetails.plant_density)*10000, NA),
  #   planting_method = ifelse(r$planting.plantingDetails.planting_technique %in% c("line_sowing", "line_planting"), "line sowing",
  #                            ifelse(r$planting.plantingDetails.planting_technique == "n/a", NA, r$planting.plantingDetails.planting_technique)),
  #   row_spacing = as.numeric(r$planting.densityDetails.row_spacing)*100,
  #   plant_spacing = as.numeric(r$planting.densityDetails.plant_spacing)*100,
  #   OM_used = ifelse(r$fertilizer_org_used == "yes", TRUE,
  #                    ifelse(r$fertilizer_org_used == "no", FALSE, NA)),
  #   OM_type = gsub("; ", "", paste(ifelse(r$organicInputsDetails.fertilizer_org_name.compost == "TRUE", "compost", ""),
  #                                  ifelse(r$organicInputsDetails.fertilizer_org_name.manure == "TRUE", "farmyard manure", ""),
  #                                  ifelse(r$organicInputsDetails.fertilizer_org_name.crop_residues == "TRUE", "leaf litter", ""), sep = "; ")),
  #   OM_date = as.character(as.Date(r$organicInputsDetails.fertilizer_org_date, "%m/%d/%Y")),
  #   plot_area.BR = as.numeric(r$plotDescription.plotLayout_BR.plot_area_control),
  #   plot_area.AEZ = as.numeric(r$plotDescription.plotLayout_AEZ.plot_area_aez),
  #   plot_area.SSR = as.numeric(r$plotDescription.plotLayout_SSR.plot_area_ssr),
  #   plot_area.Lime = as.numeric(r$plotDescription.plotLayout_Lime.plot_area_Lime),
  #   weeding_done = ifelse(as.integer(r$cropManagement.weeding.weeding_number) > 0, TRUE, FALSE),
  #   weeding_times = as.integer(r$cropManagement.weeding.weeding_number),
  #   weeding_dates = gsub("; $", "", gsub("; ; ", "", paste(ifelse(r$cropManagement.weeding.weeding_details.weeding_date_1 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_1, "%m/%d/%Y")), ""),
  #                                                          ifelse(r$cropManagement.weeding.weeding_details.weeding_date_2 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_2, "%m/%d/%Y")), ""),
  #                                                          ifelse(r$cropManagement.weeding.weeding_details.weeding_date_3 != "n/a", as.character(as.Date(r$cropManagement.weeding.weeding_details.weeding_date_3, "%m/%d/%Y")), ""), sep = "; "))),
  #   weeding_method = gsub("manual_weeding", "manual", gsub("; $", "", gsub("; ; ", "", paste(ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_1 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_1, ""),
  #                                                                                            ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_2 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_2, ""),
  #                                                                                            ifelse(r$cropManagement.weeding.weeding_details.weeding_technique_3 != "n/a", r$cropManagement.weeding.weeding_details.weeding_technique_3, ""), sep = "; ")))),
  #   # EGB:
  #   # # Pest, disease and stresses are available in the data, but not processed here...
  #   harvest_date = as.character(as.Date(r$cropManagement.Harvest.harvest.harvest_date, "%m/%d/%Y")),
  #   fw_yield.potato.BR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_BR) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_BR),
  #   yield_marketable.potato.BR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_BR),
  #   yield_part.potato.BR = "tubers",
  #   fw_yield.potato.AEZ = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR1) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR1),
  #   yield_marketable.potato.AEZ = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR1),
  #   yield_part.potato.AEZ = "tubers",
  #   fw_yield.potato.SSR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR2) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR2),
  #   yield_marketable.potato.SSR = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR2),
  #   yield_part.potato.SSR = "tubers",
  #   fw_yield.potato.Lime = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_Lime) + as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_Lime),
  #   yield_marketable.potato.Lime = as.numeric(r$cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_Lime),
  #   yield_part.potato.Lime = "tubers",
  #   fw_yield.rice.BR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_BR),
  #   moisture_yield.rice.BR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_BR),
  #   yield_part.rice.BR = "grain",
  #   fw_yield.rice.AEZ = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR1),
  #   moisture_yield.rice.AEZ = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR1),
  #   yield_part.rice.AEZ = "grain",
  #   fw_yield.rice.SSR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR2),
  #   moisture_yield.rice.SSR = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR2),
  #   yield_part.rice.SSR = "grain",
  #   fw_yield.rice.Lime = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_Lime),
  #   moisture_yield.rice.Lime = as.numeric(r$cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_Lime),
  #   yield_part.rice.Lime = "grain",
  #   # fw_yield.maize.BR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_BR),
  #   # moisture_yield.maize.BR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_BR),
  #   # fw_yield.maize.AEZ = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_SSR1),
  #   # moisture_yield.maize.AEZ = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_SSR1),
  #   # fw_yield.maize.SSR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_SSR2),
  #   # moisture_yield.maize.SSR = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_SSR2),
  #   # fw_yield.maize.Lime = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_FW_Lime),
  #   # moisture_yield.maize.Lime = as.numeric(r$cropManagement.Harvest.harvest.maize_harvest.maizeGrains_MC_Lime),
  #   # fw_yield.bean.BR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_BR),
  #   # moisture_yield.bean.BR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_BR),
  #   # yield_part.bean.BR = "seed",
  #   # fw_yield.bean.AEZ = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_SSR1),
  #   # moisture_yield.bean.AEZ = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_SSR1),
  #   # yield_part.bean.AEZ = "seed",
  #   # fw_yield.bean.SSR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_SSR2),
  #   # moisture_yield.bean.SSR = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_SSR2),
  #   # yield_part.bean.SSR = "seed",
  #   # fw_yield.bean.Lime = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beans_FW_Lime),
  #   # moisture_yield.bean.Lime = as.numeric(r$cropManagement.Harvest.harvest.bean_harvest.beansGrains_MC_Lime),
  #   # yield_part.bean.Lime = "seed",
  #   crop_price = as.numeric(r$cropManagement.product_price),
  #   currency = r$intro.local_currency
  # )
  # 
  # d$OM_type[grep("^\\s*$", d$OM_type, value = FALSE)] <- NA
  # 
  # # Process treatments to long
  # d1 <- reshape(d, direction="long",
  #               varying=c("fw_yield.rice.BR", "fw_yield.potato.BR", "moisture_yield.rice.BR", "yield_part.rice.BR", "yield_marketable.potato.BR", "yield_part.potato.BR", "plot_area.BR",
  #                         "fw_yield.rice.AEZ", "fw_yield.potato.AEZ", "moisture_yield.rice.AEZ", "yield_part.rice.AEZ", "yield_marketable.potato.AEZ", "yield_part.potato.AEZ", "plot_area.AEZ",
  #                         "fw_yield.rice.SSR", "fw_yield.potato.SSR", "moisture_yield.rice.SSR", "yield_part.rice.SSR", "yield_marketable.potato.SSR", "yield_part.potato.SSR", "plot_area.SSR",
  #                         "fw_yield.rice.Lime", "fw_yield.potato.Lime", "moisture_yield.rice.Lime", "yield_part.rice.Lime", "yield_marketable.potato.Lime", "yield_part.potato.Lime", "plot_area.Lime"),
  #               timevar="treatment",
  #               times= c("BR", "AEZ", "SSR", "Lime"),
  #               v.names= c("plot_area",
  #                          "yield_part.rice",
  #                          "moisture_yield.rice",
  #                          "fw_yield.rice",
  #                          "yield_part.potato",
  #                          "yield_marketable.potato",
  #                          "fw_yield.potato"))
  # 
  # # Process crop to long
  # d2 <- reshape(d1[,c(1:length(d1)-1)], direction = "long",
  #               varying = c("fw_yield.potato", "yield_part.potato", 
  #                           "fw_yield.rice", "yield_part.rice"),
  #               timevar = "crop",
  #               times = c("potato", "rice"),
  #               v.names = c("yield_part",
  #                           "fw_yield"))
  # 
  # # Fix column names
  # d2$yield_marketable <- d2$yield_marketable.potato
  # d2$yield_marketable.potato <- NULL
  # d2$yield_moisture <- d2$moisture_yield.rice
  # d2$moisture_yield.rice <- NULL
  # d2$id <- NULL
  # 
  # # Fix future dates error in weeding dates
  # d2$weeding_dates <- gsub("2024", "2023", d2$weeding_dates)
  # 
  # # Synthesize to 1 row per trial/treatment/crop
  # 
  # e1 <- d2[!is.na(d2$previous_crop_burnt), 1:30]
  # e2 <- d2[!is.na(d2$weeding_done), c(1:12, 31:33)]
  # e3 <- d2[!is.na(d2$fw_yield), c(7,8,12, 34:40)]
  # 
  # wt <- aggregate(e2$weeding_times,
  #                 by = list('trial_id' = e2$trial_id, 'treatment' = e2$treatment, 'crop' = e2$crop), FUN = sum, na.rm = T)
  # colnames(wt)[length(wt)] <- "weeding_times"
  # wd <- aggregate(e2$weeding_dates,
  #                 by = list('trial_id' = e2$trial_id, 'treatment' = e2$treatment, 'crop' = e2$crop), FUN = paste, collapse = '; ')
  # colnames(wd)[length(wd)] <- "weeding_dates"
  # wm <- aggregate(e2$weeding_method,
  #                 by = list('trial_id' = e2$trial_id, 'treatment' = e2$treatment, 'crop' = e2$crop), FUN = paste, collapse = '; ')
  # colnames(wm)[length(wm)] <- "weeding_method"
  # 
  # d3 <- merge(e3, merge(wm, merge(wd, merge(wt, e1, by = c("trial_id", "treatment", "crop"), all.y = TRUE),
  #                                 by = c("trial_id", "treatment", "crop"), all.y = TRUE),
  #                       by = c("trial_id", "treatment", "crop"), all.y = TRUE),
  #             by = c("trial_id", "treatment", "crop"), all.y = TRUE)
  # 
  # d3$weeding_done <- ifelse(d3$weeding_times > 0, TRUE, FALSE)
  # 
  # d3 <- d3[!with(d3, is.na(d3$harvest_date) & is.na(d3$crop_price) & is.na(d3$plot_area) & is.na(d3$yield_part) & is.na(d3$yield_marketable) & is.na(d3$fw_yield) & is.na(d3$yield_moisture)),]
  
  # carobiner::write_files(meta, d2, path=path)
  
}

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
    treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer",
    response_vars= "fwy_residue;fw_yield",
    notes = ""
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
    irrigated = ifelse(r1[[25]][2:length(r1[[25]])] == "rainfed", FALSE, TRUE),
    previous_crop = ifelse(sub(" .*", "", r1[[26]][2:length(r1[[26]])]) == "other", NA, sub(" .*", "", r1[[26]][2:length(r1[[26]])]))
  )
  
  d1 <- d1[complete.cases(d1),]
  
  # Actual farmer's practices
  d1p1 <- data.frame(
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    event = r1[[9]][2:length(r1[[7]])],
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
    plant_spacing = ifelse(as.numeric(r1[[202]][2:length(r1[[202]])])*100 > 100, as.numeric(r1[[202]][2:length(r1[[202]])]), as.numeric(r1[[202]][2:length(r1[[202]])])*100),
    # plot_length = as.numeric(r1[[369]][2:length(r1[[369]])]),
    # plot_width = as.numeric(r1[[370]][2:length(r1[[370]])]),
    # plot_area = as.numeric(r1[[371]][2:length(r1[[371]])]),
    fertilizer_used = ifelse(r1[[405]][2:length(r1[[405]])] != "none", TRUE, FALSE),
    fertilizer_type = ifelse(r1[[405]][2:length(r1[[405]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[405]][2:length(r1[[405]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[418]][2:length(r1[[418]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[419:426]), 419:426]), 2, as.numeric), na.rm = TRUE),
    N_fertilizer = as.numeric(r1[[425]][2:length(r1[[425]])])*0.1,
    P_fertilizer = as.numeric(r1[[425]][2:length(r1[[425]])])*0.2,
    K_fertilizer = as.numeric(r1[[425]][2:length(r1[[425]])])*0.1
    
    # fertilizer_price = (as.numeric(r1[[419]][2:length(r1[[419]])])/50)*as.numeric(r1[[471]][2:length(r1[[471]])]) +
    #   (as.numeric(r1[[420]][2:length(r1[[420]])])/50)*as.numeric(r1[[472]][2:length(r1[[472]])]) +
    #   (as.numeric(r1[[421]][2:length(r1[[421]])])/50)*as.numeric(r1[[473]][2:length(r1[[473]])]) +
    #   (as.numeric(r1[[422]][2:length(r1[[422]])])/50)*as.numeric(r1[[474]][2:length(r1[[474]])]) +
    #   (as.numeric(r1[[423]][2:length(r1[[423]])])/50)*as.numeric(r1[[475]][2:length(r1[[475]])]) +
    #   (as.numeric(r1[[424]][2:length(r1[[424]])])/50)*as.numeric(r1[[476]][2:length(r1[[476]])]) +
    #   (as.numeric(r1[[425]][2:length(r1[[425]])])/50)*as.numeric(r1[[477]][2:length(r1[[477]])]) +
    #   (as.numeric(r1[[426]][2:length(r1[[426]])])/50)*as.numeric(r1[[478]][2:length(r1[[478]])]),
    
    # EGB:
    # # There's no yield data (!?)
    )
  d1p1 <- d1p1[d1p1$event == "event1", colnames(d1p1)[colnames(d1p1) != "event"]]
  # Add yield components
  d2p1 <- data.frame(
    trial_id = ifelse(is.na(r2[[8]][2:length(r2[[8]])]), r2[[9]][2:length(r2[[9]])], r2[[8]][2:length(r2[[8]])]),
    treatment = ifelse(r2[[3]][2:length(r2[[3]])] == "event8a", "Actual farmer practices",
                       ifelse(r2[[3]][2:length(r2[[3]])] == "event8b", "standardized farmer practices", "MVP")),
    plot_length = as.numeric(r2[[373]][2:length(r2[[373]])]),
    plot_width = as.numeric(r2[[374]][2:length(r2[[374]])]),
    plot_area = as.numeric(r2[[375]][2:length(r2[[375]])]),
    harvest_date = as.character(as.Date(as.integer(r2[[524]][2:length(r2[[524]])]), origin = "1900-01-01")),
    fwy_residue = (as.numeric(r2[[527]][2:length(r2[[527]])]) / as.numeric(r2[[375]][2:length(r2[[375]])])) * 10000,
    fw_yield = (as.numeric(r2[[528]][2:length(r2[[528]])]) / as.numeric(r2[[375]][2:length(r2[[375]])])) * 10000,
    crop_price = as.numeric(r2[[533]][2:length(r2[[533]])])
  )
  d2p1 <- d2p1[d2p1$treatment == "Actual farmer practices", ]
  dp1 <- merge(d1p1, d2p1, "trial_id")
  
  # Standardized farmers practices 
  d1p2 <- data.frame(
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    event = r1[[9]][2:length(r1[[7]])],
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
    plant_spacing = ifelse(as.numeric(r1[[285]][2:length(r1[[285]])])*100 > 100, as.numeric(r1[[285]][2:length(r1[[285]])]), as.numeric(r1[[285]][2:length(r1[[285]])])*100),
    # plot_length = as.numeric(r1[[385]][2:length(r1[[385]])]),
    # plot_width = as.numeric(r1[[386]][2:length(r1[[386]])]),
    # plot_area = as.numeric(r1[[387]][2:length(r1[[387]])]),
    fertilizer_used = ifelse(r1[[427]][2:length(r1[[427]])] != "none", FALSE, TRUE),
    fertilizer_type = ifelse(r1[[427]][2:length(r1[[427]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[427]][2:length(r1[[427]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[440]][2:length(r1[[440]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[441:448]), 441:448]), 2, as.numeric), na.rm = TRUE),
    # SSP amount is missing
    N_fertilizer = ifelse(is.na((as.numeric(r1[[447]][2:length(r1[[447]])]))), 0, as.numeric(r1[[447]][2:length(r1[[447]])])*0.1) + 
      ifelse(is.na(as.numeric(r1[[448]][2:length(r1[[448]])])), 0, as.numeric(r1[[448]][2:length(r1[[448]])])*0.1),
    P_fertilizer = ifelse(is.na((as.numeric(r1[[447]][2:length(r1[[447]])]))), 0, as.numeric(r1[[447]][2:length(r1[[447]])])*0.2) + 
      ifelse(is.na(as.numeric(r1[[448]][2:length(r1[[448]])])), 0, as.numeric(r1[[448]][2:length(r1[[448]])])*0.2),
    K_fertilizer = ifelse(is.na((as.numeric(r1[[447]][2:length(r1[[447]])]))), 0, as.numeric(r1[[447]][2:length(r1[[447]])])*0.1) + 
      ifelse(is.na(as.numeric(r1[[448]][2:length(r1[[448]])])), 0, as.numeric(r1[[448]][2:length(r1[[448]])])*0.1)
    
    # fertilizer_price = (as.numeric(r1[[441]][2:length(r1[[441]])])/50)*as.numeric(r1[[471]][2:length(r1[[471]])]) +
    #   (as.numeric(r1[[442]][2:length(r1[[442]])])/50)*as.numeric(r1[[472]][2:length(r1[[472]])]) +
    #   (as.numeric(r1[[443]][2:length(r1[[443]])])/50)*as.numeric(r1[[473]][2:length(r1[[473]])]) +
    #   (as.numeric(r1[[444]][2:length(r1[[444]])])/50)*as.numeric(r1[[474]][2:length(r1[[474]])]) +
    #   (as.numeric(r1[[445]][2:length(r1[[445]])])/50)*as.numeric(r1[[475]][2:length(r1[[475]])]) +
    #   (as.numeric(r1[[446]][2:length(r1[[446]])])/50)*as.numeric(r1[[476]][2:length(r1[[476]])]) +
    #   (as.numeric(r1[[447]][2:length(r1[[447]])])/50)*as.numeric(r1[[477]][2:length(r1[[477]])]) +
    #   (as.numeric(r1[[448]][2:length(r1[[448]])])/50)*as.numeric(r1[[478]][2:length(r1[[478]])]),
    
    # EGB:
    # # There's no yield data (!?)
  )
  d1p2 <- d1p2[d1p2$event == "event1", colnames(d1p2)[colnames(d1p2) != "event"]]
  # Add yield components
  d2p2.size <- data.frame(
    trial_id = ifelse(is.na(r2[[8]][2:length(r2[[8]])]), r2[[9]][2:length(r2[[9]])], r2[[8]][2:length(r2[[8]])]),
    treatment = ifelse(r2[[3]][2:length(r2[[3]])] == "event8a", "Actual farmer practices",
                       ifelse(r2[[3]][2:length(r2[[3]])] == "event8b", "standardized farmer practices", "MVP")),
    plot_length = as.numeric(r2[[376]][2:length(r2[[376]])]),
    plot_width = as.numeric(r2[[377]][2:length(r2[[377]])]),
    plot_area = as.numeric(r2[[378]][2:length(r2[[378]])])
  )
  d2p2.size <- d2p2.size[d2p2.size$treatment == "Actual farmer practices", colnames(d2p2.size)[colnames(d2p2.size) != "treatment"]]
  d2p2 <- data.frame(
    trial_id = ifelse(is.na(r2[[8]][2:length(r2[[8]])]), r2[[9]][2:length(r2[[9]])], r2[[8]][2:length(r2[[8]])]),
    treatment = ifelse(r2[[3]][2:length(r2[[3]])] == "event8a", "Actual farmer practices",
                       ifelse(r2[[3]][2:length(r2[[3]])] == "event8b", "standardized farmer practices", "MVP")),
    harvest_date = as.character(as.Date(as.integer(r2[[552]][2:length(r2[[552]])]), origin = "1900-01-01")),
    fwy_residue = as.numeric(r2[[555]][2:length(r2[[555]])]),
    fw_yield = as.numeric(r2[[556]][2:length(r2[[556]])]),
    crop_price = as.numeric(r2[[561]][2:length(r2[[561]])])
  )
  d2p2 <- d2p2[d2p2$treatment == "standardized farmer practices", ]
  d2p2 <- merge(d2p2, d2p2.size, "trial_id")
  d2p2$fwy_residue <- (d2p2$fwy_residue / d2p2$plot_area) * 10000
  d2p2$fw_yield <- (d2p2$fw_yield / d2p2$plot_area) * 10000
  dp2 <- merge(d1p2, d2p2, "trial_id")
  
  # MVP
  d1p3 <- data.frame(
    trial_id = ifelse(is.na(r1[[7]][2:length(r1[[7]])]), r1[[8]][2:length(r1[[8]])], r1[[7]][2:length(r1[[7]])]),
    event = r1[[9]][2:length(r1[[7]])],
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
    plant_spacing = ifelse(as.numeric(r1[[368]][2:length(r1[[368]])])*100 > 100, as.numeric(r1[[368]][2:length(r1[[368]])]), as.numeric(r1[[368]][2:length(r1[[368]])])*100),
    # plot_length = as.numeric(r1[[388]][2:length(r1[[388]])]),
    # plot_width = as.numeric(r1[[389]][2:length(r1[[389]])]),
    # plot_area = as.numeric(r1[[390]][2:length(r1[[390]])]),
    fertilizer_used = ifelse(r1[[449]][2:length(r1[[449]])] != "none", FALSE, TRUE),
    fertilizer_type = ifelse(r1[[449]][2:length(r1[[449]])] != "none", gsub("other", "unknown", gsub("compoundD", "D-compound	", sub(" .*", "", r1[[449]][2:length(r1[[449]])]))), NA),
    fertilizer_date = as.character(as.Date(as.integer(r1[[462]][2:length(r1[[462]])]), origin = "1900-01-01")),
    fertilizer_amount = rowSums(apply(as.matrix(r1[2:nrow(r1[463:470]), 463:470]), 2, as.numeric), na.rm = TRUE),
    N_fertilizer = ifelse(is.na((as.numeric(r1[[465]][2:length(r1[[465]])]))), 0, as.numeric(r1[[465]][2:length(r1[[465]])])*0.18) + 
      ifelse(is.na(as.numeric(r1[[467]][2:length(r1[[467]])])), 0, as.numeric(r1[[467]][2:length(r1[[467]])])*0.1) +
      ifelse(is.na(as.numeric(r1[[469]][2:length(r1[[469]])])), 0, as.numeric(r1[[469]][2:length(r1[[469]])])*0.1),
    P_fertilizer = ifelse(is.na((as.numeric(r1[[465]][2:length(r1[[465]])]))), 0, as.numeric(r1[[465]][2:length(r1[[465]])])*0.2) + 
      ifelse(is.na(as.numeric(r1[[467]][2:length(r1[[467]])])), 0, as.numeric(r1[[467]][2:length(r1[[467]])])*0.1) +
      ifelse(is.na(as.numeric(r1[[469]][2:length(r1[[469]])])), 0, as.numeric(r1[[469]][2:length(r1[[469]])])*0.1),
    K_fertilizer = ifelse(is.na(as.numeric(r1[[467]][2:length(r1[[467]])])), 0, as.numeric(r1[[467]][2:length(r1[[467]])])*0.1) +
      ifelse(is.na(as.numeric(r1[[469]][2:length(r1[[469]])])), 0, as.numeric(r1[[469]][2:length(r1[[469]])])*0.1)
    
    # fertilizer_price = (as.numeric(r1[[441]][2:length(r1[[441]])])/50)*as.numeric(r1[[471]][2:length(r1[[471]])]) +
    #   (as.numeric(r1[[442]][2:length(r1[[442]])])/50)*as.numeric(r1[[472]][2:length(r1[[472]])]) +
    #   (as.numeric(r1[[443]][2:length(r1[[443]])])/50)*as.numeric(r1[[473]][2:length(r1[[473]])]) +
    #   (as.numeric(r1[[444]][2:length(r1[[444]])])/50)*as.numeric(r1[[474]][2:length(r1[[474]])]) +
    #   (as.numeric(r1[[445]][2:length(r1[[445]])])/50)*as.numeric(r1[[475]][2:length(r1[[475]])]) +
    #   (as.numeric(r1[[446]][2:length(r1[[446]])])/50)*as.numeric(r1[[476]][2:length(r1[[476]])]) +
    #   (as.numeric(r1[[447]][2:length(r1[[447]])])/50)*as.numeric(r1[[477]][2:length(r1[[477]])]) +
    #   (as.numeric(r1[[448]][2:length(r1[[448]])])/50)*as.numeric(r1[[478]][2:length(r1[[478]])]),
    
    # EGB:
    # # There's no yield data (!?)
  )
  d1p3 <- d1p3[d1p3$event == "event1", colnames(d1p3)[colnames(d1p3) != "event"]]
  # Add yield components
  d2p3.size <- data.frame(
    trial_id = ifelse(is.na(r2[[8]][2:length(r2[[8]])]), r2[[9]][2:length(r2[[9]])], r2[[8]][2:length(r2[[8]])]),
    treatment = ifelse(r2[[3]][2:length(r2[[3]])] == "event8a", "Actual farmer practices",
                       ifelse(r2[[3]][2:length(r2[[3]])] == "event8b", "standardized farmer practices", "MVP")),
    plot_length = as.numeric(r2[[379]][2:length(r2[[379]])]),
    plot_width = as.numeric(r2[[380]][2:length(r2[[380]])]),
    plot_area = as.numeric(r2[[381]][2:length(r2[[381]])])
  )
  d2p3.size <- d2p3.size[d2p3.size$treatment == "Actual farmer practices", colnames(d2p3.size)[colnames(d2p3.size) != "treatment"]]
  d2p3 <- data.frame(
    trial_id = ifelse(is.na(r2[[8]][2:length(r2[[8]])]), r2[[9]][2:length(r2[[9]])], r2[[8]][2:length(r2[[8]])]),
    treatment = ifelse(r2[[3]][2:length(r2[[3]])] == "event8a", "Actual farmer practices",
                       ifelse(r2[[3]][2:length(r2[[3]])] == "event8b", "standardized farmer practices", "MVP")),
    harvest_date = as.character(as.Date(as.integer(r2[[562]][2:length(r2[[562]])]), origin = "1900-01-01")),
    fwy_residue = as.numeric(r2[[565]][2:length(r2[[565]])]),
    fw_yield = as.numeric(r2[[566]][2:length(r2[[566]])]),
    crop_price = as.numeric(r2[[571]][2:length(r2[[571]])])
  )
  d2p3 <- d2p3[d2p3$treatment == "MVP", ]
  d2p3 <- merge(d2p3, d2p3.size, "trial_id")
  d2p3$fwy_residue <- (d2p3$fwy_residue / d2p3$plot_area) * 10000
  d2p3$fw_yield <- (d2p3$fw_yield / d2p3$plot_area) * 10000
  dp3 <- merge(d1p3, d2p3, "trial_id")
  
  dp1 <- merge(d1, dp1, by = "trial_id")
  dp2 <- merge(d1, dp2, by = "trial_id")
  dp3 <- merge(d1, dp3, by = "trial_id")
  
  d <- carobiner::bindr(dp1, dp2, dp3)

  # EGB: 
  # # Not sure why, but there are duplicated entries... Keeping only the latests submissions
  dd <- data.frame()
  for (trid in unique(d$trial_id)) {
    for (treat in unique(d$treatment)) {
      subs <- d[d$trial_id == trid & d$treatment == treat,]
      k <- subs[which.max(as.Date(subs$planting_date)),]
      dd <- rbind(dd, k)
    }
  }
  
  carobiner::write_files(meta, dd, path=path)
  
}

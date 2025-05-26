# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
   " Rice is the main crop and major staple food of many Asian countries, with global&nbsp;production at about 600 million tons annually. Demand for rice is estimated to double&nbsp;when the global population reaches 9 billion by 2050. However, rice production currently&nbsp;faces many serious challenges, such as climate change,&nbsp;labor&nbsp;shortage, water shortages&nbsp;and loss of croplands because of increased urbanization and industrialization. At the&nbsp;same time, flooded rice production has a substantial environmental footprint, such as&nbsp;contributing 1.5% to global GHGEs. These challenges and problems of rice production&nbsp;are applicable in Vietnam, which is one of the top rice-exporting countries. In addition,&nbsp;the current overuse of agronomic inputs such as seeds, fertilizer, and pesticides in rice&nbsp;production in Vietnam has adverse effects on biodiversity, the environment and human&nbsp;health"
   
   
   uri <- "4F9XYAvjFntaR60zM9kH1CeX"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication= NA,
      authors ="Hung Van Nguyen",
      data_institute ="IRRI",
      title = NA,
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC018",
      usecase_name = 'SEA-DSRC-Vietnam',
      activity = 'validtion',
      treatment_vars= "seed_rate; N_fertilizer; P_fertilizer; K_fertilizer",
      response_vars= "yield",
      project = 'Excellence in Agronomy',
      data_type = "experiment",
      carob_date="2025-05-13",
      notes= NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Vietnam-DSRC-Validation/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_DSRC_Vietnam_Validation_clean_2018_2024.xlsx"]
   # Read relevant file
   
   ## Processing data 
   r <- carobiner::read.excel(f, fix_names = TRUE, na=("n/a"))
   d <- data.frame(
      country= "Vietnam",
      crop ="rice",
      on_farm = ifelse(grepl("On-farm", r$Trial.type.on.station.on.farm),TRUE, FALSE),
      is_survey = FALSE,
      adm1=r$Province,
      trial_id = as.character((r$Trial.ID)),
      season= r$Season,
      treatment= r$Name,
      seed_rate= r$Seed.rate.kg.ha,
      fertilizer_type= "NPK",
      N_fertilizer= r$Ferilizer.N.kg.ha,
      P_fertilizer= r$Ferilizer.P2O5.kg.ha/2.29,
      K_fertilizer= r$Ferilizer.K2O.kg.ha/1.2051,
      yield= r$Yield.t.ha*1000, ### Kg/ha
      productivity_rate= r$Labor.productivity.kg.day, ## 
      planting_date= substr(r$Year, 1, 4),
      Net_income= r$Net.income.USD.ha.1 , ## per hactar
      currency= "USD",
      N_PFP= r$PFPN.kg.kg, #Partial factor productivity of N (measure of efficiency of N input use)
      P_PFP= r$PFPP.kg.kg,
      K_PFP= r$PFPK.kg.kg,
      Energy_Efficiency= r$Energy.efficiency.GJ.ha,
      GHG_emissions= r$GHGe.kg.CO2eq.ha,
      carbonFootprint_soil= r$CF.Soil.emission.kg.CO2eq.kg.rice,
      carbonFootprint_input= r$CF.Agro.input.kg.CO2eq.kg.rice,
      carbonFootprint_operation= r$CF.Opearation.kg.CO2eq.kg.rice,
      Total_carbonFootprint= r$CF.Total.kg.CO2eq.kg.rice,
      pesticide_used= ifelse(is.na(r$No.of.pesticide.application),FALSE, TRUE),
      geo_from_source = FALSE,
      yield_part= "grain", 
      irrigated= NA
   )
   
   ### Adding geo-coordinate 
   
   geo <- data.frame(
      adm1= c("Can Tho","Tien Giang", "Long An", "Soc Trang", "Dong Thap"),
      latidude= c(10.04163, 10.37189, 10.56061, 9.60256, 10.39079),
      longitude= c(105.86858, 106.15924, 106.64956, 105.97296, 105.550048)
   )
   
   d <- merge(d, geo, by="adm1", all.x= TRUE)
   
   carobiner::write_files(meta, d, path=path)
}

#carob_script(path)
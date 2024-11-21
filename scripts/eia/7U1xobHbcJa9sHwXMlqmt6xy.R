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
   
   uri <- "7U1xobHbcJa9sHwXMlqmt6xy"
   group <- "eia"
   
   meta <- data.frame(
      uri = uri,
      dataset_id = uri,
      publication= NA,
      authors ="Mary Jane; John Doe",
      data_institute ="IITA",
      title = NA,
      group = group,
      license = 'none',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC013",
      usecase_name = 'NG-Akilimo-MC Sprout',
      activity = 'addon',
      treatment_vars= "none",
      response_vars= "none",
      project = 'Excellence in Agronomy',
      data_type = "survey",
      carob_date="2024-11-20",
      notes= NA
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Nigeria-Sprout-AddOn/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_AddOn_Full_Survey_MercyCorps_2023_11_clean.xlsx"]
   # Read relevant file
   
   ## Processing data 
   r <- carobiner::read.excel(f,sheet ="EiA_AddOn_Full_Survey")[-c(1),]
   d <- data.frame(
      country = r$country,
      on_farm = FALSE,
      is_survey = TRUE,
      adm1=r$admin_1,
      adm2=r$admin_2,
      location=r$village,
      longitude=as.numeric(r$longitude),
      latitude=as.numeric(r$latitude),
      currency=r$local_currency,
      trial_id = r$barcodehousehold,
      #technology_use= r$use_case_technology, ## Not a carob variable
      fertilizer_amount= ifelse(grepl("sacks_50kg", r$fertiliser_units), as.numeric(r$fertiliser_amount)*50, as.numeric(r$fertiliser_amount)),
      fertilizer_type= r$fertiliser_type,
      irrigation_dates= r$Irrigation_months, 
      irrigation_method= r$irrigation_technique,
      irregation_source= r$Irrigation_source,
      irrigated= ifelse(grepl("No", r$land_irrigated), FALSE,
                        ifelse(grepl("Yes", r$land_irrigated), TRUE, NA)), 
      land_prep_implement=r$tillage_power,
      fertilizer_constraint= r$constraint_fertilizers, ## Not a carob variable
      geo_from_source = TRUE
   )
   
   ## Adding farmer gender 
   rr <- carobiner::read.excel(f,sheet = "hh_members_details_repeat")[-c(1),]
   rr <- rr[, c("person_gender", "barcodehousehold", "person_age")]
   colnames(rr) <- c("farmer_gender", "trial_id", "farmer_age") 
   rr <- rr[!duplicated(rr$trial_id), ]
   d <- merge(d, rr, by= "trial_id", all.x= TRUE) 
   
   ### Processing plot size data 
   r1 <- carobiner::read.excel(f,sheet = "hh_plots_repeat")[-c(1),]
   d1 <- data.frame(
      plot_area= ifelse(grepl("acres", r1$unitland), as.numeric(r1$plot_size)*4047,
                        ifelse(grepl("hectare", r1$unitland), as.numeric(r1$plot_size)*10000, as.numeric(r1$plot_size))),
      trial_id= r1$barcodehousehold
   )
   d1 <- d1[!duplicated(d1$trial_id), ]
   d <- merge(d, d1, by="trial_id", all.x=TRUE)
   
   ### Processing yield data
   
   r2 <- carobiner::read.excel(f,sheet = "crop_repeat")[-c(1),]
   d2 <- data.frame(
      crop=  tolower(r2$crop_label),
      season=r2$season_grown,
      yield= ifelse(grepl("sacks_100kg", r2$crop_yield_units), as.numeric(r2$crop_yield)*100,
                    ifelse(grepl("sacks_50kg", r2$crop_yield_units), as.numeric(r2$crop_yield)*50,
                           ifelse(grepl("Basket (25kg)", r2$crop_yield_units), as.numeric(r2$crop_yield)*25, as.numeric(r2$crop_yield)))) ,
      crop_system= r2$crop_intercrop, ## Not a carob variable
      crop_price= ifelse(grepl("50kg", r2$crop_sold_price_quantityunits), as.numeric(r2$crop_sold_income)/50, 
                         ifelse(grepl("other", r2$crop_sold_price_quantityunits), as.numeric(r2$crop_sold_income)/100, as.numeric(r2$crop_sold_income))),
      unit_price= r2$crop_sold_price_quantityunits,
      trial_id= r2$barcodehousehold
   ) 
   
   d2$crop_price <- ifelse(grepl("income_per_year", d2$unit_price), d2$crop_price/d2$yield, d2$crop_price)
   d2$unit_price <- NULL
   d2 <- d2[!duplicated(d2$trial_id), ]
   d <- merge(d, d2, by="trial_id", all.x=TRUE)
   
   ## Adding variety, disease, pest and previous crop 
   r3 <- carobiner::read.excel(f,sheet = "plot_information_repeat")[-c(1),]
   d3 <- data.frame(
      house_distance= as.numeric(r3$distance_min),
      variety= r3$variety,
      previous_crop= tolower(r3$previous_crop),
      intercrops= r3$inter_crop,
      planting_date= r3$planting_date,
      soil_type= ifelse(grepl("other", r3$soilName), r3$soilName_other, r3$soilName) ,
      land_prep_method= ifelse(grepl("other", r3$land_prep_activities), r3$land_prep_activities_other, r3$land_prep_activities),
      labor_cost1= ifelse(grepl("other", r3$land_prep_average_cost_laborers), r3$land_prep_other_cost, r3$land_prep_average_cost_laborers) ,
      planting_implement= r3$seeding_method,
      planting_method= r3$seeding_tool,
      labor_cost2= ifelse(grepl("other", r3$planting_average_cost_laborers),r3$planting_other_cost, r3$planting_average_cost_laborers) ,
      weeding_times= as.integer(r3$weeding_number),
      weeding_done= TRUE,
      labor_cost3= r3$weeding_other_cost,
      irrigation_times= as.integer(r3$irrigation_number),
      irrigation_amount= as.integer(r3$irrigation_amount_mm),
      labor_cost4= ifelse(grepl("other", r3$irrigation_average_cost_laborers),r3$irrigation_other_cost, r3$irrigation_average_cost_laborers),
      labor_cost5= ifelse(grepl("other",r3$harvest_transport_cost),r3$harvest_other_cost, r3$harvest_transport_cost) ,
      pest_species= r3$pest_name,
      diseases= r3$disease_name,
      trial_id= r3$barcodehousehold
      
   )
   
   d3 <- d3[!duplicated(d3$trial_id), ]
   d <- merge(d, d3, by="trial_id", all.x=TRUE)
   
   ### Adding organic matter 
   
   r4 <- carobiner::read.excel(f,sheet = "organic_inputs_details_repeat", na=c("NA"))[-c(1),]
   r4$organic_ids <- gsub("_NA|_high|_low", "", r4$organic_ids)
   d4 <- data.frame(
      OM_amount= ifelse(grepl("50kg", r4$organic_input_unit), as.numeric(r4$organic_input_amount)*50,
                  ifelse(grepl("100kg", r4$organic_input_unit), as.numeric(r4$organic_input_amount)*100,
                  ifelse(grepl("tonne",r4$organic_input_unit),as.numeric(r4$organic_input_amount)*1000, as.numeric(r4$organic_input_amount)))),
      
      OM_type= ifelse(grepl("manure", r4$organic_id), "farmyard manure",
               ifelse(grepl("-1" , r4$organic_id), "none", 
               ifelse(grepl("other_input",r4$organic_id), "unknown", r4$organic_id))),
      
      OM_price= r4$organic_input_cost,
      trial_id= r4$barcodehousehold
   )
   
   d4$OM_used <- ifelse(grepl("none", d4$OM_type), FALSE, TRUE)
   d4 <- d4[!duplicated(d4$trial_id), ]
   d <- merge(d, d4, by="trial_id", all.x=TRUE)
   
   ### Adding fertilizer price
   
   r5 <- carobiner::read.excel(f,sheet = "inorganic_inputs_details_repeat")[-c(1),]
   d5 <- data.frame(
      fertilizer_type= r5$inorganic_ids,
      fertilizer_price= r5$inorganic_inputs_costs, ## Total fertilizer cost 
      trial_id= r5$barcodehousehold 
   )  
   d5 <- d5[!duplicated(d5$trial_id), ]
   d <- merge(d, d5, by=c("trial_id", "fertilizer_type"), all.x=TRUE)
   
   d$yield <- (d$yield/d$plot_area)*10000 #  kg/ha
   d$fertilizer_price <- d$fertilizer_price
   d$labour_cost <- as.numeric(d$labor_cost1)+ as.numeric(d$labor_cost2)+ as.numeric(d$labor_cost3)+as.numeric(d$labor_cost4)+ as.numeric(d$labor_cost5)
   
   d$labor_cost1 <- d$labor_cost2<- d$labor_cost3 <- d$labor_cost4 <- d$labor_cost5 <- NULL
   ## Fixing fertilizer type 
   p <- carobiner::fix_name(d$fertilizer_type)
   p <- gsub(" ","; ",p)
   p <- gsub("15|17", "", p)
   p <- gsub("other","unknown", p)
   d$fertilizer_type <- p
   d$fertilizer_type[d$fertilizer_type=="NA"] <- "none"
   ### Fixing crop 
   d$crop <- gsub("irish potatoes", "potato", d$crop)
   
   d$previous_crop <- gsub("groundnuts", "groundnut", d$previous_crop)
   d$previous_crop <- gsub("peas", "pea", d$previous_crop)
   d$previous_crop <- ifelse(grepl("potatoirish", d$previous_crop), "potato",
                             ifelse(grepl("no_answer|other1", d$previous_crop), "none",
                                    ifelse(grepl("beansbush", d$previous_crop), "common bean", d$previous_crop)))
   
   inter <- carobiner::fix_name(d$intercrops, "lower") 
   inter <- gsub("beansbush", "common bean", inter)
   inter <- gsub("potatoirish", "potato", inter)
   inter <- gsub("other3", "none", inter)
   inter <- gsub("groundnuts", "groundnut", inter)
   inter <- gsub("bananas", "banana", inter)
   d$intercrops <- inter
   ### 
   d$land_prep_implement[grepl("manual 4wheel_tractor", d$land_prep_implement)] <- "manual; 4 wheel tractor"
   d$land_prep_implement[grepl("manual other|mechanical manual", d$land_prep_implement)] <- "manual"
   d$land_prep_implement[d$land_prep_implement=="2wheel_tractor"] <- "2 wheel tractor"
   d$land_prep_implement[d$land_prep_implement=="4wheel_tractor"] <- "4 wheel tractor"
   d$land_prep_method <- "conventional" ## not sure 
   ### Fixed irrigation method
   
   irr <- carobiner::fix_name(d$irrigation_method)
   irr <- gsub("flooding hose", "continuous flooding", irr)
   irr <- gsub("hose flooding", "continuous flooding", irr)
   irr <- gsub("hose furrow", "furrow", irr)
   irr <- gsub("furrow hose" , "furrow", irr)
   irr <- gsub("bucket furrow hose", "furrow", irr)
   irr <- gsub("bucket hose furrow", "furrow", irr)
   irr <- gsub("bucket furrow", "furrow", irr)
   irr <- gsub("micro-basin", "basins", irr)
   irr <- gsub("hose basin", "basins", irr)
   irr <- gsub("bucket basin", "basins", irr)
   irr <- gsub("hose drip", "drip", irr)
   irr <- gsub("can hose", "furrow", irr)
   irr <- gsub("bucket can", "furrow", irr)
   irr <- gsub("can bucket", "furrow", irr)
   irr <- gsub("bucket hose", "furrow", irr)
   irr <- gsub("hose bucket", "furrow", irr)
   d$irrigation_method <- irr
   d$irrigation_method[grepl("bucket|hose|can", d$irrigation_method)] <- "furrow"
   d$irrigation_method[grepl("flooding", d$irrigation_method)] <- "continuous flooding"
   d$irrigation_method[grepl("basinss", d$irrigation_method)] <- "basins"
   
   
   d$planting_implement[grepl("dibbling", d$planting_implement)] <- "dibbling stick"
   d$planting_implement[grepl("lineseeding", d$planting_implement)] <- "direct seeder"
   d$planting_method <- "manual"
   
   
   ### Fixing Irrigation date
   d$irrigation_dates <- gsub("no_answer", "", d$irrigation_dates)
   d$irrigation_dates <- gsub(" |  ", "; ", d$irrigation_dates)
   d$irrigation_dates <- gsub("jan", "2023-01", d$irrigation_dates)
   d$irrigation_dates <- gsub("feb", "2023-02", d$irrigation_dates)
   d$irrigation_dates <- gsub("mar", "2022-03", d$irrigation_dates)
   d$irrigation_dates <- gsub("apr", "2022-04", d$irrigation_dates)
   d$irrigation_dates <- gsub("may", "2022-05", d$irrigation_dates)
   d$irrigation_dates <- gsub("jun", "2022-06", d$irrigation_dates)
   d$irrigation_dates <- gsub("jul", "2022-07", d$irrigation_dates)
   d$irrigation_dates <- gsub("aug", "2022-08", d$irrigation_dates)
   d$irrigation_dates <- gsub("sep", "2022-09", d$irrigation_dates)
   d$irrigation_dates <- gsub("oct", "2022-10", d$irrigation_dates)
   d$irrigation_dates <- gsub("nov", "2022-11", d$irrigation_dates)
   d$irrigation_dates <- gsub("dec", "2022-12", d$irrigation_dates)
   
   d$N_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- as.numeric(NA)
   
   
   carobiner::write_files(meta, d, path=path)
}

#carob_script(path)
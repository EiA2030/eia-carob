# R script for EiA version of"carob"

## ISSUES
# RUE for nutrients is precalculated
# Energy consumption is provided
# GHG emissions (CO2eq) are provided
# Only net income is provided, so the calculation of crop_price is flawed...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  uri <- "8ymri2XtaPRDl1dmMJW1H9jG"
  group <- "eia"
  
  meta <- data.frame(
    # Need to fill-in metadata...
    uri = uri,
    dataset_id = uri,
    authors = "Hung Van Nguyen",
    data_organization = "IRRI",
    data_citation = "NA",
    title = "Use Case 2.7: mDSRC (Vietnam) optimizing mechanized rice direct seeding and agronomy advisory.",
    description = "Comparative analysis on agronomy</strong>&nbsp;and sustainable factors/ indicators of rice production using different rice crop establishment&nbsp;options, including broadcast seeding, blower seeding, mechanized DSR, and mechanized TRP.&nbsp;​</li>\r\n\t<li>\r\n\t<p><strong>Identified scale-appropriate precision&nbsp;</strong>sowing options for MRD of Vietnam considering to field size, soil, land preparation,&nbsp;availability of tractors/ undercarriages and the other related value chain support services.&nbsp;&nbsp;&nbsp;​</p>\r\n\t</li>\r\n\t<li>\r\n\t<p><strong>Insights from ex-post studies of mechanized-DSR and -TRP adopters and dis-adopters</strong>, coupled with an&nbsp;ex-ante study of the&nbsp;potential for adoption by new farmers, to inform a business case and scaling strategy for precision sowing and locally tailored&nbsp;agronomy.&nbsp;​</p>\r\n\t</li>\r\n\t<li>\r\n\t<p><strong>A service provision brokering</strong>&nbsp;through an agronomic data and market dashboard to (1) provide core learnings and messages for&nbsp;machinery service providers and farmers on locally appropriate sowing agronomic practices and to (2) assist farming machine&nbsp;(focused on crop establishment) companies, owners, service providers, and suitable mechanics to easily locate each other.&nbsp;​</p>\r\n\t</li>\r\n\t<li>\r\n\t<p>Evidence for suggested&nbsp;improvements in ergonomics of precision sowing machinery for women and men&nbsp;and performance of&nbsp;soil engaging and seeding equipment.&nbsp;​</p>\r\n\t</li>\r\n\t<li>\r\n\t<p>Through this Use Case&rsquo;s multi-stakeholder engagement and co-design processes, the data, analytics, and learning from the above&nbsp;will be synthesized and incorporated into a document that provides&nbsp;(1) the business case for mechanized-DSR and -TRP relevant&nbsp;to machinery manufacturers, service providers, and farmers in MRD of Vietnam, and&nbsp;(2) a strategy by which both public and&nbsp;<strong>private sector partners</strong>&nbsp;can cooperate after 2021 to scale-out the use of precision sowing in MRD and other rice regions having&nbsp;similar conditions.",
    license = "Unknown",
    group = group,
    publication = NA,
    usecase_code = "USC018",
    usecase_name = "SEA-DSRC-Vietnam",
    activity = 'validation',
    carob_contributor = 'Eduardo Garcia Bendito',
    project = 'Excellence in Agronomy',
    data_type = "compilation", 
    carob_date = "2025-09-30",
    treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;seed_rate;planting_method",
    response_vars = "yield;labor_productivity",
    notes = NA
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Vietnam-DSRC-Validation/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "EiA_DSRC_Vietnam_Validation_clean_2018_2024.xlsx"]
  # Read relevant file
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    trial_id = as.character(r$`Trial ID`),
    country = "Vietnam",
    adm1 = r$Province,
    on_farm = ifelse(r$`Trial type (on-station; on-farm)` == "on-station", FALSE, TRUE),
    longitude = NA,
    latitude = NA,
    geo_from_source = FALSE,
    is_survey = FALSE,
    crop = "rice",
    
    planting_date = substr(r$Year, 1, 4),
    season = ifelse(r$Season == "W-S", "winter-spring",
                    ifelse(r$Season == "S-A", "summer-autumn", "autumn-winter")),
    residue_prevcrop_used = ifelse(r$Note == "Straw incorporated", TRUE, FALSE),
    previous_crop_burnt = ifelse(r$Note == "Straw burned", TRUE, FALSE),
    previous_crop_residue_management = r$Note,
    land_prep_method = ifelse(grepl("(drum seeder)", r$`Treatment to be used for meta analysis`), "not puddled", "puddled"),
    seed_rate = as.numeric(r[[11]]),
    planting_method = ifelse(grepl("transplanting", r$`Treatment to be used for meta analysis`), "transplanting",
                             ifelse(grepl("(drum seeder)", r$`Treatment to be used for meta analysis`), "mechanized",
                                    ifelse(grepl("DSR", r$`Treatment to be used for meta analysis`), "direct seeding", "manual"))),
    N_fertilizer = as.numeric(r[[12]]),
    P_fertilizer = as.numeric(r[[13]]) * 0.436,
    K_fertilizer = as.numeric(r[[14]]) * 0.83,
    yield = (as.numeric(r[[16]]) * 1000) * 0.8,
    yield_moisture = 20,
    yield_part = "grain",
    irrigated = FALSE,
    labor_productivity = as.numeric(r[[20]]),
    crop_price = as.numeric(r[[21]]) / (as.numeric(r[[16]]) * 1000) * 0.8, # This is actually net income...
    currency = "USD"
  )
  
  carobiner::write_files(meta, d, path = path)
  
}

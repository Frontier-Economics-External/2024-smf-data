## imports =====================================================================
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(readr)
library(janitor)
library(sf)
# library(lubridate)
# library(scales)

options(stringsAsFactors=FALSE)
options(scipen=999)
options(dplyr.show_progress=FALSE)
options(dplyr.summarise.inform=FALSE)

TIME_STAMP <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
THIS <- rstudioapi::getActiveDocumentContext()$path

## project parameters ==========================================================
config <- list(
  output_dir=here("output", "output v01"), #sprintf("output %s", TIME_STAMP)),
  data_dir=here("data"),
  draw_charts=TRUE,
  refresh_cache=FALSE
)

## code parameters =============================================================


## setup =======================================================================
dir.create("output", showWarnings=F) ## assumes you have this in config$output_dir
dir.create(config$output_dir, showWarnings=F)

## save copy of the script being run
file.copy(THIS, fe::fpath(config$output_dir, sprintf("_%s", basename(THIS))), overwrite=T)
## save details of who is running it and the packages used
data.frame(variable=names(Sys.info()), value=unname(Sys.info())) %>% 
  write.csv(fe::fpath(config$output_dir, "_sys_info.csv"), row.names=F)
capture.output(sessionInfo()) %>% 
  writeLines(fe::fpath(config$output_dir, "_session_info.csv"))

## check any paths specified
for(file_path in c()){
  if(!file.exists(file_path)) stop(sprintf("file path does not exist:\n%s", file_path))
}

## general functions ===========================================================
print. <- function(str) cat(sprintf("%s\n", str))

## run code ====================================================================
time_start <- Sys.time()
print.("start\n")


## import data =================================================================


## Import new england data
## want a list of school URNs and northing easting


schools_info <- read_csv(file.path(config$data_dir, "england_school_information.csv")) %>%
  clean_names %>%
  select(urn, schname, postcode)


## import establishment set


schools_loc <- read_csv(file.path(config$data_dir, "establishment.csv")) %>%
  clean_names %>%
  select(urn, 
         northing, 
         easting)






## 1. England deprivation ======================================================

eng_deprivation <- file.path(config$data_dir, "English indices of deprivation 2019/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx") %>% 
  readxl::read_excel(sheet = "IMD2019") %>% clean_names %>%
  select(
    lsoa_code_2011, 
    lsoa_name_2011,
    la_code_2019 = local_authority_district_code_2019,  
    la_name_2019 = local_authority_district_name_2019,  
    imd_rank = index_of_multiple_deprivation_imd_rank,
    imd_decile = index_of_multiple_deprivation_imd_decile
  ) 



## 2. Scotland deprivation =====================================================

scot_deprevation <- file.path(config$data_dir, "data/School+level+summary+statistics+2023.xlsx") %>% 
  readxl::read_excel(sheet = "2023 School Level Statistics", skip =1)


## 3. Welsh deprivation ========================================================
wales_deprivation <- file.path(config$data_dir, "data/Welsh_wmid.xlsx") %>% 
  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names %>%
  select(
    lsoa_code, ## no la codes here
    la_name = local_authority_name, 
    lsoa_name,
    wimd_2019
  )


## 4. IDACI of schools in England ==============================================
idaci_deprivation <- file.path(config$data_dir, "File_3_-_IoD2019_Supplementary_Indices_-_IDACI_and_IDAOPI.xlsx") %>% 
  readxl::read_excel(sheet = "IoD2019 IDACI & IDAOPI") %>% clean_names%>%
  select(
    lsoa_code_2011,
    lsoa_name_2011,
    la_code_2019 = local_authority_district_code_2019,  
    la_name_2019 = local_authority_district_name_2019,  
    imd_rank = index_of_multiple_deprivation_imd_rank_where_1_is_most_deprived,
    imd_decile = index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as,
    icai_rank = income_deprivation_affecting_children_index_idaci_rank_where_1_is_most_deprived,
    idaci_decile = income_deprivation_affecting_children_index_idaci_decile_where_1_is_most_deprived_10_percent_of_lso_as
  ) 


## merging the datasets 
#merged_data <- left_join(idaci_deprivation, eng_deprivation,  by = c("lsoa_code_2011", "lsoa_name_2011", "la_code_2019", "la_name_2019"))

# comparison_result <- merged_data %>%
#  mutate(is_equal = if_else(imd_rank.x == imd_rank.y, TRUE, FALSE)) %>%
#  summarise(all_equal = all(is_equal)) # all true, therefore only need idaci_deprivation :)




## 5. Number of students in school =============================================
pupils_data <- read_csv(file.path(config$data_dir, "data/spc_school_characteristics_.csv")) %>%
   select(
    time_period,  
    region_code,  
    region_name,
    old_la_code,
    la_code_2019 = new_la_code,  
    la_name_2019 = la_name,
    phase_type_grouping,
    admissions_policy, 
    headcount_of_pupils 
  )

## this is being subset by so many different things... 

## 6. FSM pupils ===============================================================
fsm_pupils_data <- read_csv(file.path(config$data_dir, "spc_pupils_fsm.csv")) %>%
  select(
    time_period,  
    region_code,  
    region_name,
    old_la_code,
    la_code_2019 = new_la_code,
    la_name_2019 = la_name, 
    phase_type_grouping,
    fsm, 
    headcount
    ) %>%
  filter(fsm == "Total")

## do we care about phase_type_grouping? lists the different types of schools... 


## 7. Stage 4 progress =========================================================
stage_4 <- read_csv(file.path(config$data_dir, "ks4_dm_ud_202122_la_rev.csv")) %>%
  select(
    time_period, 
    old_la_code, 
    la_code_2019 = new_la_code, 
    la_name_2019 = la_name, 
    institution_group,
    breakdown_topic, 
    breakdown, 
    data_type, 
    all_work, 
    further_educ = fe, 
    overall, 
    appl2, 
    appl3, 
    appren
         ) 

## 8. Attainment scores ========================================================
attainment <- read_csv(file.path(config$data_dir, "data-key-stage-4-performance.csv")) %>%
  select(
    time_period, 
    la_code_2019 = lad_code, 
    la_name_2019 = lad_name, 
    free_school_meals, 
    t_schools, 
    t_pupils,
    average_level8 = avg_att8
  )


    ## no indicator for phase type grouping :( 

## 9. A level results ==========================================================
a_level <- read_csv(file.path(config$data_dir, "data-a-level-and-other-16-to-18-results.csv")) %>%
  select(
    time_period, 
    region_code, 
    region_name, 
    old_la_code, 
    la_code_2019 = new_la_code, 
    la_name_2019 = la_name, 
    characteristic_type, 
    number_of_students_alev, 
    aps_per_entry_grade_alev
  )

## no indicator for phase type grouping :( 


## 10. Parliamentary constituency ==============================================
##TODO: add show_col_types to all read_csv (or make a wrapper function)
constituency <- read_csv(file.path(config$data_dir, "parliamentary_constituency_csv.csv"), show_col_types=FALSE) %>% 
  clean_names %>%
  select(
    time_period, 
    pcon_code, 
    pcon_name, 
    t_schools, 
    avg_att8
  )




## create output tables ========================================================
l <- list()

## CRS (Coordinate Reference System) reference, using EPSG(defunct European Petroleum Survey Group)
## also see https://www.ordnancesurvey.co.uk/documents/resources/guide-coordinate-systems-great-britain.pdf
## and https://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/

## British National Grid = BNG = OSGB 36 National Grid = EPSG: 27700
## WGS 84 = ETRS89 = EPSG: 4326 - this is LatLon with WGS84 datum used by GPS units and Google Earth 
l$schools <- schools_info %>%
  left_join(schools_loc,  by = c("urn")) %>% 
  filter(!is.na(easting) & !is.na(northing)) %>% ##TODO: locate via postcode instead
  st_as_sf(coords=c("easting", "northing"), crs=27700) %>% 
  st_transform(crs=4326)

## export ======================================================================
l %>% saveRDS(file.path(config$output_dir, "output.RDS"))




print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))



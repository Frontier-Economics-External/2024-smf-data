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


## Import data =================================================================


## 1.1 Import schools information ==============================================

schools_info <- read_csv(file.path(config$data_dir, "england_school_information.csv")) %>%
  clean_names %>%
  select(urn, schname, postcode, admission = admpol)


## 1.2. Import establishment set ===============================================

schools_loc <- read_csv(file.path(config$data_dir, "establishment.csv")) %>%
  clean_names %>%
  select(urn, 
         northing, 
         easting)


## 2. England deprivation ======================================================

eng_deprivation <- read_csv(file.path(config$data_dir, "england_imd_idaci.csv"), show_col_types=FALSE) %>%
  clean_names


## 3. Scotland deprivation =====================================================

scot_deprevation <- file.path(config$data_dir, "data/School+level+summary+statistics+2023.xlsx") %>% 
  readxl::read_excel(sheet = "2023 School Level Statistics", skip =1)


## 4. Welsh deprivation ========================================================
wales_deprivation <- file.path(config$data_dir, "data/Welsh_wmid.xlsx") %>% 
  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names %>%
  select(
    lsoa_code, ## no la codes here
    la_name = local_authority_name, 
    lsoa_name,
    wimd_2019
  )



## 5. Number of students in school =============================================
pupils_data <-  read_csv(file.path(config$data_dir, "updated/england_ks4final.csv"), show_col_types=FALSE) %>%
  clean_names %>% 
   select(
     urn, 
     schname, 
     postcode = pcode, 
     constituency_code = pcon_code, 
     constituency_name = pcon_name, 
     total_pupils = totpups, 
     fsm_share = ptfsm6cla1a, 
     attain_8_score = att8scr_fsm6cla1a)


## 5. A Level results ==========================================================
a_level <- read_csv(file.path(config$data_dir, "updated/england_ks5final.csv"), show_col_types = FALSE) %>%
  clean_names %>%
   select(
     urn, 
     schname, 
     a_level_score = tallppegrd_alev_dis) 


## 6. Appren/Uni ==== ==========================================================
next_stage <- read_csv(file.path(config$data_dir, "updated/england_ks5-studest-he.csv"), show_col_types = FALSE) %>%
  clean_names %>%
   select(
    urn, 
    schname, 
    progress_to_uni = dis_russell, 
    progress_to_appren = dis_appren) 




## merge step by step first to check all okay

# merge eng deprivation to postcode
schools_info <- schools_info %>% 
  left_join(eng_deprivation,  by = c("postcode")) 
## I get a warning here - check this


schools_info <- schools_info %>% 
  left_join(pupils_data, by = c("urn", "postcode", "schname"))


schools_info <- schools_info %>% 
  left_join(a_level, by = c("urn", "schname"))


schools_info <- schools_info %>% 
  left_join(next_stage, by = c("urn", "schname"))


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



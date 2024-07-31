## imports =====================================================================
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(readr)
library(janitor)
library(sf)
library(htmltools)
library(purrr)

options(stringsAsFactors=FALSE)
options(scipen=999)
options(dplyr.show_progress=FALSE)
options(dplyr.summarise.inform=FALSE)

TIME_STAMP <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
THIS <- rstudioapi::getActiveDocumentContext()$path
time_start <- Sys.time()

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
read_csv. <- function(data_file_name) {
  read.csv(file.path(config$data_dir, data_file_name)) %>% 
    clean_names() %>% 
    tibble()
}

check_key <- function(df, df_name, key_name) {
  str_id <- sprintf("Key column '%s' in table '%s'", key_name, df_name)
  if (!key_name %in% names(df)) {
    stop(sprintf("%s does not exist", str_id))
  }
  
  ## extract the column
  key_data <- df[[key_name]]
  
  ## check for uniqueness and no NAs
  not_unique <- length(unique(key_data)) < nrow(df)
  has_nas <- any(is.na(key_data))
  if(not_unique & has_nas){
    stop(sprintf("%s is not unique and has NAs", str_id))
  } else if (not_unique) {
    stop(sprintf("%s is not unique", str_id))
  } else if (has_nas) {
    stop(sprintf("%s has NAs", str_id))
  }
}

## Import data =================================================================
print.("importing")
i <- list()

## 1.1 Import schools information ==============================================

i$schools_info <- read_csv.("england_school_information.csv") %>%
  select(urn, schname, postcode, admission=admpol)


## 1.2. Import establishment set ===============================================

i$schools_loc <- read_csv.("establishment.csv") %>%
  select(urn, northing, easting)


## 2. England deprivation ======================================================

i$eng_deprivation <- read_csv.("england_imd_idaci.csv") %>% 
  distinct() ##TODO: there are duplicate postcodes: look into


## 3. Scotland deprivation =====================================================

## AL: commented out until cleaned - otherwise throws errors in the input check
# i$scot_deprevation <- file.path(config$data_dir, "data/School+level+summary+statistics+2023.xlsx") %>% 
#   readxl::read_excel(sheet = "2023 School Level Statistics", skip =1)


## 4. Welsh deprivation ========================================================
i$wales_deprivation <- file.path(config$data_dir, "data/Welsh_wmid.xlsx") %>% 
  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names %>%
  select(
    lsoa_code, ## no la codes here
    la_name = local_authority_name, 
    lsoa_name,
    wimd_2019
  )



## 5. Number of students in school =============================================
i$pupils_data <-  read_csv.("updated/england_ks4final.csv") %>%
  select(
     urn, 
     constituency_code = pcon_code, 
     constituency_name = pcon_name, 
     total_pupils = totpups, 
     fsm_share = ptfsm6cla1a, 
     attain_8_score = att8scr_fsm6cla1a) %>% 
  filter(!is.na(urn)) ##TODO: look into NA URNs


## 5. A Level results ==========================================================
i$a_level <- read_csv.("updated/england_ks5final.csv") %>%
   select(
     urn, 
     a_level_score = tallppegrd_alev_dis) %>% 
  filter(!is.na(urn)) ##TODO: look into NA URNs


## 6. Appren/Uni ==== ==========================================================
i$next_stage <- read_csv.("updated/england_ks5-studest-he.csv") %>%
   select(
    urn, 
    progress_to_uni = dis_russell, 
    progress_to_appren = dis_appren) %>% 
  filter(!is.na(urn)) ##TODO: look into NA URNs


## perform standard input checks ===============================================
for (df_name in names(i)){
  df=i[[df_name]]
  ## assuming one key and the first col
  key_name <- names(df)[1]
  check_key(df, df_name, key_name)
}
## create output data ==========================================================
print.("creating output data")
d <- list()

## CRS (Coordinate Reference System) reference, using EPSG(defunct European Petroleum Survey Group)
## also see https://www.ordnancesurvey.co.uk/documents/resources/guide-coordinate-systems-great-britain.pdf
## and https://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/

## British National Grid = BNG = OSGB 36 National Grid = EPSG: 27700
## WGS 84 = ETRS89 = EPSG: 4326 - this is LatLon with WGS84 datum used by GPS units and Google Earth 
d$schools <- i$schools_info %>%
  # left_join(eng_deprivation,  by = c("postcode")) %>% ## need to think about what to do with postcode
  left_join(i$pupils_data, by = c("urn")) %>% 
  left_join(i$a_level, by = c("urn")) %>% 
  left_join(i$next_stage, by = c("urn")) %>% 
  left_join(i$schools_loc,  by = c("urn")) %>% 
  filter(!is.na(easting) & !is.na(northing)) %>% ##TODO: locate via postcode instead
  st_as_sf(coords=c("easting", "northing"), crs=27700) %>% 
  st_transform(crs=4326)

d$display_names <- c(
  "urn"="URN",
  "schname"="School name",
  "postcode"="Postcode",
  "admission"="Admission",
  "constituency_code"="Consituency code",
  "constituency_name"="Constituency",
  "total_pupils"="Total pupils",
  "fsm_share"="FSM share",
  "attain_8_score"="Attain 8 score",
  "a_level_score"="A level score",
  "progress_to_uni"="Progress to Uni",
  "progress_to_appren"="Progress to apprenticeship"
)

d$labels <- d$schools %>% 
  st_drop_geometry() %>% 
  select(all_of(names(d$display_names))) %>% 
  gather(variable, value, -urn, -schname) %>% 
  mutate(variable_name = unname(d$display_names[variable])) %>% 
  group_by(urn) %>% 
  do({x <- .
    html <- paste(c(
      sprintf("<label class='control-label'>%s</label><br><strong>URN</strong>: %s", 
              unique(x$schname), unique(x$urn)),
      paste(sprintf("<strong>%s</strong>: %s", x$variable_name, x$value), collapse="<br>")
    ), collapse="<br>")
    data.frame(html_label=html)
  }) %>% 
  mutate(html_label = purrr::map(html_label, HTML))

## export ======================================================================
print.("exporting")
d %>% saveRDS(file.path(config$output_dir, "output.RDS"))




print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))



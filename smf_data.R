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
  output_dir=here("output", "output v02"), #sprintf("output %s", TIME_STAMP)),
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
read_csv. <- function(data_file_name, encoding="") {
  read.csv(file.path(config$data_dir, data_file_name), fileEncoding=encoding) %>% 
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
  filter(ispost16 == 1) %>%     ## want to keep only those schools which have 16+ year olds
  select(urn, schname, postcode, admission=admpol)


## 1.2. Import establishment set ===============================================
i$schools_loc <- read_csv.("establishment.csv") %>%
  select(urn, northing, easting)


## 1.3. Import Schools Contact info ============================================
i$schools_contact <- read_csv.("English schools list.csv", encoding="ISO-8859-1") %>%   
  mutate(head_teacher = paste(head_first_name, head_last_name)) %>% 
  select(urn, main_email, telephone_num, head_teacher)

## 2. England deprivation ======================================================

i$eng_deprivation <- read_csv.("england_imd_idaci.csv") %>% 
  distinct() 

# have checked that the duplicates are duplicated rows, and not duplicated postcodes. 


## 3. Scotland deprivation =====================================================

## AL: commented out until cleaned - otherwise throws errors in the input check
 #i$scot_deprevation <- file.path(config$data_dir, "data/School+level+summary+statistics+2023.xlsx") %>% 
  # readxl::read_excel(sheet = "2023 School Level Statistics", skip =1)
#this file does not have post codes, only at the local authority level, searching for data 

## 4. Welsh deprivation ========================================================
##i$wales_deprivation <- file.path(config$data_dir, "data/Welsh_wmid.xlsx") %>% 
#  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names %>%
#  select(
#    lsoa_code, ## no la codes here
#    la_name = local_authority_name, 
#    lsoa_name,
#    wimd_2019
#  )

# only at the lsoa code level, will search for data

## 5. Number of students in school =============================================
i$pupils_data <-  read_csv.("updated/england_ks4final.csv") %>%
  select(
     urn, 
     constituency_code = pcon_code, 
     constituency_name = pcon_name, 
     total_pupils = totpups, 
     fsm_share = ptfsm6cla1a, 
     attain_8_score = att8scr_fsm6cla1a) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data


## 5. A Level results ==========================================================
i$a_level <- read_csv.("updated/england_ks5final.csv") %>%
   select(
     urn, 
     a_level_score = tallppegrd_alev_dis) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data



## 6. Appren/Uni ==== ==========================================================
i$next_stage <- read_csv.("updated/england_ks5-studest-he.csv") %>%
   select(
    urn, 
    progress_to_uni = dis_russell, 
    progress_to_appren = dis_appren) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data


## 7. Post code lat / lon data =================================================
i$post_code_lat_long <-  read_csv.("postcode_long_lat.csv") %>%
  select(
    postcode, 
    pc_latitude=latitude, 
    pc_longitude=longitude)


## Import constituency shape file ==============================================
## using the latest constituencies
constituency_shape <- st_read(file.path(config$data_dir, "PCON_DEC_2020_UK_BFC.shp"), quiet=T) %>% 
  st_transform(crs=4326) %>% 
  select(constituency_code=PCON20CD, constituency_name=PCON20NM)


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


## test code to account for missing northing and easting
schools <- i$schools_info %>%
  left_join(i$schools_contact, by = c("urn")) %>%
  left_join(i$pupils_data, by = c("urn")) %>%
  left_join(i$a_level, by = c("urn")) %>%
  left_join(i$next_stage, by = c("urn")) %>%
  # left_join(i$eng_deprivation, by = c("postcode")) %>%
  left_join(i$schools_loc, by = c("urn")) %>%
  left_join(i$post_code_lat_long, by = c("postcode")) 

## add some geometry
with_easting_northing <- schools %>%
  filter(!is.na(easting) & !is.na(northing)) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(crs = 4326)

## if no location of school is available, use the postcode location
with_postcode_long_lat <- schools %>%
  filter((is.na(easting) | is.na(northing)) & !is.na(pc_longitude) & !is.na(pc_latitude)) %>%
  st_as_sf(coords = c("pc_longitude", "pc_latitude"), crs = 4326)

d$display_names <- c(
  "urn"="URN",
  "schname"="School name",
  "head_teacher"="Head Teacher",
  "main_email"="Email Address",
  "telephone_num"="Contact Number",
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
  # "idaci_decile" = "IDACI decile", 
  # "index_of_multiple_deprivation_decile" = "IMD decile"
)

d$na_codes <- c(
  "NP"="Not Provided",
  "NEW"="New school",
  "NE"="New school",
  "SUPP"="Withheld for confidentiality",
  "SP"="Withheld for confidentiality"
)


d$labels <- schools %>% 
  st_drop_geometry() %>% 
  ## attain_8_score appears to be a percentage, so adding it on here
  mutate(attain_8_score = if_else(is.na(attain_8_score) | attain_8_score=="" | attain_8_score %in% names(d$na_codes), 
                                  attain_8_score, paste0(attain_8_score, "%"))) %>% 
  select(all_of(names(d$display_names))) %>% 
  gather(variable, value, -urn, -schname) %>% 
  ## replace NA items with strings explaining them
  mutate(value = if_else(is.na(value) | value=="", "NA", value),
         value = if_else(value %in% names(d$na_codes), sprintf("NA (%s)", unname(d$na_codes[value])), value)) %>% 
  mutate(variable_name = unname(d$display_names[variable])) %>% 
  group_by(urn) %>% 
  do({x <- .
    html <- paste(c(
      sprintf("<div class='text-primary fw-bold' style='font-size: 1.2em;'>%s</div><br><strong>URN</strong>: %s", 
              unique(x$schname), unique(x$urn)),
      paste(sprintf("<strong>%s</strong>: %s", x$variable_name, x$value), collapse="<br>")
    ), collapse="<br>")
    data.frame(html_label=html)
  }) %>% 
  mutate(html_label = purrr::map(html_label, HTML))


a_level_score_levels <- c(
  "A+",
  "A",
  "A-",
  "B+",
  "B",
  "B-",
  "C+",
  "C",
  "C-",
  "D+",
  "D",
  "D-",
  "E+",
  "E",
  "E-",
  "x"
)
clean_na <- function(col) if_else(col %in% names(d$na_codes) | col=="", NA, col)
rem_perc <- function(col) gsub("%", "", col)
## combine geo locations and convert to numeric.
d$schools_point <- bind_rows(with_easting_northing, with_postcode_long_lat) %>% 
  select(-easting, -northing, -pc_longitude, -pc_latitude) %>% 
  ## doing each column explicitly for clarity and flexibility
  mutate(total_pupils = as.numeric(clean_na(total_pupils)),
         fsm_share = as.numeric(rem_perc(clean_na(fsm_share))) / 100,
         attain_8_score = as.numeric(clean_na(attain_8_score)) / 100, ## appears to be % even if not showing a % sign
         a_level_score = factor(clean_na(a_level_score), levels=rev(a_level_score_levels)), ## order from least to most to match the slider order in the map
         progress_to_uni = as.numeric(rem_perc(clean_na(progress_to_uni))) / 100,
         progress_to_appren = as.numeric(rem_perc(clean_na(progress_to_appren))) / 100) %>% 
  st_join(constituency_shape %>% select(within_code=constituency_code, within_name=constituency_name), left=F) %>% 
  left_join(constituency_shape %>% st_drop_geometry() %>%  select(constituency_code, match_name=constituency_name), by="constituency_code") %>% 
  ## for all matched shapes, accept the shape name
  mutate(constituency_name = if_else(is.na(match_name), constituency_name, match_name)) %>%
  ## for all NA constituencies, match to which one it sits within
  mutate(constituency_code = if_else(is.na(constituency_code) | constituency_code=="", 
                                     within_code, constituency_code),
         constituency_name = if_else(is.na(constituency_code) | constituency_code=="", 
                                     within_name, constituency_code)) %>% 
  select(-within_code, -within_name, -match_name)

check_constituency_shapes <- d$schools_point %>% 
  st_drop_geometry() %>% 
  left_join(constituency_shape %>% st_drop_geometry() %>% select(constituency_code, constituency_name2=constituency_name), by="constituency_code") %>% 
  filter(is.na(constituency_name2), !is.na(constituency_code), constituency_code!="")

if(nrow(check_constituency_shapes)){
  warning("There are some constituency codes in 'd$schools_point' which does not appear in 'constituency_shape'. This means that the map will be inconsistent")
}

## add to exports
d$constituency_shape <- constituency_shape

## Checking the NAs ===========================================================
## AL: the following is optionally executed by hand for any manual checks when
##     implementing new data
if(F){
  has_na_in_any_column <- d$schools_point %>%
    map_lgl(~ any(is.na(.x)))
  
  print(has_na_in_any_column)
  
  ## Checking the proportion of NAs
  schools_df <- d$schools_point %>% 
    st_drop_geometry()
  
  
  # Check NAs for these relevant variables
  columns_to_check <- c(
    "constituency_name",
    "total_pupils",
    "fsm_share",
    "attain_8_score",
    "a_level_score", 
    "progress_to_uni", 
    "progress_to_appren"
    # "idaci_decile", 
    # "index_of_multiple_deprivation_decile"
  )
  
  na_proportion <- function(x) {
    mean(is.na(x))
  }
  
  # Calculate the proportion of NAs for each column
  na_prop <- schools_df %>%
    select(all_of(columns_to_check)) %>%
    summarise(across(everything(), na_proportion))
  
  missing_geo_prop <- d$schools_point %>%
    filter(st_is_empty(geometry)) %>% 
    nrow() / nrow(d$schools_point)
}
## export ======================================================================
print.("exporting")
d %>% saveRDS(file.path(config$output_dir, "output.RDS"))




print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))


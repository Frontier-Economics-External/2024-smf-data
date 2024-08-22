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
library(stringr)

options(stringsAsFactors=FALSE)
options(scipen=999)
options(dplyr.show_progress=FALSE)
options(dplyr.summarise.inform=FALSE)

TIME_STAMP <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
THIS <- rstudioapi::getActiveDocumentContext()$path
time_start <- Sys.time()

## project parameters ==========================================================
config <- list(
  version="v04",
  data_dir=here("data"),
  draw_charts=TRUE,
  refresh_cache=FALSE
)

## code parameters =============================================================


## setup =======================================================================
dir.create("output", showWarnings=F) ## assumes you have this in config$output_dir
output_dir <- file.path("output", config$version)
dir.create(output_dir, showWarnings=F)

## save copy of the script being run
file.copy(THIS, fe::fpath(output_dir, sprintf("_%s", basename(THIS))), overwrite=T)
## save details of who is running it and the packages used
data.frame(variable=names(Sys.info()), value=unname(Sys.info())) %>% 
  write.csv(fe::fpath(output_dir, "_sys_info.csv"), row.names=F)
capture.output(sessionInfo()) %>% 
  writeLines(fe::fpath(output_dir, "_session_info.csv"))

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
  filter(minorgroup != "Independent school") %>% # remove independent schools
  filter(issecondary == 1) %>%     ## want to keep only those schools which have 16+ year olds
  select(urn, schname, postcode, admission=admpol,
         ispost16)


## 1.2. Import establishment set ===============================================
i$schools_loc <- read_csv.("establishment.csv") %>%
  filter(establishment_type_group_name != "Independent schools") %>%
  select(urn, northing, easting)


## 1.3. Import English Schools Contact info =====================================
i$schools_contact <- read_csv.("English schools list.csv", encoding="ISO-8859-1") %>%   
  mutate(head_teacher = paste(head_first_name, head_last_name)) %>% 
  select(urn, main_email, telephone_num, head_teacher)

## can filter for indep schools here too if needed. 

## 2. Import England deprivation ===============================================

i$eng_deprivation <- read_csv.("england_imd_idaci.csv") %>% 
  distinct() # duplicates are duplicated rows, not duplicated postcodes. 


## 3. Import Scotland data =====================================================

## 3.1. Import Scotland deprivation ============================================
 i$scot_deprivation <- file.path(config$data_dir, "data/School+level+summary+statistics+2023.xlsx") %>% 
   readxl::read_excel(sheet = "2023 School Level Statistics", skip =1) %>%
   filter(`School Type` == "Secondary") %>% clean_names() %>% 
   select(urn = seed_code,
          la_name = local_authority,
          schname = school_name, 
          total_pupils = pupil_roll, 
          fsm_share = percentage_of_p6_p7_s1_s6_sp_pupils_registered_for_free_school_meals_note_5,
          simd_quintile_1 = simd_quintile_1_note_6,
          simd_quintile_2 = simd_quintile_2_note_6,
          simd_quintile_3 = simd_quintile_3_note_6,
          simd_quintile_4 = simd_quintile_4_note_6,
          simd_quintile_5 = simd_quintile_5_note_6)


## 3.2. Import Scotland contacts and long/lat ==================================
i$scot_contact <- file.path(config$data_dir, "scot_SchoolRoll_2023.xlsx") %>% 
  readxl::read_excel(sheet = "Schools_Final") %>%
  filter(SchoolType == "Secondary") %>% clean_names() %>% 
  select(urn = seed_code,
         postcode = post_code, 
         pc_latitude = latitude, 
         pc_longitude = longitude,
         main_email = email, 
         telephone_num = phone)



## 4. Import Welsh data ========================================================

## 4.1. Import Welsh deprivation ===============================================
i$wales_deprivation <- file.path(config$data_dir, "data/Welsh_wmid.xlsx") %>% 
  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names %>%
  select(lsoa_code, 
         la_name = local_authority_name, 
         wimd_2019) 

## 4.2. Import Welsh fsm share =================================================
wales_fsm <- read_csv.("Welsh data/fsm_1.csv") %>% 
  clean_names %>% 
  select(la_name = name,
         fsm_share = x_of_secondary_school_pupils_who_are_eligible_for_free_school_meals_ay_21_22)


## 4.3. Import Schools location data ===========================================
wales_maint_loc <- file.path(config$data_dir, "Welsh data/address-list-schools-wales_0.xlsx") %>% 
  readxl::read_excel(sheet = "Maintained") %>% clean_names %>%
  filter(school_type == "Secondary (ages 11-19)") %>% 
  select(urn = school_number, 
         schname = school_name, 
         la_name = local_authority, 
         postcode, 
         telephone_num = phone_number,
         total_pupils = pupils_see_notes) %>% 
  mutate(telephone_num = str_replace_all(telephone_num, " ", "")
  )



## join Wales data to our list 
i$wales_deprivation <- wales_maint_loc %>% 
  left_join(i$wales_deprivation, by = "la_name")
## expect multiple matches, since wales_maint_loc does not have unique la_name


## 4.4. Aggregate wimd to la level, to add to map ==============================
wimd_weighted_average <- i$wales_deprivation %>%
  group_by(la_name) %>% 
  summarise(weighted_wimd = sum(wimd_2019 * total_pupils) /sum(total_pupils)) 
## using total_pupils as a proxy for la population

## merge back onto our list
i$wales_deprivation <- i$wales_deprivation %>%
  left_join(wimd_weighted_average, by = "la_name") %>%
  select(-lsoa_code, -wimd_2019) %>%
  distinct()


## 5. Number of students in English schools ====================================
i$pupils_data <-  read_csv.("updated/england_ks4final.csv") %>%
  select(
     urn, 
     constituency_code = pcon_code, 
     constituency_name = pcon_name, 
     total_pupils = totpups, 
     fsm_share = ptfsm6cla1a, 
     attain_8_score = att8scr_fsm6cla1a) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data


## 6. A Level results ==========================================================
i$a_level <- read_csv.("updated/england_ks5final.csv") %>%
   select(
     urn, 
     a_level_score = tallppegrd_alev_dis) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data



## 7. Appren/Uni ===============================================================
i$next_stage <- read_csv.("updated/england_ks5-studest-he.csv") %>%
   select(
    urn, 
    progress_to_uni = all_progressed, 
    progress_to_appren = all_appren,
    institution_type = nftype) %>% 
  mutate(institution_type = recode(institution_type,
                                   "AC" = "Academy Sponsor Led",
                                   "ACC" = "Academy Sponsor Led",
                                   "AC1619" = "Academy Sponsor Led", 
                                   "ACC1619" = "Academy 16-19 Converter",
                                   "CY" = "Community School", 
                                   "SS" = "Studio School", 
                                   "VC" = "Voluntary Controlled School", 
                                   "VA" = "Voluntary Aided School", 
                                   "F1619" = "Free School - 16-19", 
                                   "FD" = "Foundation School",
                                   "F" = "Free School", 
                                   "CTC" = "City Technology College")) %>%
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data


## 8. Post code lat / lon data =================================================
i$post_code_lat_long <-  read_csv.("postcode_long_lat.csv") %>%
  select(
    postcode, 
    pc_latitude=latitude, 
    pc_longitude=longitude)


## Import constituency shape file ==============================================
constituency_shape <- st_read(file.path(config$data_dir, "PCON_DEC_2020_UK_BFC.shp"), quiet=T) %>% 
  st_transform(crs=4326) %>% 
  select(constituency_code=PCON20CD, constituency_name=PCON20NM)


## Import local authority shape file ===========================================
local_authority_shape <- st_read(file.path(config$data_dir, "LAD_MAY_2024_UK_BFC.shp"), quiet = T) %>%
  st_transform(crs=4326)

# finding the centre point of local authorities 
local_authority_centre <- local_authority_shape %>%
  st_centroid()

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
  left_join(i$eng_deprivation, by = c("postcode")) %>%
  left_join(i$schools_contact, by = c("urn")) %>%
  left_join(i$pupils_data, by = c("urn")) %>%
  left_join(i$a_level, by = c("urn")) %>%
  left_join(i$next_stage, by = c("urn")) %>%
  left_join(i$schools_loc, by = c("urn")) %>%
  full_join(i$scot_deprivation, by =c("urn")) %>%
  left_join(i$scot_contact, by = c("urn")) %>%
  full_join(i$wales_deprivation, by = c("urn")) %>% 
  left_join(i$post_code_lat_long, by = c("postcode")) 






  
    ## using schools_check to add our data together 
schools_check <- schools %>%
  mutate(
    fsm_share.x = as.numeric(gsub("%", "", fsm_share.x)), 
    telephone_num.x = as.character(telephone_num.x),
    telephone_num.y = as.character(telephone_num.y),
    main_email.x = as.character(main_email.x),
    main_email.y = as.character(main_email.y),
    total_pupils.x = as.numeric(total_pupils.x),
    total_pupils.y = as.numeric(total_pupils.y),
    fsm_share.x = as.numeric(fsm_share.x),
    fsm_share.y = as.numeric(fsm_share.y),
    la_name.x = as.character(la_name.x),
    la_name.y = as.character(la_name.y),
    pc_latitude.x = as.numeric(pc_latitude.x),
    pc_latitude.y = as.numeric(pc_latitude.y),
    pc_longitude.x = as.numeric(pc_longitude.x),
    pc_longitude.y = as.numeric(pc_longitude.y)
  ) %>%
  mutate(
    schname = coalesce(schname.x, schname.y), 
    postcode = coalesce(postcode.x, postcode.y), 
    main_email = coalesce(main_email.x, main_email.y), 
    telephone_num = coalesce(telephone_num.x, telephone_num.y), 
    total_pupils = coalesce(total_pupils.x, total_pupils.y), 
    fsm_share = coalesce(fsm_share.x, fsm_share.y), 
    la_name = coalesce(la_name.x, la_name.y), 
    pc_latitude = coalesce(pc_latitude.x, pc_latitude.y), 
    pc_longitude = coalesce(pc_longitude.x, pc_longitude.y)
  ) %>%
  # remove original columns
  select(-schname.x, -schname.y, 
         -postcode.x, -postcode.y,
         -main_email.x, -main_email.y, 
         -telephone_num.x, -telephone_num.y, 
         -total_pupils.x, -total_pupils.y, 
         -fsm_share.x, -fsm_share.y, 
         -la_name.x, -la_name.y, 
         -pc_latitude.x, -pc_latitude.y, 
         -pc_longitude.x, -pc_longitude.y)



## add some geometry
with_easting_northing <- schools_check %>%
  filter(!is.na(easting) & !is.na(northing)) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(crs = 4326)

## if no location of school is available, use the postcode location
with_postcode_long_lat <- schools_check %>%
  filter((is.na(easting) | is.na(northing)) & !is.na(pc_longitude) & !is.na(pc_latitude)) %>%
  st_as_sf(coords = c("pc_longitude", "pc_latitude"), crs = 4326)

d$display_names <- c(
  "urn"="URN",
  "schname"="School name",
  "institution_type" = "School type",
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
  "progress_to_appren"="Progress to apprenticeship",
  "idaci_decile" = "IDACI decile", 
  "index_of_multiple_deprivation_decile" = "IMD decile", 
  "simd_quintile_1" = "Scottish IMD quintile 1", 
  "simd_quintile_2" = "Scottish IMD quintile 2", 
  "simd_quintile_3" = "Scottish IMD quintile 3", 
  "simd_quintile_4" = "Scottish IMD quintile 4", 
  "simd_quintile_5" = "Scottish IMD quintile 5", 
  "weighted_wimd" = "Welsh IMD")

d$na_codes <- c(
  "NP"="Not Provided",
  "NEW"="New school",
  "NE"="New school",
  "SUPP"="Withheld for confidentiality",
  "SP"="Withheld for confidentiality"
)


d$labels <- schools_check %>% 
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
         progress_to_appren = as.numeric(rem_perc(clean_na(progress_to_appren))) / 100, 
         ## add new data points here
         index_of_multiple_deprivation_decile = as.numeric(clean_na(index_of_multiple_deprivation_decile))
         ) %>% 
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
d %>% saveRDS(file.path(output_dir, sprintf("shiny_data_%s.RDS", config$version)))




print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))


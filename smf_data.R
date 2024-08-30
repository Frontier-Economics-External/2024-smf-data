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
  version="v05",
  data_dir=here("data")
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

## 0. Post code lat / lon data =================================================
i$post_code_lat_long <-  read_csv.("postcodes_lon_lat/postcode_long_lat.csv") %>%
  select(
    postcode, 
    pc_latitude=latitude, 
    pc_longitude=longitude) %>% 
  ## any fixes here
  # postcode pc_latitude pc_longitude
  # LE19 0AL        100.            0
  mutate(pc_latitude = if_else(postcode=="LE19 0AL", 52.583332, pc_latitude),
         pc_longitude = if_else(postcode=="LE19 0AL", -1.209052, pc_longitude))

pc_out_of_bounds <- i$post_code_lat_long %>% 
  filter(pc_latitude > 90 | pc_latitude < -90 | pc_longitude > 180 | pc_longitude < -180)
if(nrow(pc_out_of_bounds)) stop("some postcodes are not within lat/lon bounds")


## 1. Import England schools information =======================================

## 1.1. Import establishment set ===============================================
i$eng_schools <- read_csv.("england_school_information/establishment.csv", encoding="ISO-8859-1") %>%
  filter(establishment_type_group_name != "Independent schools") %>%
  filter(phase_of_education_name == "Secondary") %>%
  mutate(head_teacher = paste(head_first_name, head_last_name), 
         ispost16 = ifelse(statutory_high_age >= 16, 1, 0)) %>%
  select(urn, northing, easting, schname=establishment_name, 
         postcode, school_website, telephone_num, head_teacher, 
         admission=admissions_policy_name, ispost16)

## 1.2. Import England deprivation =============================================
i$eng_deprivation <- read_csv.("england_deprivation/england_imd_idaci.csv") %>% 
  rename(imd_decile=index_of_multiple_deprivation_decile) %>% 
  distinct() # duplicates are duplicated rows, not duplicated postcodes. 


## 1.3. Number of students in English schools ==================================
i$eng_pupils <-  read_csv.("england_pupils_info/england_ks4final.csv") %>%
  select(
    urn, 
    constituency_code = pcon_code, 
    constituency_name = pcon_name, 
    total_pupils = totpups, 
    fsm_share = ptfsm6cla1a, 
    attain_8_score = att8scr_fsm6cla1a) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data
#establishment data has total pupils and fsm share but not the other key variables
#so this import is still needed.

## 1.4. A Level results ========================================================
i$eng_a_level <- read_csv.("england_pupils_info/england_ks5final.csv") %>%
  select(
    urn, 
    a_level_score = tallppegrd_alev_dis) %>% 
  filter(!is.na(urn)) ##NA URNs are associated with aggregated local authority data



## 1.5. Appren/Uni =============================================================
i$eng_next_stage <- read_csv.("england_pupils_info/england_ks5-studest-he.csv") %>%
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

## 2. Import Scotland data =====================================================

## 2.1. Import Scotland deprivation ============================================
i$scot_deprivation <- file.path(config$data_dir, "scotland_deprivation/School+level+summary+statistics+2023.xlsx") %>% 
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
         simd_quintile_5 = simd_quintile_5_note_6) %>% 
  ## this is done to fit in the with english version but also to make filtering
  ## etc. more managable
  group_by(urn) %>% 
  do({d <- .
  quintile_counts <- d %>% select(starts_with("simd_quintile")) %>% 
      gather() %>% 
      mutate(value = as.numeric(if_else(value=="c", "0", value))) %>% 
      .$value
    all_quintiles <- rep(1:5, times = quintile_counts)
    d %>%
      mutate(simd_quintile=median(all_quintiles))
  }) %>% 
  ungroup() %>% 
  select(-starts_with("simd_quintile_")) %>% 
  mutate(imd_decile = simd_quintile * 2) ## i.e. we round up
## checked that all NAs match to a mix of c and 0 values


## 2.2. Import Scotland contacts and long/lat ==================================
i$scot_contact <- file.path(config$data_dir, "scotland_school_information/scot_SchoolRoll_2023.xlsx") %>% 
  readxl::read_excel(sheet = "Schools_Final") %>%
  filter(SchoolType == "Secondary") %>% clean_names() %>% 
  select(urn = seed_code,
         postcode = post_code, 
         pc_latitude = latitude, 
         pc_longitude = longitude,
         main_email = email, 
         telephone_num = phone,
         easting=grid_ref_easting, 
         northing=grid_ref_northing)


## 3. Import Welsh data ========================================================

## 3.1. Import Schools location data ===========================================

## some items will be mapped onto the lsoa of the school, we have the postcode
## for the school but will need to get the lsoa
wales_pc_lsoa_map <- file.path(config$data_dir, "wales_lsoa_map/Geography look ups - match postcodes to LSOA and Local Authority.xlsx") %>% 
  readxl::read_excel(sheet="Postcode_to_LSOA_and_LA_lookup", skip =2) %>% clean_names()

## In Wales, "maintained" schools refer to schools that are funded and controlled 
## by the local authority (also known as the local council). These schools follow 
## the national curriculum and do not charge fees for education. They are called 
## "maintained" because they are maintained (financially supported) by public funds, 
## which come from local taxes and the Welsh Government.
## So this is the Welsh equivalent of removing independent schools
wales_maint_loc <- file.path(config$data_dir, "wales_school_information/address-list-schools-wales_0.xlsx") %>% 
  readxl::read_excel(sheet = "Maintained") %>% clean_names() %>%
  filter(school_type == "Secondary (ages 11-19)") %>% 
  select(urn = school_number, 
         schname = school_name, 
         la_name = local_authority, 
         postcode, 
         telephone_num = phone_number,
         total_pupils = pupils_see_notes) %>% 
  mutate(telephone_num = str_replace_all(telephone_num, " ", "")) %>% 
  left_join(wales_pc_lsoa_map %>% select(postcode=pcds, lsoa_code), by="postcode")

missing_lsoas <- wales_maint_loc %>% 
  filter(is.na(lsoa_code))
if(nrow(missing_lsoas)) stop("the welsh postcode to lsoa map is incomplete")

## 3.2. Import Welsh deprivation ===============================================
wales_deprivation_lsoa <- file.path(config$data_dir, "wales_deprivation/Welsh_wmid.xlsx") %>% 
  readxl::read_excel(sheet = "Data", skip =3) %>% clean_names() %>%
  select(lsoa_code, 
         wimd_2019)

i$wales_deprivation <- wales_maint_loc %>% 
  left_join(wales_deprivation_lsoa, by="lsoa_code") %>% 
  mutate(imd_decile = floor(wimd_2019 / 10) + 1) # i.e. it is "within" the decile





## Import constituency shape file ==============================================
constituency_shape <- st_read(file.path(config$data_dir, "constituency_shapes/PCON_DEC_2020_UK_BFC.shp"), quiet=T) %>% 
  st_transform(crs=4326) %>% 
  select(constituency_code=PCON20CD, constituency_name=PCON20NM)


## re-add if required
# ## Import local authority shape file ===========================================
# local_authority_shape <- st_read(file.path(config$data_dir, "LAD_MAY_2024_UK_BFC.shp"), quiet = T) %>%
#   st_transform(crs=4326)
# 
# # finding the centre point of local authorities 
# local_authority_centre <- local_authority_shape %>%
#   st_centroid()

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
schools_england <- i$eng_schools %>%
  left_join(i$eng_pupils, by = c("urn")) %>%
  left_join(i$eng_a_level, by = c("urn")) %>%
  left_join(i$eng_next_stage, by = c("urn")) %>%
  left_join(i$eng_deprivation, by = c("postcode")) %>% 
  mutate(telephone_num = as.character(telephone_num),
         ispost16 = if_else(ispost16==1, "Yes", if_else(ispost16==0, "No", NA)))


schools_scotland <- i$scot_deprivation %>% 
  left_join(i$scot_contact, by = c("urn")) %>% 
  select(urn,
         schname,
         postcode,
         main_email,
         telephone_num,
         total_pupils,
         fsm_share,
         imd_decile,
         northing,
         easting) %>% 
  mutate(admission = NA,
         ispost16 = NA,
         head_teacher = NA,
         constituency_code = NA,
         constituency_name = NA,
         attain_8_score = NA,
         a_level_score = NA,
         progress_to_uni = NA,
         progress_to_appren = NA,
         institution_type = NA,
         idaci_decile = NA) %>% 
  mutate(telephone_num = if_else(telephone_num=="tbc", NA, telephone_num),
         total_pupils = as.character(total_pupils),
         fsm_share = if_else(fsm_share=="c", NA, fsm_share))
  
schools_wales <- i$wales_deprivation %>% 
  select(urn,
         schname,
         postcode,
         telephone_num,
         total_pupils,
         imd_decile) %>% 
  mutate(main_email=NA,
         admission = NA,
         ispost16 = NA,
         head_teacher = NA,
         constituency_code = NA,
         constituency_name = NA,
         attain_8_score = NA,
         a_level_score = NA,
         progress_to_uni = NA,
         progress_to_appren = NA,
         institution_type = NA,
         northing = NA, ## we only have postcode for Welsh schools
         easting = NA,
         fsm_share = NA,
         idaci_decile = NA) %>% 
  mutate(total_pupils = as.character(total_pupils))


schools <- bind_rows(
  schools_england %>% mutate(country="England"), 
  schools_scotland %>% mutate(country="Scotland"), 
  schools_wales %>% mutate(country="Wales")) %>% 
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
  "institution_type" = "School type",
  "ispost16" = "Sixth form",
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
  "imd_decile" = "IMD decile", 
  "idaci_decile" = "IDACI decile"
)

d$display_desc <- c(
  "urn"="Unique Reference Number for the school",
  "schname"="The full name of the school",
  "institution_type"="The type of institution",
  "ispost16"="Indicates whether the school has a sixth form",
  "head_teacher"="The name of the head teacher",
  "main_email"="The main contact email for the school",
  "telephone_num"="The primary phone number for the school",
  "postcode"="The school's postal code",
  "admission"="The school's admission policy or type",
  "constituency_code"="The code for the school's parliamentary constituency",
  "constituency_name"="The name of the parliamentary constituency",
  "total_pupils"="The total number of enrolled pupils",
  "fsm_share"="Percentage of pupils eligible for Free School Meals",
  "attain_8_score"="The school's average attainment score across 8 subjects",
  "a_level_score"="The average A level score for the school",
  "progress_to_uni"="Percentage of students progressing to university",
  "progress_to_appren"="Percentage of students progressing to apprenticeships",
  "imd_decile"="The school's decile ranking on the Index of Multiple Deprivation",
  "idaci_decile"="The school's decile ranking on the Income Deprivation Affecting Children Index"
)

d$notes <- c(
  "urn"="In Scotland this is 'seed_code', in Wales it is 'school_number'",
  "schname"="",
  "institution_type"="Only available in England",
  "ispost16"="Only available in England",
  "head_teacher"="Only available in England",
  "main_email"="Not available in Wales",
  "telephone_num"="",
  "postcode"="",
  "admission"="Only available in England",
  "constituency_code"="Given directly (with some missing) for England. For all others, membership is inferred from school location within constiuency borders GIS data",
  "constituency_name"="",
  "total_pupils"="", 
  "fsm_share"="Not available in Wales",
  "attain_8_score"="Only available in England",
  "a_level_score"="Only available in England",
  "progress_to_uni"="Only available in England",
  "progress_to_appren"="Only available in England",
  "imd_decile"="Scotland provides number of students per quintile per school. We have calculated the overall school quintile by taking the median student. We then multiply this by 2 to get deciles, so note that Scotland will not have odd deciles. For Wales, a 2 decimal point decile is given. We round this up to indicate the decile it is part of.",
  "idaci_decile"="Only available in England"
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
         is_na = value %in% c("NA", names(d$na_codes)),
         value = if_else(value %in% names(d$na_codes), sprintf("NA (%s)", unname(d$na_codes[value])), value)) %>% 
  mutate(variable_name = unname(d$display_names[variable]),
         colour = if_else(is_na, "#A9A9A9", "#000000")) %>% 
  left_join(tibble(variable=names(d$display_desc), 
                   tool_tip=gsub("'", "&#39;", unname(d$display_desc))), by="variable") %>% 
  group_by(urn) %>% 
  do({x <- .
    html <- paste(c(
      sprintf("<div class='text-primary fw-bold' style='font-size: 1.2em;'>%s</div>", 
              unique(x$schname)),
      sprintf("<span data-tooltip='%s' style='color: #000000;'><strong>URN</strong>: %s</span>", 
              unname(d$display_desc["urn"]), unique(x$urn)),
      paste(sprintf("<span data-tooltip='%s' style='color: %s;'><strong>%s</strong>: %s</span>", 
                    x$tool_tip, x$colour, x$variable_name, x$value), collapse="<br>")
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
  "E-"
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
         imd_decile = as.numeric(clean_na(imd_decile)),
         idaci_decile = as.numeric(clean_na(idaci_decile)),
         admission = if_else(admission %in% c(""), NA, admission)) %>% 
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

## add to exports if needed
# d$constituency_shape <- constituency_shape

## add meta data details
d$variable_info <- tibble(variable=names(d$display_names), display_name=unname(d$display_names)) %>% 
  left_join(tibble(variable=names(d$display_desc), description=unname(d$display_desc)), by="variable") %>% 
  left_join(tibble(variable=names(d$notes), notes=unname(d$notes)), by="variable")

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

## sources =====================================================================
d$sources <- read_file("sources.md")


## export ======================================================================
print.("exporting")
d %>% saveRDS(file.path(output_dir, sprintf("shiny_data_%s.RDS", config$version)))




print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))


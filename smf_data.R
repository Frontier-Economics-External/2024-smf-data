## imports =====================================================================
rm(list=ls());gc()
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
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
  output_dir=here("output", sprintf("output %s", TIME_STAMP)),
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








print.(sprintf("finished. the analysis took %s minutes to complete",
               round(as.numeric(Sys.time() - time_start, units = "mins"))))
# How to run this code
Code should be opened in R studio by double clicking on the `smf_data.Rproj` file. This will load the project. It is assumed that this code is run on a local machine. This code is not used in the deployment of the map, it is only used to create a single data file that the map will use (once we manually copy it across).

**Before you run, make sure you change the version**. This will mean that you are not overwriting data. This is essential to be able to re-produce any data that was being used previously. The code will save the output and the code being run into a version folder to ensure this. However if you make changes and re-run without changing the version then you will have lost this information. Change the version by changing the value of the `version` parameter in the `config` list. For example:

```
## project parameters ==========================================================
config <- list(
  version="v04",
  data_dir=here("data")
)
```
Changing the version before we make any changes (from `v04` to `v05`):
```
## project parameters ==========================================================
config <- list(
  version="v05",
  data_dir=here("data")
)
```
you should then source the `smf_data.R` code. (either `shift+ctrl_s` or click the `source` button in the top right of the code pane in Rstudio). This will confirm that the code runs without errors and you are writing to a new version folder (in the example above, this would write to `output/v05` after the change).


# Updating data
Data is stored in the data directory of this repository. This is not fixed and can be changed by adjusting the path of the `data_dir` parameter in the `config` list (see above). The process of updating data is:

First, copy the data file (excel, csv etc.) into the same location as the file it is replacing. Call it something different so as not to overwrite the old data file. In each folder there is a `source.txt` file which identifies where this data came from - make sure this is updated to reflect where you got the updated data from (this ensures everything is traceable).

If the structure of data does not change then updating input data is relatively easy. You simply find where the previous file was being referenced (search for its name) and replace the text with the new file name.

If the structure of the data has changed then some amount of coding may be required. Hopefully it should just be a case of cleaning it slightly differently (different column names etc) to end up with the same structure that the code uses. 

Lastly, make sure that all the info that we show on the map is correct, this should just be:

1. check that the notes (`d$notes`) is still valid - this should not change unless the methodology or availability of data changes; and
1. check that the sources markdown file is up to date and correct. You can open this in R and click the "preview" button at the top to see what your markdown will look like within the map.

Once data is updated, then everything should be sourced (as above) to confirm that everything runs through from beginning to end.


### update_landing_page.R ###############################################

# A selection of functions used to update ESS-DIVE landing pages

# We acknowledge the River Corridors SFA Data Management Team for providing this script. 

### Prep Script ################################################################

# load libraries
library(tidyverse)
library(rlog)
library(officer) # for reading in docx files
library(readxl) # for reading in excel files
library(janitor) # for cleaning up col headers
library(glue)
library(devtools) # for sourcing in script
library(jsonlite) # for converting to json-ld file
library(httr) # for uploading to the API

# load functions
source_url("https://raw.githubusercontent.com/river-corridors-sfa/rcsfa-data_processing_for_publication/refs/heads/main/Data_Transformation/functions/rename_column_headers.R")
source_url("https://raw.githubusercontent.com/river-corridors-sfa/rcsfa-data_processing_for_publication/refs/heads/main/Data_Package_ESS-DIVE/update_ESS-DIVE_landing_page/functions/update_landing_page_authors.R")
source_url("https://raw.githubusercontent.com/river-corridors-sfa/rcsfa-data_processing_for_publication/refs/heads/main/Data_Package_ESS-DIVE/update_ESS-DIVE_landing_page/functions/update_landing_page_coordinates.R")


### Updating Authors ###########################################################
# For detailed information on the function that uses the API to update the
# landing page, refer to the `README_update_ESS-DIVE_landing_page_authors.md`
# file. This script also includes two internal "helper" functions developed for
# the RC-SFA DM Team. These functions extract author lists from the ESS-DIVE
# metadata and merge them with the data stored in the author spreadsheet.


# USER INPUTS
your_author_spreadsheet <- "2-code/ess-dive_submission/authors.xlsx"
your_api_token = "" # this is your personal token that you can get after signing into ess-dive; recommend adding this in the console
your_essdive_id = "ess-dive-23eeddf72ccce8f-20250602T204652536" # id that begins with "ess-dive-" found on the landing page you want to update
your_upload_site = "main" # options: c("sandbox", "main")
## NOTE that the ESS-DIVE ID will change each time the dataset is updated

# RUN functions (no modifications needed)
# get author info from spreadsheet
author_info <- read_excel(your_author_spreadsheet)


# update landing page - warning this will overwrite all existing authors with the new ones you provide
update_landing_page_authors(api_token = your_api_token,
                            author_df = author_info,
                            essdive_id = your_essdive_id,
                            upload_site = your_upload_site)


### Updating Coordinates #######################################################
# For detailed information on the function that uses the API to update the
# landing page, refer to the `README_update_ESS-DIVE_landing_page_coordinates.md`
# file. 


# USER INPUTS
your_coordinates_file_path <- "2-code/ess-dive_submission/coordinates.csv" # this is the .csv absolute file path of the coordinates (required cols: Description, Latitude, Longitude)
your_api_token = "" # this is your personal token that you can get after signing into ess-dive; recommend adding this in the console
your_essdive_id = "ess-dive-0b67a6aabb56d8f-20250916T232425678283" # id that begins with "ess-dive-" found on the landing page you want to update
your_upload_site = "main" # options: c("sandbox", "main")
## NOTE that the ESS-DIVE ID will change each time the dataset is updated

# RUN function (no modifications needed)
# update landing page - warning this will overwrite all existing coordinates with the new ones you provide
update_landing_page_coordinates(api_token = your_api_token,
                                essdive_id = your_essdive_id,
                                coordinates_file_path = your_coordinates_file_path,
                                upload_site = your_upload_site)

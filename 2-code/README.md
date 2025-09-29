## 2-code 

This subdirectory contains code for processing the data. These scripts are called in the `targets` workflow (see `run.R` and `_targets.R`) and should not be run as stand-alones.


| file | description |
|---|---|
|`0-packages.R`                             | packages used in this workflow. also contains custom ggplot theme for the figures.|
|`1-initial_processing.R`                   | download metadata files from GoogleDrive. Users must have access to the Google folder to run this script. Run this only once, to download/update sample metadata. |
|`2-functions_processing.R`                 | functions to process and clean raw data |
|`3a-download_soil_moisture_sensor_data.R`  | download data from TEROS-12 and AquaTROLL sensors. Users must have access to the Google folder to run this script. Run this only once, to download/update data. |
|`3b-functions_soil_moisture_sensors.R`     | functions to process the sensor data |


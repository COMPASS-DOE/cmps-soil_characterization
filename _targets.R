# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
source("2-code/0-packages.R")
#source("2-code/1-initial_processing.R")
source("2-code/a-functions_processing.R")
source("2-code/b-functions_analysis.R")
source("2-code/c-functions_fticrrr.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # sample metadata
  tar_target(sample_key_data, "1-data/sample_key.csv", format = "file"),
  tar_target(sample_key, read.csv(sample_key_data)),
  tar_target(subsampling_data, "1-data/subsampling_weights.csv", format = "file"),
  tar_target(subsampling, read.csv(subsampling_data)),
  tar_target(analysis_key_data, "1-data/analysis_key.csv", format = "file"),
  tar_target(analysis_key, read.csv(analysis_key_data)),
  
  # data files
  tar_target(moisture_data, import_gsheet("1nYzExPmpv01IYalqo2L3pmDsBDvhjdvHYMJo31N5_MA")),
  tar_target(moisture_processed, process_moisture(moisture_data)),
  tar_target(loi_data, import_loi(FILEPATH = "1-data/loi")),
  tar_target(loi_processed, process_loi(loi_data)),
  tar_target(pH_data, import_gsheet("177ZR7O9JpsgVmFfLh72LfJ72oM-oTc6k_r1qE0XHL64")),
  tar_target(pH_processed, process_pH(pH_data)),
  tar_target(tctn_data, import_tctn_data(FILEPATH = "1-data/tctnts")),
  tar_target(ts_data, import_ts_data(FILEPATH = "1-data/tctnts")),
  tar_target(tctnts_data_samples, process_tctnts(tctn_data, ts_data)),
  tar_target(weoc_data, import_weoc_data(FILEPATH = "1-data/npoc", PATTERN = "Summary_Raw")),
  tar_target(weoc_processed, process_weoc(weoc_data, analysis_key, moisture_processed, subsampling)),
  tar_target(dic_data, import_dic_data(FILEPATH = "1-data/dic")),
  tar_target(dic_processed, process_dic(dic_data, analysis_key, moisture_processed, subsampling)),
  #tar_target(din_data, readxl::read_xlsx("1-data/din/223013 Patel Final Report.xlsx", sheet = "NO3-N and NH4-N data")),
  tar_target(din_data, import_din_data("1-data/din")),
  tar_target(din_processed, process_din(din_data, analysis_key, moisture_processed, subsampling)),
  tar_target(icp_data, import_icp_data(FILEPATH = "1-data/icp")),
  tar_target(icp_processed, process_icp(icp_data, analysis_key, moisture_processed, subsampling)),
  #tar_target(cec_processed, compute_cec(icp_processed)),
  tar_target(ferrozine_data, import_iron(FILEPATH = "1-data/iron-ferrozine")$ferrozine_data),
  tar_target(ferrozine_map, import_iron(FILEPATH = "1-data/iron-ferrozine")$ferrozine_map),
  tar_target(ferrozine_processed, process_iron(ferrozine_map, ferrozine_data, moisture_processed, subsampling)),
  tar_target(mehlich_map, import_mehlich(FILEPATH = "1-data/phosphorus-mehlich")$mehlich_map),
  tar_target(mehlich_data, import_mehlich(FILEPATH = "1-data/phosphorus-mehlich")$mehlich_data),
  tar_target(mehlich_processed, process_mehlich(mehlich_map, mehlich_data, moisture_processed, subsampling)),
##  tar_target(icr_report, import_fticr_data(FILEPATH = "1-data/icr")),
##  tar_target(icr_meta, make_fticr_meta(icr_report)$meta2),
##  tar_target(icr_data_long, make_fticr_data(icr_report, analysis_key, sample_key)$data_long_blank_corrected),
##  tar_target(icr_data_trt, make_fticr_data(icr_report, analysis_key, sample_key)$data_long_trt),
##  tar_target(icr_relabund_samples, compute_icr_relabund(icr_data_long, icr_meta)),
  tar_target(ions_data, import_ions(FILEPATH = "1-data/ions")),
  tar_target(ions_processed, process_ions(ions_data, analysis_key, sample_key, moisture_processed, subsampling)$samples3),
  tar_target(ions_processed_meq, process_ions(ions_data, analysis_key, sample_key, moisture_processed, subsampling)$samples_meq),
  tar_target(xrd_data, import_xrd(FILEPATH = "1-data/xrd")),
  tar_target(xrd_processed, process_xrd(xrd_data, sample_key)),
  tar_target(wrc_data, import_wrc_data(FILEPATH = "1-data/wrc")),
  tar_target(wrc_processed, process_wrc(wrc_data)),
  tar_target(hydrometer_data, "1-data/particle_size.csv", format = "file"),
  tar_target(hydrometer_df, read.csv(hydrometer_data)),
  tar_target(texture_processed, compute_texture(hydrometer_df)),
  tar_target(gg_wrc, plot_wrc(wrc_processed)),
  tar_target(gg_texture, plot_texture(texture_processed)),
  
  # analysis - graphs
  ## tar_target(gg_moisture, plot_moisture(moisture_processed, sample_key)),
  ## tar_target(gg_loi, plot_loi(loi_processed, sample_key)),
  ## tar_target(gg_pH, plot_pH(pH_processed, sample_key)),
  ## tar_target(gg_sp_cond, plot_sp_cond(pH_processed, sample_key)),
  ## tar_target(gg_tctnts, plot_tctnts(tctnts_data_samples, sample_key)),
  ## tar_target(gg_weoc, plot_weoc(weoc_processed, sample_key)),
  ## tar_target(gg_din, plot_din(din_processed, sample_key)),
  ## tar_target(gg_icp, plot_icp(icp_processed, sample_key)),
  ## tar_target(gg_ferrozine, plot_ferrozine(ferrozine_processed, sample_key)),
  ## tar_target(gg_mehlich, plot_mehlich(mehlich_processed, sample_key)),
##   tar_target(gg_icr_vankrevelen, plot_vankrevelen(icr_data_trt, icr_meta)),
##   tar_target(gg_icr_unique, plot_vankrevelen_unique(icr_data_trt, icr_meta)),
##   tar_target(gg_icr_pca, compute_icr_pca(icr_relabund_samples, sample_key)),
  ## tar_target(gg_ions, plot_ions(ions_processed, sample_key)),
  
  # combined data
  tar_target(data_combined_all_horizons, combine_data(moisture_processed, pH_processed, tctnts_data_samples, loi_processed,
                                         weoc_processed, dic_processed, din_processed, icp_processed,
                                         ferrozine_processed, mehlich_processed, ions_processed_meq, 
                                         sample_key)),
  tar_target(data_combined, subset_surface_horizons(data_combined_all_horizons)),
  tar_target(analysis_completion_matrix, compute_analysis_matrix(data_combined)),
  tar_target(gg_pca_all, compute_overall_pca(data_combined, sample_key)),
  tar_target(gg_correlations, compute_correlations(data_combined, sample_key)),
  
  tar_target(gg_by_transect_colorsites, make_graphs_by_transect_SITE_AS_COLOR(data_combined)),
  tar_target(gg_by_transect, make_graphs_by_transect(data_combined)),
  tar_target(gg_by_site_oa, make_graphs_by_site(data_combined %>% filter(horizon != "B"))),
  tar_target(gg_by_site_oab, make_graphs_by_site(data_combined)),
  tar_target(gg_xrd, plot_xrd(xrd_processed)),
  
  tar_target(summary_tables, make_summary_tables(data_combined)),
  
  # export
  tar_target(export, {
    write.csv(data_combined, "1-data/processed/chemistry_combined_surface_horizon.csv", row.names = FALSE)
    write.csv(data_combined_all_horizons, "1-data/processed/chemistry_combined_all_horizons.csv", row.names = FALSE)
##    write.csv(icr_meta, "1-data/processed/icr_meta.csv", row.names = FALSE)
##    crunch::write.csv.gz(icr_data_long, "1-data/processed/icr_long_all_samples.csv.gz", row.names = FALSE)
##    crunch::write.csv.gz(icr_data_trt, "1-data/processed/icr_long_treatments.csv.gz", row.names = FALSE)
    
  }, format = "file"),
   
   
   # report  
   tar_render(report, path = "3-reports/characterization_report.Rmd")
  
)

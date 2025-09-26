## Transition zones at the changing coastal terrestrial-aquatic interface

This repository contains data and code for the manuscript "Transition zones at the changing coastal terrestrial-aquatic interface" by Patel et al. 2025.

---

### Abstract
Coastal soils are a significant but highly uncertain component of global biogeochemical cycles. 
These systems experience spatial and temporal variability in biogeochemical processes, driven by marsh-to-upland gradients and hydrological fluctuations. 
These fluctuations make it difficult to understand and predict biogeochemical processes in these highly dynamic systems. 
We studied coastal soil biogeochemistry and its variability (a) at regional scales and (b) across transects from upland forest to marsh, in two contrasting regions — 
Lake Erie, a freshwater lacustrine system, and Chesapeake Bay, a saltwater estuarine system. 
Salinity-related analytes were a key source of variability in soil biogeochemistry, not just in the saltwater system, but surprisingly, also in the freshwater system. 
We had hypothesized linear trends in biogeochemical parameters along the TAI – 
however, contrary to expectations, transition soils were not consistently intermediate between upland and marsh endmembers; 
the non-monotonic trends of C, P, Fe along our transects suggest that these do not behave as expected and may be difficult to model and predict – 
thus these are key analytes to study in our regions. 
Rapidly changing soil factors across coastal gradients (e.g., Ca, K, CEC, and TS) may act as precursors to ecosystem shifts. 
Our comprehensive soil characterization represents a snapshot of a single timepoint of surface soils and provides essential data for mechanistic modeling of ecosystem dynamics across coastal transects.


<img align = "center" height = "400" src="https://github.com/COMPASS-DOE/cmps-soil_characterization/blob/main/3-reports/manuscript_figures_files/figure-gfm/map-1.png">

---

### Directory structure

```

home
|
|----- 1-data/
|         |----- raw/
|         |----- processed/
|         |----- soil_moisture_sensors/
|         |----- sample_key.csv
|         |----- analysis_key.csv
|         |----- synoptic_elevations.csv
|
|----- 2-code/
|
|----- 3-reports/
|
|----- cb_2018_us_state_5m/
|
|----- cmps-soil_characterization.Rproj
|----- README.md
|----- _targets.R
|----- run.R

```

We used the `{targets}` package for reproducible analytical workflows ([Landau 2021](https://doi.org/10.21105/joss.02959)). 
The workflow contains targets stored in `_targets.R`, which is run though `run.R`, both in the parent directory. 
The `_targets.R` file pulls data from `1-data/` and functions from `2-code/`, and the final graphs are rendered in the `3-reports/` subdirectory. 

## Funding
This work was supported through the Field, Measurements, and Experiments (FME) component of the 
Coastal Observations, Mechanisms, and Predictions Across Systems and Scales (COMPASS) program (https://compass.pnnl.gov/). 
COMPASS-FME is a multi-institutional project supported by the US Department of Energy, Office of Science, Biological and Environmental Research 
as part of the Environmental System Science Program.

A portion of this research was performed on project awards ([60602](https://www.osti.gov/award-doi-service/biblio/10.46936/cpcy.proj.2022.60602/60008672), 
[60511](https://www.osti.gov/award-doi-service/biblio/10.46936/cpcy.proj.2022.60511/60008517), [61038](https://www.osti.gov/award-doi-service/biblio/10.46936/cont.proj.2023.61038/60012321)) 
from the Environmental Molecular Sciences Laboratory, 
a DOE Office of Science User Facility sponsored by the Biological and Environmental Research program under Contract No. DE-AC05-76RL01830.


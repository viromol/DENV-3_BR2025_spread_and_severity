Repository Structure

📁 Spread/
This folder contains BEAST input and output files used for phylogeographic reconstruction.

- Time-scaled phylogenetic tree (without ancestral state reconstruction)
D3III_B32_allBR_augur5cm.xml — BEAST configuration file
D3III_B32_allBR_augur5cm.comb.log — Combined log file
D3III_B32_allBR_augur5cm.comb1k.trees — Posterior time-scaled trees used as input for the phylogeographic analysis

- Phylogeographic analysis (complete BEAST outputs)
FiloGeo_locsAll_SymBSSVS_OpEd.xml — BEAST configuration file for phylogeographic inference
FiloGeo_locsAll_SymBSSVS.log — MCMC log file
FiloGeo_locsAll_SymBSSVS.trees — Posterior tree distribution
FiloGeo_locsAll_SymBSSVS.MCC.tree — Maximum clade credibility (MCC) tree
FiloGeo_locsAll_SymBSSVS.location.history.trees — Trees annotated with location history
FiloGeo_locsAll_SymBSSVS.location.rates.log — Estimated migration rates between locations
FiloGeo_locsAll_SymBSSVS.MJ.history.txt — Markov jump history file

📁 Severity/
This folder contains scripts and datasets used for dengue severity analysis.

- Scripts
SINAN_datasets.R — R script to download and preprocess SINAN data
script_logistic_and_mixed_models.R — R script implementing logistic and mixed-effects models to investigate predictors of dengue severity

- Data
dataset_all_states.csv — Dataset used in severity analyses
monthly_notification_rate.csv — Monthly dengue notification rates
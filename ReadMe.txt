# MI_Wavelet_analysis  Code Repository

This repository contains the raw data and codes used to produce the results presented in the paper entitled: "Scale-Dependent Controls of Nighttime Net Ecosystem Exchange in Sub-Humid West African Ecosystems", currently submitted to the journal: "JGR biogeosciences".

## Repository Structure

### Raw Data subfolder

This subfolder contains raw data sets used in this study

### Processing subfolder

This subfolder contains each stage of the analysis pipeline:

#### Data Processing and Gap-Filling
- NEE_data_post_process.r - Data preparation
- Mutual_information_analysis.r - Analysis performed using mutual information
- NEE_data_MDS_gap_filling.r - Gap-filling using marginal distribution sampling (MDS)
- NEE_data_RF_gap_filling.r - Gap-filling of long gaps performed using Random Forest (RF)

#### Wavelet Analysis
- NEE_data_RF_gap_filling.r - Data preparation for coherence analysis
- Global_power_analysis.m - Global wavelet power analysis
- Coherence_analysis.m - Wavelet coherence analysis
- Phase_analysis.m - Phase difference analysis using wavelets

#### Dependencies
Associated dependencies for running the analysis scripts are also included in this subfolder. 


### Figures Directory

The code for generating the figures presented in the article can be found in the Figures subfolder.
For the wavelet transform analysis the codes used source codes shared by  Grinsted et al. (2004).


## Contact

[ ]
# BF591 Final Project

## Overview
This repo contains my final project for BF591. This Rshiny application was created to visualize and analyze differential expression results, counts matrices, sample data, and fgsea results from the results of a study on Huntington's disease (Labadorf et al., 2015). 

## Use
In RStudio, navigate to the directory that has this repo stored and open final_project.Rproj. Then, simply open app.R and press the run button in the top right corner of the code viewer window.

## Features
-Analyze sample metadata in both table and plot form <br>
-Analyze counts matrices with adjustable thresholds <br>
-Visualize DESeq2 results with adjustable parameters <br>
-Analyze fgsea results <br>

## Repository Structure

├── data/                 # Folder of processed datasets from the study to use in the app  
├── modules/              # Folder containing different Shiny apps used as modules in the main app    
├── README.md             # Project documentation (this file) <br>
├── app.R                 # Main app containing all modules (Run to load interactive app)  
└── final_project.Rproj   # RStudio project file to organize the project environment   

## References
Labadorf A, Hoss AG, Lagomarsino V, Latourelle JC et al. RNA Sequence Analysis of Human Huntington Disease Brain Reveals an Extensive Increase in Inflammatory and Developmental Gene Expression. *PLoS One* 2015;10(12):e0143563. PMID: 26636579<br>
Labadorf A, Choi SH, Myers RH. Evidence for a Pan-Neurodegenerative Disease Response in Huntington's and Parkinson's Disease Expression Profiles. *Front Mol Neurosci* 2017;10:430. PMID: 29375298<br>
Agus F, Crespo D, Myers RH, Labadorf A. The caudate nucleus undergoes dramatic and unique transcriptional changes in human prodromal Huntington's disease brain. *BMC Med Genomics* 2019 Oct 16;12(1):137. PMID: 31619230<br><br>


## Data
Data available via: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE64810

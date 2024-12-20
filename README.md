# BF591 Final Project

## Overview
This repo contains my final project for BF591. This Rshiny application was created to visualize and analyze differential expression results, counts matrices, sample data, and fgsea results from the results of a study on Huntington's disease (Labadorf et al., 2016)

## Features
-Analyze sample metadata in both table and plot form <br>
-Analyze counts matrices with adjustable thresholds <br>
-Visualize DESeq2 results with adjustable parameters <br>
-Analyze fgsea results <br>

## Repository Structure
``
├── data/                 # Folder of processed data sets from the study to use in the app <br>
├── modules/              # Folder containing different Shiny apps that are used as module in the main app<br>
├── notebooks/            # RMarkdown files for reports and exploration<br>
├── app.R/                # Main app containing all modules<br>
├── final_project.Rproj/  # RStudio project file to organize the project environment<br>
└── README.md             # Project documentation (this file)
``

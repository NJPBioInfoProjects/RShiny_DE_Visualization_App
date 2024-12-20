# BF591 Final Project

## Overview
This repo contains my final project for BF591. This Rshiny application was created to visualize and analyze differential expression results, counts matrices, sample data, and fgsea results from the results of a study on Huntington's disease (Labadorf et al., 2016)

## Features
-Analyze sample metadata in both table and plot form 
-Analyze counts matrices with adjustable thresholds
-Visualize DESeq2 results with adjustable parameters
-Analyze fgsea results

## Repository Structure
├── data/                 # Folder of processed data sets from the study to use in the app
├── modules/              # Folder containing different Shiny apps that are used as module in the main app
├── notebooks/            # RMarkdown files for reports and exploration
├── app.R/                # Main app containing all modules
├── final_project.Rproj/  # RStudio project file to organize the project environment
└── README.md             # Project documentation (this file)


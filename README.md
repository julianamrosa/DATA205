# DATA205
Bias Incidents

This is a project for the Montgomery College Data 205 (Capstone in Data Science) course. The chosen topic is "Bias Incidents" and the following datasets were used:
- Bias Incidents dataset from dataMontgomery (https://data.montgomerycountymd.gov/Public-Safety/MCPD-Bias-Incidents/7bhj-887p);
- Hate Crimes dataset from Kaggle (https://www.kaggle.com/louissebye/united-states-hate-crimes-19912017?select=hate_crime.csv).

This repository contains 9 files:

1- data_ingestion_wrangling_mc.R

  contains the R code that imports the Bias Incidents dataset and cleans it, generating an excel sheet with a clean version of the data (hc_mc.xlsx).
  
2- data_ingestion_wrangling_us.R

  contains the R code that imports the Hate Crimes dataset and cleans it, generating an excel sheet with a clean version of the data (hc_us.xlsx).
  
3- data_preparation_for_comparison.R

  contains the R code that imports both datasets and wrangles them, generating two excel sheets with versions of the datasets that were later used for comparison (hc_mc_comp.xlsx and hc_us_comp.xlsx).
  
4- EDA.Rmd

  contains the R code that generates exploratory charts, tables, and statistical measures. It also includes text with interpretations for the mentioned visualizations and statistics.
  
5- visualizations_and_statistical_analysis.Rmd

  contains the R code that performes some statistical analyses and produces more sofisticated visualizations (for answering a couple of research questions).
  
6- visualizations_and_statistical_analysis_2.ipynb

  contains the Python code that performes some statistical analyses and produces more sofisticated visualizations (for answering more research questions).
  
7- population_states.xlsx

  it is a small dataset that contains the population of each US state. It was used inside the EDA.Rmd and visualizations_and_statistical_analysis.Rmd files.
  
8- final_report.pdf

  it consists of the written report that summarizes the project's results.
  
9- final_presentation.pptx

  it consists of the slide presentation that summarizes the project's results.

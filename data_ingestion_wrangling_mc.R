#Loading packages
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(httr)
library(jsonlite)
library(tidyverse)
library(writexl)

#Importing dataset
hc_mc <- GET('https://data.montgomerycountymd.gov/resource/7bhj-887p.json')
jsonRespText <- content(hc_mc, as="text")
hc_mc <- fromJSON(jsonRespText)

#Reordering columns and removing bias_code_2 variable
hc_mc <- hc_mc%>%
  select(id, incident_date, bias_code, bias, status, victim_type, no_of_victims,
         no_of_suspects, suspects_less_than_18_years, suspects_18_35_years_old,
         suspects_36_45_years_old, suspects_46_55_years_old, suspects_55_years_old)

#Renaming some columns
hc_mc <- hc_mc%>%
  rename(offense=bias, case_status = status, victim_count=no_of_victims,
         suspect_count=no_of_suspects)

#Assigning appropriate data types
hc_mc$incident_date <- ymd_hms(hc_mc$incident_date, tz='EST')
hc_mc$victim_count <- as.integer(hc_mc$victim_count)
hc_mc$suspect_count <- as.integer(hc_mc$suspect_count)
hc_mc$suspects_less_than_18_years <- as.integer(hc_mc$suspects_less_than_18_years)
hc_mc$suspects_18_35_years_old <- as.integer(hc_mc$suspects_18_35_years_old)
hc_mc$suspects_36_45_years_old <- as.integer(hc_mc$suspects_36_45_years_old)
hc_mc$suspects_46_55_years_old <- as.integer(hc_mc$suspects_46_55_years_old)
hc_mc$suspects_55_years_old <- as.integer(hc_mc$suspects_55_years_old)

#Changing offense labels
offense_vector_2 <- hc_mc$offense
for (i in 1:564){
  if (offense_vector_2[i] %in% c("Physical Intimidation/Simple Assault", "Verbal Intimidation/Simple Assault", "Written Intimidation/Simple Assault")){
    offense_vector_2[i] = "Intimidation"
  }
  else if (offense_vector_2[i] %in% c("Flyer Left Behind", "Display of Noose")){
    offense_vector_2[i]="Other"
  }
  else if (offense_vector_2[i] %in% c("Assault (simple)", "Assault (physical)")){
    offense_vector_2[i]="Assault"
  }
}
hc_mc$offense <- offense_vector_2

#Changing case_status labels
status_vector <- hc_mc$case_status
for (i in 1:564){
  if (is.na(status_vector[i])){
    next
  }
  else if (status_vector[i] %in% c("UNF", "RTOJ", "N/A")){
    status_vector[i]=NA
  }
  else if (status_vector[i] %in% c("Closed-Exception", "Closed-Arrest", "Closed-Admin")){
    status_vector[i]="Closed"
  }
}
hc_mc$case_status <- status_vector

#Saving clean data as xlsx
write_xlsx(hc_mc, "hc_mc.xlsx")
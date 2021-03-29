#Loading packages
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(httr)
library(jsonlite)
library(tidyverse)

#Importing datasets
##US
devtools::install_github("ldurazo/kaggler")
library(readr)
library(kaggler)
kgl_auth(creds_file = 'kaggle.json')
response <- kgl_datasets_download_all(owner_dataset = 
                                        "louissebye/united-states-hate-crimes-19912017")
download.file(response[["url"]], "hate_crime.csv", mode="wb")
unzip_result <- unzip("hate_crime.csv", exdir = "data", overwrite = 
                        TRUE)
hc_us <- read_csv("data/hate_crime.csv")
##MC
hc_mc <- GET('https://data.montgomerycountymd.gov/resource/7bhj-887p.json')
jsonRespText <- content(hc_mc, as="text")
hc_mc <- fromJSON(jsonRespText)

#Converting to date time
hc_mc$incident_date <- ymd_hms(hc_mc$incident_date, tz='EST')

#Selecting columns of interest and reordering them
##MC
hc_mc_comp <- hc_mc%>%
  select(id, incident_date, bias_code, bias, no_of_victims, victim_type, no_of_suspects,
         suspects_less_than_18_years, suspects_18_35_years_old, suspects_36_45_years_old,
         suspects_46_55_years_old, suspects_55_years_old)
##US
hc_us_comp <- hc_us%>%
  select(INCIDENT_ID, DATA_YEAR, BIAS_DESC, OFFENSE_NAME, VICTIM_COUNT, VICTIM_TYPES,
         TOTAL_OFFENDER_COUNT, JUVENILE_OFFENDER_COUNT, ADULT_OFFENDER_COUNT)

#Renaming variables
##MC
hc_mc_comp <- hc_mc_comp%>%
  rename(offense = bias, victim_count = no_of_victims, suspect_count = no_of_suspects)
##US
hc_us_comp <- hc_us_comp%>%
  rename(id = INCIDENT_ID, year = DATA_YEAR, bias_code = BIAS_DESC, offense = OFFENSE_NAME,
         victim_count = VICTIM_COUNT, victim_type = VICTIM_TYPES,
         suspect_count = TOTAL_OFFENDER_COUNT,
         juvenile_suspects = JUVENILE_OFFENDER_COUNT,
         adult_suspects = ADULT_OFFENDER_COUNT)

#Changing the date variable to a year variable in the mc dataset
hc_mc_comp <- hc_mc_comp%>%
  mutate(year = year(incident_date))
hc_mc_comp <- hc_mc_comp%>%
  select(id, year, bias_code, offense, victim_count, victim_type, suspect_count,
         suspects_less_than_18_years, suspects_18_35_years_old, suspects_36_45_years_old,
         suspects_46_55_years_old, suspects_55_years_old)

#Matching bias codes
bias_codes_vector <- hc_us_comp$bias_code
other <- c("Anti-American Indian or Alaska Native;Anti-Asian",
           "Anti-American Indian or Alaska Native;Anti-Black or African American",
           "Anti-American Indian or Alaska Native;Anti-Hispanic or Latino",
           "Anti-American Indian or Alaska Native;Anti-Islamic (Muslim)",
           "Anti-American Indian or Alaska Native;Anti-White",
           "Anti-Arab;Anti-Asian;Anti-Black or African American",
           "Anti-Arab;Anti-Black or African American",
           "Anti-Arab;Anti-Black or African American;Anti-Islamic (Muslim)",
           "Anti-Arab;Anti-Hispanic or Latino",
           "Anti-Arab;Anti-Hispanic or Latino;Anti-Islamic (Muslim)",
           "Anti-Arab;Anti-Islamic (Muslim)",
           "Anti-Arab;Anti-Multiple Races, Group",
           "Anti-Asian;Anti-Atheism/Agnosticism",
           "Anti-Asian;Anti-Black or African American",
           "Anti-Asian;Anti-Female", "Anti-Asian;Anti-Gay (Male)",
           "Anti-Asian;Anti-Hispanic or Latino", "Anti-Asian;Anti-Islamic (Muslim)",
           "Anti-Asian;Anti-Multiple Races, Group",
           "Anti-Asian;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Asian;Anti-White", "Anti-Bisexual;Anti-Black or African American",
           "Anti-Bisexual;Anti-Gay (Male)", "Anti-Bisexual;Anti-Heterosexual",
           "Anti-Black or African American;Anti-Gay (Male)",
           "Anti-Black or African American;Anti-Gay (Male);Anti-Jewish",
           "Anti-Black or African American;Anti-Gay (Male);Anti-Lesbian (Female)",
           "Anti-Black or African American;Anti-Gay (Male);Anti-White",
           "Anti-Black or African American;Anti-Hispanic or Latino",
           "Anti-Black or African American;Anti-Hispanic or Latino;Anti-Multiple Races, Group;Anti-White", "Anti-Black or African American;Anti-Islamic (Muslim)",
           "Anti-Black or African American;Anti-Jewish",
           "Anti-Black or African American;Anti-Jewish;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Black or African American;Anti-Jewish;Anti-Multiple Races, Group",
           "Anti-Black or African American;Anti-Jewish;Anti-Multiple Races, Group;Anti-Multiple Religions, Group",
           "Anti-Black or African American;Anti-Jewish;Anti-White",
           "Anti-Black or African American;Anti-Lesbian (Female)",
           "Anti-Black or African American;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Black or African American;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group);Anti-Transgender",
           "Anti-Black or African American;Anti-Mental Disability", 
           "Anti-Black or African American;Anti-Multiple Races, Group",
           "Anti-Black or African American;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Black or African American;Anti-Other Religion",
           "Anti-Black or African American;Anti-Protestant",
           "Anti-Black or African American;Anti-Transgender",
           "Anti-Black or African American;Anti-White", "Anti-Buddhist",
           "Anti-Catholic;Anti-Jewish", "Anti-Catholic;Anti-Protestant",
           "Anti-Female;Anti-Gay (Male)", "Anti-Female;Anti-Mental Disability",
           "Anti-Female;Anti-Mental Disability;Anti-White", "Anti-Female;Anti-White",
           "Anti-Gay (Male);Anti-Heterosexual", "Anti-Gay (Male);Anti-Islamic (Muslim)",
           "Anti-Gay (Male);Anti-Jewish", "Anti-Gay (Male);Anti-Lesbian (Female)",
           "Anti-Gay (Male);Anti-Lesbian (Female);Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Gay (Male);Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)", "Anti-Gay (Male);Anti-Male",
           "Anti-Gay (Male);Anti-Mental Disability", "Anti-Gay (Male);Anti-Multiple Races, Group",
           "Anti-Gay (Male);Anti-Other Race/Ethnicity/Ancestry", "Anti-Gay (Male);Anti-Physical Disability",
           "Anti-Gay (Male);Anti-Transgender", "Anti-Gay (Male);Anti-White", "Anti-Heterosexual;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Hindu", "Anti-Hispanic or Latino;Anti-Jewish", "Anti-Hispanic or Latino;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Hispanic or Latino;Anti-Male", "Anti-Hispanic or Latino;Anti-Multiple Races, Group",
           "Anti-Hispanic or Latino;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Hispanic or Latino;Anti-White", "Anti-Islamic (Muslim);Anti-Jewish",
           "Anti-Islamic (Muslim);Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Islamic (Muslim);Anti-Multiple Races, Group",
           "Anti-Islamic (Muslim);Anti-Other Religion", "Anti-Jehovah's Witness",
           "Anti-Jewish;Anti-Lesbian (Female)",
           "Anti-Jewish;Anti-Lesbian (Female);Anti-White",
           "Anti-Jewish;Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Jewish;Anti-Multiple Races, Group",
           "Anti-Jewish;Anti-Multiple Races, Group;Anti-Multiple Religions, Group",
           "Anti-Jewish;Anti-Multiple Religions, Group",
           "Anti-Jewish;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Jewish;Anti-Transgender", "Anti-Jewish;Anti-White",
           "Anti-Lesbian (Female);Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)",
           "Anti-Lesbian (Female);Anti-Other Religion", "Anti-Lesbian (Female);Anti-White",
           "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group);Anti-Multiple Races, Group",
           "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group);Anti-Physical Disability",
           "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group);Anti-White", "Anti-Male",
           "Anti-Male;Anti-Multiple Races, Group;Anti-White",
           "Anti-Male;Anti-Native Hawaiian or Other Pacific Islander", "Anti-Male;Anti-White",
           "Anti-Mental Disability;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Mental Disability;Anti-Physical Disability", "Anti-Mormon", "Anti-Multiple Races, Group;Anti-Multiple Religions, Group",
           "Anti-Multiple Races, Group;Anti-Other Race/Ethnicity/Ancestry",
           "Anti-Multiple Races, Group;Anti-Other Religion",
           "Anti-Multiple Races, Group;Anti-White", "Anti-Multiple Religions, Group;Anti-Other Religion",
           "Anti-Multiple Religions, Group;Anti-Protestant",
           "Anti-Multiple Religions, Group;Anti-Transgender",
           "Anti-Native Hawaiian or Other Pacific Islander",
           "Anti-Other Race/Ethnicity/Ancestry;Anti-Other Religion",
           "Anti-Other Race/Ethnicity/Ancestry;Anti-White",
           "Anti-Other Religion;Anti-Protestant", "Anti-Other Religion;Anti-White",
           "Anti-Physical Disability;Anti-White", "Anti-Transgender;Anti-White",
           "Unknown (offender's motivation not known)", "Anti-Atheism/Agnosticism",
           "Anti-Eastern Orthodox (Russian, Greek, Other)", "Anti-Female", "Anti-Sikh",
           "Anti-Bisexual", "Anti-Heterosexual", "Anti-Mental Disability",
           "Anti-Physical Disability", "Anti-Protestant")
for (i in 1:201403){
  if (is.na(bias_codes_vector[i])){
    next
  }
  else if (bias_codes_vector[i]=="Anti-Other Race/Ethnicity/Ancestry"){
    bias_codes_vector[i]="Anti-Other Ethnicity"
  }
  else if (bias_codes_vector[i]=="Anti-Multiple Religions" | bias_codes_vector[i]=="Anti-Multiple Religions, Group"){
    bias_codes_vector[i]="Anti-Multi-Religious Group"
  }
  else if (bias_codes_vector[i]=="Anti-Multiple Races" | bias_codes_vector[i]=="Anti-Multiple Races, Group"){
    bias_codes_vector[i]="Anti-Multi-Racial"
  }
  else if (bias_codes_vector[i]=="Anti-Lesbian (Female)"){
    bias_codes_vector[i]="Anti-Homosexual"
  }
  else if (bias_codes_vector[i]=="Anti-Islamic (Muslim)"){
    bias_codes_vector[i]="Anti-Islamic"
  }
  else if (bias_codes_vector[i]=="Anti-Hispanic or Latino"){
    bias_codes_vector[i]="Anti-Hispanic"
  }
  else if (bias_codes_vector[i]=="Anti-Gay (Male)"){
    bias_codes_vector[i]="Anti-Homosexual"
  }
  else if (bias_codes_vector[i]=="Anti-Black or African American"){
    bias_codes_vector[i]="Anti-Black"
  }
  else if (bias_codes_vector[i] %in% other){
    bias_codes_vector[i]="Other"
  }
}
hc_us_comp$bias_code <- bias_codes_vector

#Matching offense
##US
offense_vector <- hc_us_comp$offense
`%notin%` <- Negate(`%in%`)
for (i in 1:201403){
  if (offense_vector[i] %notin% c("Simple Assault", "Intimidation",
                                 "Destruction/Damage/Vandalism of Property",
                                 "Arson", "Aggravated Assault")){
    offense_vector[i] = "Other"
  }
  else if (offense_vector[i] == "Destruction/Damage/Vandalism of Property"){
    offense_vector[i] = "Vandalism"
  }
  else if (offense_vector[i] == "Simple Assault" | offense_vector[i] == "Aggravated Assault"){
    offense_vector[i] = "Assault"
  }
}
hc_us_comp$offense <- offense_vector
##MC
offense_vector_1 <- hc_mc_comp$offense
for (i in 1:564){
  if (offense_vector_1[i] %in% c("Physical Intimidation/Simple Assault", "Verbal Intimidation/Simple Assault", "Written Intimidation/Simple Assault")){
    offense_vector_1[i] = "Intimidation"
  }
  else if (offense_vector_1[i] %in% c("Flyer Left Behind", "Display of Noose")){
    offense_vector_1[i]="Other"
  }
  else if (offense_vector_1[i] %in% c("Assault (simple)", "Assault (physical)")){
    offense_vector_1[i]="Assault"
  }
}
hc_mc_comp$offense <- offense_vector_1

#Matching victim type
##US
victim_type_vector <- hc_us_comp$victim_type
for (i in 1:201403){
  if(is.na(victim_type_vector[i])){
    next
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Unknown"))){
    victim_type_vector[i]=NA
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Government"))){
    victim_type_vector[i]="Government"
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Business")) | str_detect(victim_type_vector[i], pattern=fixed("Financial Institution"))){
    victim_type_vector[i]="Business/Financial Institution"
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Religious Organization"))){
    victim_type_vector[i]="Religious Organization"
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Society"))){
    victim_type_vector[i]="Society"
  }
  else if (str_detect(victim_type_vector[i], pattern=fixed("Individual"))){
    victim_type_vector[i]="Individual(s)"
  }
  else{
    victim_type_vector[i]="Other"
  }
}
hc_us_comp$victim_type <- victim_type_vector

#Changing age variable for MC dataset
##Converting age variables into integers for both datasets
hc_us_comp$juvenile_suspects <- as.integer(hc_us_comp$juvenile_suspects)
hc_us_comp$adult_suspects <- as.integer(hc_us_comp$adult_suspects)
hc_mc_comp$suspects_less_than_18_years <- as.integer(hc_mc_comp$suspects_less_than_18_years)
hc_mc_comp$suspects_18_35_years_old <- as.integer(hc_mc_comp$suspects_18_35_years_old)
hc_mc_comp$suspects_36_45_years_old <- as.integer(hc_mc_comp$suspects_36_45_years_old)
hc_mc_comp$suspects_46_55_years_old <- as.integer(hc_mc_comp$suspects_46_55_years_old)
hc_mc_comp$suspects_55_years_old <- as.integer(hc_mc_comp$suspects_55_years_old)
##Converting NA's into 0's for the MC dataset
suspects_18_35_vector <- hc_mc_comp$suspects_18_35_years_old
for (i in 1:564){
  if (is.na(suspects_18_35_vector[i])){
    suspects_18_35_vector[i]=0
  }
}
hc_mc_comp$suspects_18_35_years_old <- suspects_18_35_vector
suspects_36_45_vector <- hc_mc_comp$suspects_36_45_years_old
for (i in 1:564){
  if (is.na(suspects_36_45_vector[i])){
    suspects_36_45_vector[i]=0
  }
}
hc_mc_comp$suspects_36_45_years_old <- suspects_36_45_vector
suspects_46_55_vector <- hc_mc_comp$suspects_46_55_years_old
for (i in 1:564){
  if (is.na(suspects_46_55_vector[i])){
    suspects_46_55_vector[i]=0
  }
}
hc_mc_comp$suspects_46_55_years_old <- suspects_46_55_vector
suspects_55_vector <- hc_mc_comp$suspects_55_years_old
for (i in 1:564){
  if (is.na(suspects_55_vector[i])){
    suspects_55_vector[i]=0
  }
}
hc_mc_comp$suspects_55_years_old <- suspects_55_vector
##Summing the adult suspects count to create one column for them (MC dataset)
hc_mc_comp <- hc_mc_comp%>%
  mutate(adult_suspects = suspects_18_35_years_old+suspects_36_45_years_old+suspects_46_55_years_old+suspects_55_years_old)
hc_mc_comp <- hc_mc_comp%>%
  select(id, year, bias_code, offense, victim_count, victim_type, suspect_count,
         suspects_less_than_18_years, adult_suspects)
hc_mc_comp <- hc_mc_comp%>%
  rename(juvenile_suspects = suspects_less_than_18_years)
##Converting 0's back to NA's
adult_suspects_vector <- hc_mc_comp$adult_suspects
for (i in 1:564){
  if (adult_suspects_vector[i]==0){
    adult_suspects_vector[i]=NA
  }
}
hc_mc_comp$adult_suspects <- adult_suspects_vector

#Filtering for year
hc_mc_comp <- hc_mc_comp%>%
  filter(year %in% c(2016, 2017, 2018))
hc_us_comp <- hc_us_comp%>%
  filter(year %in% c(2016, 2017, 2018))

#Assigning appropriate data types
##MC
hc_mc_comp$victim_count <- as.integer(hc_mc_comp$victim_count)
hc_mc_comp$suspect_count <- as.integer(hc_mc_comp$suspect_count)
hc_mc_comp$adult_suspects <- as.integer(hc_mc_comp$adult_suspects)
##MD
hc_us_comp$victim_count <- as.integer(hc_us_comp$victim_count)
hc_us_comp$suspect_count <- as.integer(hc_us_comp$suspect_count)
hc_us_comp$id <- as.character(hc_us_comp$id)
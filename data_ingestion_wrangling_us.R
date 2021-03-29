#Importing dataset
library(devtools)
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

#Loading packages
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)

##Reordering columns and removing unwanted ones
hc_us <- hc_us%>%
  select(INCIDENT_ID, DATA_YEAR, STATE_NAME, BIAS_DESC, OFFENSE_NAME,
         VICTIM_TYPES, VICTIM_COUNT,
         TOTAL_OFFENDER_COUNT, ADULT_OFFENDER_COUNT, JUVENILE_OFFENDER_COUNT,
         OFFENDER_RACE)

#Renaming columns
hc_us <- hc_us%>%
  rename(id=INCIDENT_ID, year=DATA_YEAR, state=STATE_NAME, bias_code=BIAS_DESC,
         offense=OFFENSE_NAME, victim_type=VICTIM_TYPES, victim_count=VICTIM_COUNT,
         offender_count=TOTAL_OFFENDER_COUNT, adult_offenders=ADULT_OFFENDER_COUNT,
         juvenile_offenders=JUVENILE_OFFENDER_COUNT, offender_race=OFFENDER_RACE)

#Assigning appropriate data types
hc_us$id <- as.character(hc_us$id)
hc_us$victim_count <- as.integer(hc_us$victim_count)
hc_us$offender_count <- as.integer(hc_us$offender_count)
hc_us$adult_offenders <- as.integer(hc_us$adult_offenders)
hc_us$juvenile_offenders <- as.integer(hc_us$juvenile_offenders)

#Changing bias codes
bias_codes_vector_1 <- hc_us$bias_code
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
  if (is.na(bias_codes_vector_1[i])){
    next
  }
  else if (bias_codes_vector_1[i]=="Anti-Other Race/Ethnicity/Ancestry"){
    bias_codes_vector_1[i]="Anti-Other Ethnicity"
  }
  else if (bias_codes_vector_1[i]=="Anti-Multiple Religions" | bias_codes_vector_1[i]=="Anti-Multiple Religions, Group"){
    bias_codes_vector_1[i]="Anti-Multi-Religious Group"
  }
  else if (bias_codes_vector_1[i]=="Anti-Multiple Races" | bias_codes_vector_1[i]=="Anti-Multiple Races, Group"){
    bias_codes_vector_1[i]="Anti-Multi-Racial"
  }
  else if (bias_codes_vector_1[i]=="Anti-Lesbian (Female)"){
    bias_codes_vector_1[i]="Anti-Homosexual"
  }
  else if (bias_codes_vector_1[i]=="Anti-Islamic (Muslim)"){
    bias_codes_vector_1[i]="Anti-Islamic"
  }
  else if (bias_codes_vector_1[i]=="Anti-Hispanic or Latino"){
    bias_codes_vector_1[i]="Anti-Hispanic"
  }
  else if (bias_codes_vector_1[i]=="Anti-Gay (Male)"){
    bias_codes_vector_1[i]="Anti-Homosexual"
  }
  else if (bias_codes_vector_1[i]=="Anti-Black or African American"){
    bias_codes_vector_1[i]="Anti-Black"
  }
  else if (bias_codes_vector_1[i] %in% other){
    bias_codes_vector_1[i]="Other"
  }
}
hc_us$bias_code <- bias_codes_vector_1

#Changing offense names
offense_vector_1 <- hc_us$offense
`%notin%` <- Negate(`%in%`)
for (i in 1:201403){
  if (offense_vector_1[i] %notin% c("Simple Assault", "Intimidation",
                                  "Destruction/Damage/Vandalism of Property",
                                  "Arson", "Aggravated Assault")){
    offense_vector_1[i] = "Other"
  }
  else if (offense_vector_1[i] == "Destruction/Damage/Vandalism of Property"){
    offense_vector_1[i] = "Vandalism"
  }
  else if (offense_vector_1[i] == "Simple Assault" | offense_vector_1[i] == "Aggravated Assault"){
    offense_vector_1[i] = "Assault"
  }
}
hc_us$offense <- offense_vector_1

#Changing victim type labels
victim_type_vector_1 <- hc_us$victim_type
for (i in 1:201403){
  if(is.na(victim_type_vector_1[i])){
    next
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Unknown"))){
    victim_type_vector_1[i]=NA
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Government"))){
    victim_type_vector_1[i]="Government"
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Business")) | str_detect(victim_type_vector_1[i], pattern=fixed("Financial Institution"))){
    victim_type_vector_1[i]="Business/Financial Institution"
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Religious Organization"))){
    victim_type_vector_1[i]="Religious Organization"
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Society"))){
    victim_type_vector_1[i]="Society"
  }
  else if (str_detect(victim_type_vector_1[i], pattern=fixed("Individual"))){
    victim_type_vector_1[i]="Individual(s)"
  }
  else{
    victim_type_vector_1[i]="Other"
  }
}
hc_us$victim_type <- victim_type_vector_1

#Deleting incidents from non-states (except DC)
hc_us <- hc_us%>%
  filter(state!="Federal", state!="Guam")

#Adjusting the missing value notation for offender_race
offender_race_vector <- hc_us$offender_race
for (i in 1:201376){
  if (is.na(offender_race_vector[i])){
    next
  }
  else if (offender_race_vector[i]=='Unknown'){
    offender_race_vector[i]=NA
  }
}
hc_us$offender_race <- offender_race_vector

#Saving clean data as xlsx
write_xlsx(hc_us, "hc_us.xlsx")
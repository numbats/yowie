## code to prepare `wages_hs2020` dataset goes here

library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(zoo)
library(rstatix)

# READ THE DATA

## The downloaded data (wages-high-school-demo.dat) from NLSY website
## is provided with an R file (wages-high-school-demo.R)
## to read the data into R and transform the variable name and
## coding the variables' value into something more sensible.
## It is also come along with the wages-high-school-demo.NLSY79
## Which is a tagset, that can be uploaded to the web site to recreate the data set.

source(here::here("data-raw/wages-high-school-demo.R"))

## DEMOGRAPHIC DATA PREPROCESSING

## tidy the date of birth data
dob_tidy <- new_data_qnames %>%
  select(CASEID_1979,
         starts_with("Q1-3_A~")) %>%
  mutate(dob_year = case_when(
    # if the years recorded in both sets match, take 79 data
    `Q1-3_A~Y_1979` == `Q1-3_A~Y_1981` ~ `Q1-3_A~Y_1979`,
    # if the year in the 81 set is missing, take the 79 data
    is.na(`Q1-3_A~Y_1981`) ~ `Q1-3_A~Y_1979`,
    # if the sets don't match for dob year, take the 79 data
    `Q1-3_A~Y_1979` != `Q1-3_A~Y_1981` ~ `Q1-3_A~Y_1979`),
    dob_month = case_when(
      # if months recorded in both sets match, take 79 data
      `Q1-3_A~M_1979` == `Q1-3_A~M_1981` ~ `Q1-3_A~M_1979`,
      # if month in 81 set is missing, take the 79 data
      is.na(`Q1-3_A~M_1981`) ~ `Q1-3_A~M_1979`,
      # if sets don't match for dob month, take the 79 data
      `Q1-3_A~M_1979` != `Q1-3_A~M_1981` ~ `Q1-3_A~M_1979`),
    # flag if there is a conflict between dob recorded in 79 and 81
    dob_conflict = case_when(
      (`Q1-3_A~M_1979` != `Q1-3_A~M_1981`) & !is.na(`Q1-3_A~M_1981`) ~ TRUE,
      (`Q1-3_A~Y_1979` != `Q1-3_A~Y_1981`) & !is.na(`Q1-3_A~Y_1981`) ~ TRUE,
      (`Q1-3_A~Y_1979` == `Q1-3_A~Y_1981`) & (`Q1-3_A~M_1979` == `Q1-3_A~M_1981`) ~ FALSE,
      is.na(`Q1-3_A~M_1981`) | is.na(`Q1-3_A~Y_1981`) ~ FALSE))
dob_tidy <- dob_tidy %>%
  select(CASEID_1979,
         dob_month,
         dob_year,
         dob_conflict)

## tidy the sex and race data
demog_tidy <- categories_qnames %>%
  select(CASEID_1979,
         SAMPLE_RACE_78SCRN,
         SAMPLE_SEX_1979) %>%
  rename(gender = SAMPLE_SEX_1979,
         race = SAMPLE_RACE_78SCRN)

## tidy the grade completed in each year
demog_education <- new_data_qnames %>%
  as_tibble() %>%
  rename(HGC_2018 = `Q3-4_2018`) %>%
  select(CASEID_1979,
         starts_with("HGCREV"),
         "HGC_2012",
         "HGC_2014",
         "HGC_2016",
         "HGC_2018") %>%
  pivot_longer(!CASEID_1979,
               names_to = "var",
               values_to = "grade") %>%
  separate("var", c("var", "year"), sep = -4) %>%
  filter(!is.na(grade)) %>%
  select(-var)

## getting the highest education completed
highest_year <- demog_education %>%
  group_by(CASEID_1979) %>%
  mutate(hgc_i = max(grade)) %>%
  filter(hgc_i == grade) %>%
  filter(year == first(year)) %>%
  rename(yr_hgc = year) %>%
  select(CASEID_1979, yr_hgc, hgc_i) %>%
  ungroup() %>%
  mutate('hgc' = ifelse(hgc_i == 0, "NONE",
                        ifelse(hgc_i == 1, "1ST GRADE",
                               ifelse(hgc_i == 2, "2ND GRADE",
                                      ifelse(hgc_i == 3, "3RD GRADE",
                                             ifelse(hgc_i >= 4 & hgc_i <= 12, paste0(hgc_i,"TH GRADE"),
                                                    ifelse(hgc_i == 13, "1ST YEAR COL",
                                                           ifelse(hgc_i == 14, "2ND YEAR COL",
                                                                  ifelse(hgc_i == 15, "3RD YEAR COL",
                                                                         ifelse(hgc_i == 95, "UNGRADED", paste0((hgc_i - 12), "TH YEAR COL")))))))))))

## Join all of the demographic information
full_demographics <- full_join(dob_tidy, demog_tidy, by = "CASEID_1979") %>%
  full_join(highest_year, by = "CASEID_1979") %>%
  rename("id" = "CASEID_1979")


## EMPLOYMENT DATA (WAGES AND HOURS OF WORK) PREPROCESSING

## Tidying hours of work

# the list of the years using the information from QES-52A
year_A <- c(1979:1987, 1993)
# a function to get the hour of works
get_hour <- function(year){
  if(year %in% year_A){
    temp <- new_data_qnames %>%
      select(CASEID_1979,
             starts_with("QES-52A") &
               ends_with(as.character(year)))}
  else{
    temp <- new_data_qnames %>%
      select(CASEID_1979,
             starts_with("QES-52D") &
               ends_with(as.character(year)))}
  temp %>%
    pivot_longer(!CASEID_1979,
                 names_to = "job",
                 values_to = "hours_work") %>%
    separate("job", c("job", "year"), sep = -4) %>%
    mutate(job = paste0("job_", substr(job, 9, 10))) %>%
    rename(id = CASEID_1979)
}
# getting the hours of work for all years
hours <- list()
for(ayear in c(1979:1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010,
               2012, 2014, 2016, 2018)) {
  hours[[ayear]] <- get_hour(ayear)
}
hours_all <- bind_rows(!!!hours)

## Tidying rates per hour data
# function to get the rates per hour
get_rate <- function(year) {
  new_data_qnames %>%
    select(CASEID_1979,
           starts_with("HRP") &
             ends_with(as.character(year))) %>%
    pivot_longer(!CASEID_1979, names_to = "job", values_to = "rate_per_hour") %>%
    separate("job", c("job", "year"), sep = -4) %>%
    mutate(job = paste0("job_0", substr(job, 4, 4))) %>%
    rename(id = CASEID_1979)
}
# getting the rates per hour for all years
rates <- list()
for(ayear in c(1979:1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010,
               2012, 2014, 2016, 2018)) {
  rates[[ayear]] <- get_rate(ayear)
}
rates_all <- bind_rows(!!!rates)

# join the hours of work and the rates per hour data
hours_wages <- left_join(rates_all,
                         hours_all,
                         by = c("id", "year", "job")) %>%
  # set the 0 value in rate_per_hour as NA
  mutate(rate_per_hour = ifelse(rate_per_hour == 0, NA,
                                rate_per_hour))

## Calculate the mean hourly wage data

# calculate number of jobs that a person has in one year
no_job <- hours_wages %>%
  filter(!is.na(rate_per_hour)) %>%
  group_by(id, year) %>%
  summarise(no_jobs = length(rate_per_hour))

# filter the observations with available rate per hour
eligible_wages <- hours_wages %>%
  filter(!is.na(rate_per_hour)) %>%
  left_join(no_job, by = c("id", "year"))

# calculate the mean_hourly_wage
mean_hourly_wage <-
  eligible_wages %>%
  group_by(id, year) %>%
  # calculate the weighted mean if the number of jobs > 1
  mutate(wages = ifelse(no_jobs == 1, rate_per_hour/100,
                        weighted.mean(rate_per_hour, hours_work, na.rm = TRUE)/100)) %>%
  # give the flag if it is the weighted mean
  # 1 for weighted mean; 0 for regular/arithmetic mean
  mutate(flag1 = ifelse(!is.na(wages) & no_jobs != 1, 1,
                        0)) %>%
  # calculate the arithmetic mean for the na
  mutate(wages = ifelse(is.na(wages), mean(rate_per_hour)/100,
                        wages)) %>%
  # create the mean hourly wage variable for one id per year i.e. summarise the job
  group_by(id, year) %>%
  summarise(wages = mean(wages),
            total_hours = sum(hours_work),
            number_of_jobs = mean(no_jobs),
            flag1 = mean(flag1)) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  rename(mean_hourly_wage = wages) %>%
  mutate(is_wm = ifelse(flag1 == 1, TRUE,
                        FALSE)) %>%
  select(-flag1)

## CREATING THE WAGES_HS2020 DATASET

# join the wages information and the demographic information by case id.
wages_demog <- left_join(mean_hourly_wage, full_demographics, by="id")

# calculate the years in work force and the age of the subjects in 1979
wages_demog <- wages_demog %>%
  mutate(yr_hgc = as.numeric(yr_hgc)) %>%
  mutate(years_in_workforce = year - yr_hgc) %>%
  mutate(age_1979 = 1979 - (dob_year + 1900))

# filter only the id with high school education
wages_demog_hs <- wages_demog  %>% filter(grepl("GRADE", hgc))

# identify the extreme mean hourly wage in each id
# if the mean hourly wage is extreme, we will replace it with NA
# impute the NA with the last observation carried forward (locf) for each id
# we give the flag for that locf observation

# identify the extreme mean hourly wage in each id
wages_demog_ext <- wages_demog_hs %>%
  group_by(id) %>%
  identify_outliers("mean_hourly_wage") %>%
  filter(is.extreme == TRUE) %>%
  select(year, id, is.extreme)
# join the extreme column to wages_demog_hs
wages_demog_hs <- left_join(wages_demog_hs, wages_demog_ext, by = c("id", "year"))
# give the FALSE flag to non-extreme value
wages_demog_hs <- wages_demog_hs %>%
  mutate(is.extreme = ifelse(is.na(is.extreme), FALSE,
                             is.extreme))
# make the extreme values as NA and give the flag to that observation
# since we're gonna do locf
wages_demog_hs <- wages_demog_hs %>%
  mutate(mean_hourly_wage = ifelse(is.extreme == TRUE, NA, mean_hourly_wage),
         is_locf = ifelse(is.na(mean_hourly_wage), TRUE, FALSE))
# doing locf
for_locf <- wages_demog_hs %>%
  select(id, year, mean_hourly_wage) %>%
  group_by(id) %>%
  na.locf()
# join back the locf value to wages_demog_hs
wages_demog_hs <- left_join(wages_demog_hs, for_locf, by = c("id", "year")) %>%
  select(-mean_hourly_wage.x) %>%
  rename(mean_hourly_wage = mean_hourly_wage.y)

# calculate the number of observation
keep_me <- wages_demog_hs %>% count(id)
# filter out the id with low number of observation
keep_me <- keep_me %>% filter(n > 4)
wages_demog_hs <- wages_demog_hs %>%
  filter(id %in% keep_me$id)

# Add the extreme value flag

# Add the extreme value flag for the ID that has extreme mean
wages_demog_mean <- wages_demog_hs %>%
  group_by(id) %>%
  summarise(mean_yearly_wages = mean(mean_hourly_wage)) %>%
  identify_outliers("mean_yearly_wages") %>%
  filter(is.extreme == TRUE) %>%
  rename(is_ext_id = is.extreme) %>%
  select(id, is_ext_id)
# join back to wages_demog_hs
wages_demog_hs <- left_join(wages_demog_hs, wages_demog_mean, by = "id")
# give the flag to non-extreme id
wages_demog_hs <- wages_demog_hs %>%
  mutate(is_ext_id = ifelse(is.na(is_ext_id), FALSE, is_ext_id))

# Add the extreme value flag for the observation that is extreme
extreme_obs <- wages_demog_hs %>%
  select(-is.extreme) %>%
  group_by(id) %>%
  identify_outliers("mean_hourly_wage") %>%
  filter(is.extreme == TRUE) %>%
  rename(is_ext_obs = is.extreme) %>%
  select(id, year, is_ext_obs)

wages_demog_hs <- left_join(wages_demog_hs, extreme_obs, by = c("id", "year"))

wages_demog_hs <- wages_demog_hs %>%
  mutate(is_ext_obs = ifelse(is.na(is_ext_obs), FALSE, is_ext_obs))

# rename and select the wages in tidy
wages_hs2020 <- wages_demog_hs %>%
  select(id, year, mean_hourly_wage, age_1979, gender, race, hgc, hgc_i, yr_hgc, number_of_jobs, total_hours, is_wm, is_locf, is_ext_id, is_ext_obs)

# save it to an rda object
usethis::use_data(wages_hs2020, overwrite = TRUE)



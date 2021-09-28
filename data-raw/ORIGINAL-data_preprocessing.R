

## ---- setup
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(MASS)
library(broom)
library(purrr)
library(tsibble)


#' The downloaded data (wages-high-school-demo.dat) from NLSY website
#' is provided with an R file (wages-high-school-demo.R)
#' to read the data into R and transform the variable name and
#' coding the variables' value into something more sensible.
#' It is also come along with the wages-high-school-demo.NLSY79
#' Which is a tagset, that can be uploaded to the web site to recreate the data set.
## ---- raw-data
source(here::here("data-raw/wages-high-school-demo.R"))

#' # Demographic data pre-processing
#' ## Tidy the date of birth data

## ---- dob-tidy
dob_tidy <- new_data_qnames %>%
  dplyr::select(CASEID_1979,
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
  dplyr::select(CASEID_1979,
         dob_month,
         dob_year,
         dob_conflict)

#' Tidy the sex and race data
## ---- demog-tidy
demog_tidy <- categories_qnames %>%
  dplyr::select(CASEID_1979,
         SAMPLE_RACE_78SCRN,
         SAMPLE_SEX_1979) %>%
  rename(gender = SAMPLE_SEX_1979,
         race = SAMPLE_RACE_78SCRN)

#' Tidy the grade completed in each year
## ---- demog-ed
demog_education <- new_data_qnames %>%
  as_tibble() %>%
  rename(HGC_2018 = `Q3-4_2018`) %>%
  dplyr::select(CASEID_1979,
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
  dplyr::select(-var)

# Getting the highest education completed
highest_year <- demog_education %>%
  group_by(CASEID_1979) %>%
  mutate(hgc_i = max(grade)) %>%
  filter(hgc_i == grade) %>%
  filter(year == first(year)) %>%
  rename(yr_hgc = year) %>%
  dplyr::select(CASEID_1979, yr_hgc, hgc_i) %>%
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
      dplyr::select(CASEID_1979,
             starts_with("QES-52A") &
               ends_with(as.character(year)))}
  else{
    temp <- new_data_qnames %>%
      dplyr::select(CASEID_1979,
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
    dplyr::select(CASEID_1979,
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
  # set hours_work = 0 or > 84 to be NA
  mutate(rate_per_hour = ifelse(rate_per_hour == 0, NA,
                                rate_per_hour),
         hours_work = ifelse(hours_work == 0 | hours_work > 84, NA, hours_work))

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
  dplyr::select(-flag1)

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

# calculate the number of observation
keep_me <- wages_demog_hs %>%
  count(id) %>%
  filter(n > 4)

# keep the id who participated at least 5 years in the survey
wages_demog_hs <- wages_demog_hs %>%
  filter(id %in% keep_me$id)

# nest the data by id to build a robust linear model
by_id <- wages_demog_hs %>%
  dplyr::select(id, year, mean_hourly_wage) %>%
  group_by(id) %>%
  nest()

# build a robust linear model
id_rlm <- by_id %>%
  mutate(model = map(.x = data,
                     .f = function(x){
                       rlm(mean_hourly_wage ~ year, data = x)
                     }))
# extract the property of the regression model
id_aug <- id_rlm %>%
  mutate(augmented = map(model, broom::augment)) %>%
  unnest(augmented)

# extract the weight of each observation
id_w <- id_rlm %>%
  mutate(w = map(.x = model,
                 .f = function(x){
                   x$w
                 })) %>%
  unnest(w) %>%
  dplyr::select(w)

# bind the property of each observation with their weight
id_aug_w <- cbind(id_aug, id_w) %>%
  dplyr::select(`id...1`,
                year,
                mean_hourly_wage,
                .fitted,
                .resid,
                .hat,
                .sigma,
                w) %>%
  rename(id = `id...1`)

# if the weight < 0.12, the mean_hourly_wage is replaced by the model's fitted/predicted value.
# and add the flag whether the observation is predicted value or not.
# since the fitted value is sometimes < 0, and wages value could never be negative,
# we keep the mean hourly wage value even its weight < 0.12.

wages_rlm_dat <- id_aug_w %>%
  mutate(wages_rlm = ifelse(w < 0.12  & .fitted >= 0, .fitted,
                            mean_hourly_wage)) %>%
  mutate(is_pred = ifelse(w  < 0.12 & .fitted >= 0, TRUE,
                          FALSE)) %>%
  dplyr::select(id, year, wages_rlm, is_pred)

# join back the `wages_rlm_dat` to `wages_demog_hs`

wages_demog_hs <- left_join(wages_demog_hs, wages_rlm_dat, by = c("id", "year"))

# select out the old value of mean hourly wage and change it with the wages_rlm value
wages_demog_hs <- wages_demog_hs %>%
  dplyr::select(-mean_hourly_wage) %>%
  rename(mean_hourly_wage = wages_rlm)

# rename and select the wages in tidy
wages_hs2020 <- wages_demog_hs %>%
  dplyr::select(id, year, mean_hourly_wage, age_1979, gender, race, hgc, hgc_i, yr_hgc, number_of_jobs, total_hours, is_wm, is_pred) %>%
  mutate(id = as.factor(id),
         hgc = as.factor(hgc),
         year = as.integer(year),
         age_1979 = as.integer(age_1979),
         yr_hgc = as.integer(yr_hgc),
         number_of_jobs = as.integer(number_of_jobs)) %>%
  rename(wage = mean_hourly_wage,
         njobs = number_of_jobs,
         hours = total_hours)

wages <- as_tsibble(x = wages_hs2020,
                       key = id,
                       index = year,
                       regular = FALSE)

# save it to an rda object
usethis::use_data(wages, overwrite = TRUE)


# CREATE THE DATASET FOR THE DOMEGRAPHIC INFORMATION
demog_nlsy79 <- full_demographics %>%
  mutate(age_1979 = 1979 - (dob_year + 1900)) %>%
  dplyr::select(id,
         age_1979,
         gender,
         race,
         hgc,
         hgc_i,
         yr_hgc) %>%
  mutate(id = as.factor(id),
         age_1979 = as.integer(age_1979),
         hgc = as.factor(hgc),
         yr_hgc = as.integer(yr_hgc))


#+ eval=FALSE
# save it to an rda object
usethis::use_data(demog_nlsy79, overwrite = TRUE)

# CREATE THE DATASET FOR THE HIGH SCHOOL DROP-OUT
wages_hs_dropout <- wages_hs2020 %>%
  mutate(dob = 1979 - age_1979,
         age_hgc = yr_hgc - dob) %>%
  filter((hgc %in% c("9TH GRADE",
                     "10TH GRADE",
                     "11TH GRADE")) |
           (hgc == "12TH GRADE" &
              age_hgc >= 19)) %>%
  filter(age_1979 <= 17,
         gender == "MALE") %>%
  dplyr::select(-dob,
         -age_hgc)

wages_hs_do <- as_tsibble(x = wages_hs_dropout,
                          key = id,
                          index = year,
                          regular = FALSE)

#+ eval=FALSE
# save it to an rda object
usethis::use_data(wages_hs_do, overwrite = TRUE)






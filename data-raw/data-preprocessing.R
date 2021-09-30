

## ---- load-pkgs
library(tidyverse)
select <- dplyr::select

## ---- raw-data
source(here::here("data-raw/NLSY79/NLSY79.R"))

## ---- untidy-data
options(width=70)
new_data_qnames %>%
  select(CASEID_1979,
         starts_with("HRP") &
           ends_with(c("1979", "1980")),
         everything()) %>%
  str(list.len = 10)


## ---- dob-tidy
dob_tidy <- new_data_qnames %>%
  select(id = CASEID_1979,
         year_1979 = `Q1-3_A~Y_1979`,
         year_1981 = `Q1-3_A~Y_1981`,
         month_1979 = `Q1-3_A~M_1979`,
         month_1981 = `Q1-3_A~M_1981`) %>%
  mutate(dob_year = case_when(
                      # if the years recorded in both sets match, take 79 data
                      year_1979 == year_1981 ~ year_1979,
                      # if the year in the 81 set is missing, take the 79 data
                      is.na(year_1981) ~ year_1979,
                      # if the sets don't match for dob year, take the 79 data
                      year_1979 != year_1981 ~ year_1979),
         dob_month = case_when(
                      # if months recorded in both sets match, take 79 data
                      month_1979 == month_1981 ~ month_1979,
                      # if month in 81 set is missing, take the 79 data
                      is.na(month_1981) ~ month_1979,
                      # if sets don't match for dob month, take the 79 data
                      month_1979 != month_1981 ~ month_1979),
         # flag if there is a conflict between dob recorded in 79 and 81
         dob_conflict = case_when(
                      !is.na(month_1981) && (month_1979 != month_1981) ~ TRUE,
                      !is.na(year_1981) && (year_1979 != year_1981) ~ TRUE,
                      (year_1979 == year_1981) & (month_1979 == month_1981) ~ FALSE,
                      is.na(month_1981) | is.na(year_1981) ~ FALSE)) %>%
  select(id,
         dob_month,
         dob_year,
         dob_conflict)

has_dob_conflict <- any(dob_tidy$dob_conflict, na.rm = TRUE)



## ---- demog-tidy
demog_tidy <- categories_qnames %>%
  select(id = CASEID_1979,
         race = SAMPLE_RACE_78SCRN,
         gender = SAMPLE_SEX_1979)

## ---- demog-ed
demog_education <- new_data_qnames %>%
  # in 2018, the variable's name is Q3-4_2018, instead of HGC_2018
  rename(HGC_2018 = `Q3-4_2018`) %>%
  select(id = CASEID_1979,
         starts_with("HGCREV"),
         "HGC_2012",
         "HGC_2014",
         "HGC_2016",
         "HGC_2018") %>%
  pivot_longer(!id,
               names_to = "var",
               values_to = "grade") %>%
  separate("var", c("var", "year"), sep = "_") %>%
  filter(!is.na(grade)) %>%
  select(-var)

## ---- tidy-hgc
# Get the highest year completed
highest_year <- demog_education %>%
  group_by(id) %>%
  filter(grade == max(grade),
         year == min(year)) %>%
  rename(yr_hgc = year,
         hgc_i = grade) %>%
  select(id, yr_hgc, hgc_i) %>%
  ungroup() %>%
  mutate(hgc = case_when(hgc_i == 0 ~ "NONE",
                         hgc_i == 1 ~ "1ST GRADE",
                         hgc_i == 2 ~ "2ND GRADE",
                         hgc_i == 3 ~ "3RD GRADE",
                         hgc_i >= 4 & hgc_i <= 12 ~ paste0(hgc_i, "TH GRADE"),
                         hgc_i == 13 ~ "1ST YEAR COL",
                         hgc_i == 14 ~ "2ND YEAR COL",
                         hgc_i == 15 ~ "3RD YEAR COL",
                         hgc_i == 95 ~ "UNGRADED",
                         TRUE ~ paste0((hgc_i - 12), "TH YEAR COL")))

## ---- full-demog
full_demographics <- full_join(dob_tidy, demog_tidy, by = "id") %>%
  full_join(highest_year, by = "id") %>%
  mutate(age_1979 = 1979 - (dob_year + 1900))

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


## ---- save-demog-data
usethis::use_data(demog_nlsy79, overwrite = TRUE)

## ---- tidy-hours
# make a list for years where we used the "QES-52A"
year_A <- c(1979:1987, 1993)
# function to get the hour of work
get_hour <- function(year) {
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

# getting the hours of work of all observations
years <- c(1979:1994, seq(1996, 2018, by = 2))
hours_all <- map_dfr(years, get_hour)

## ---- tidy-rate

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

rates_all <- map_dfr(years, get_rate)

## ---- tidy-rate-hour

# join hours and rates variable
hours_wages <- left_join(rates_all,
                         hours_all,
                         by = c("id", "year", "job")) %>%
  # set the 0 value in rate_per_hour as NA
  mutate(rate_per_hour = ifelse(rate_per_hour == 0, NA, rate_per_hour),
         hours_work = ifelse(hours_work == 0 | hours_work > 84, NA, hours_work))


## ---- tidy-nojob
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
# flag1 = code 1 for weighted mean
# code 0 for arithmetic mean
mean_hourly_wage <- eligible_wages %>%
  group_by(id, year) %>%
  #calculate the weighted mean if the number of jobs > 1
  mutate(wages = ifelse(no_jobs == 1, rate_per_hour/100,
                        weighted.mean(rate_per_hour, hours_work, na.rm = TRUE)/100)) %>%
  #give the flag if it the weighted mean
  mutate(flag1 = ifelse(!is.na(wages) & no_jobs != 1, 1,
                        0)) %>%
  #calculate the arithmetic mean for the na
  mutate(wages = ifelse(is.na(wages), mean(rate_per_hour)/100,
                        wages)) %>%
  group_by(id, year) %>%
  summarise(wages = mean(wages),
            total_hours = sum(hours_work),
            number_of_jobs = mean(no_jobs),
            flag1 = mean(flag1)) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  rename(mean_hourly_wage = wages) %>%
  mutate(is_wm = flag1 == 1) %>%
  select(-flag1)

head(mean_hourly_wage, n = 10)

## ---- wages-demog-hs
# join the wages information and the demographic information by case id.
wages_demog <- left_join(mean_hourly_wage, full_demographics, by="id")
# calculate the years in work force and the age of the subjects in 1979
wages_demog <- wages_demog %>%
  mutate(yr_hgc = as.numeric(yr_hgc)) %>%
  mutate(years_in_workforce = year - yr_hgc) %>%
  mutate(age_1979 = 1979 - (dob_year + 1900))
# filter only the id with high school education
wages_before <- wages_demog  %>% filter(grepl("GRADE", hgc))
# calculate the number of observation
keep_me <- wages_before %>%
  count(id) %>%
  filter(n > 4)
wages_before <- wages_before %>%
  filter(id %in% keep_me$id)

## ---- save-data
saveRDS(wages_before, here::here("paper/results/wages_before.rds"))

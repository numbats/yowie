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


#===== TIDY DEMOGRAPHIC VARIABLES =====

## ---- dob-tidy
dob_tidy <- new_data_qnames %>%
  select(id = CASEID_1979,
         year_1979 = `Q1-3_A~Y_1979`,
         year_1981 = `Q1-3_A~Y_1981`,
         month_1979 = `Q1-3_A~M_1979`,
         month_1981 = `Q1-3_A~M_1981`) %>%
  mutate(dob_year = case_when(year_1979 == year_1981 ~ year_1979,
                      # if the year in the 81 set is missing, take the 79 data
                      is.na(year_1981) ~ year_1979,
                      # if the sets don't match for dob year, take the 79 data
                      year_1979 != year_1981 ~ year_1979),
         dob_month = case_when(month_1979 == month_1981 ~ month_1979,
                      # if month in 81 set is missing, take the 79 data
                      is.na(month_1981) ~ month_1979,
                      # if sets don't match for dob month, take the 79 data
                      month_1979 != month_1981 ~ month_1979),
         # flag if there is a conflict between dob recorded in 79 and 81
         dob_conflict = case_when(!is.na(month_1981) && (month_1979 != month_1981) ~ TRUE,
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
         gender = SAMPLE_SEX_1979) %>%
  mutate(race = as.factor(case_when(race == "NON-BLACK, NON-HISPANIC" ~ "NB, NH",
                          race == "HISPANIC" ~ "H",
                          race == "BLACK" ~ "B")),
         gender = as.factor(case_when(gender == "FEMALE" ~ "F",
                            gender == "MALE" ~ "M")))

## ---- demog-ed

# tidy highest grade completed for each round of the survey
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
  mutate(year = as.numeric(year)) %>%
  #filter(!is.na(grade)) %>%
  select(-var)

# get the highest grade completed ever
hgc_ever <- new_data_qnames %>%
  select(id = CASEID_1979,
         hgc_i = HGC_EVER_XRND) %>%
  mutate(hgc = case_when(hgc_i == 0 ~ "NONE",
                         hgc_i == 1 ~ "1ST GRADE",
                         hgc_i == 2 ~ "2ND GRADE",
                         hgc_i == 3 ~ "3RD GRADE",
                         hgc_i >= 4 & hgc_i <= 12 ~ paste0(hgc_i, "TH GRADE"),
                         hgc_i == 13 ~ "1ST YEAR COL",
                         hgc_i == 14 ~ "2ND YEAR COL",
                         hgc_i == 15 ~ "3RD YEAR COL",
                         TRUE ~ paste0((hgc_i - 12), "TH YEAR COL"))) %>%
  mutate(hgc = ifelse(hgc == "NATH YEAR COL", NA, hgc))

## ---- tidy-hgc

# Get the highest year completed from the calculation using `grade` variable
# This aims to check the consistency between grade and hgc_ever in the database

hgc_calc <- demog_education %>%
  filter(!is.na(grade)) %>%
  group_by(id) %>%
  filter(grade == max(grade)) %>%
  filter(year == min(year)) %>%
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

## check between the hgc ever and the highest year calculated from the data

check_hgc_1 <- left_join(hgc_ever, hgc_calc, by = c("id"), suffix = c("_database", "_calculated")) %>%
  filter(hgc_i_database > hgc_i_calculated)

check_hgc_2 <- left_join(hgc_ever, hgc_calc, by = c("id"), suffix = c("_database", "_calculated")) %>%
  filter(hgc_i_database < hgc_i_calculated)

# we found some issues, i.e., inconsistency between grade and hgc.

# we try to fix this issue
fix_grade_issue_1 <- left_join(demog_education, hgc_ever, by = "id") %>%
  filter(id %in% check_hgc_1$id) %>%
  # there is some cases where the
  group_by(id) %>%
  mutate(new_grade = ifelse((grade < lag(grade)), hgc_i, grade)) %>%
  mutate(new_grade = ifelse(is.na(new_grade), grade, new_grade)) %>%
  select(id, year, grade = new_grade, hgc_i, hgc)

fix_grade_issue_2 <- left_join(demog_education, hgc_ever, by = "id") %>%
  filter(id %in% check_hgc_2$id) %>%
  # there is some cases where the
  group_by(id) %>%
  mutate(grade = ifelse(grade > hgc_i, hgc_i, grade))

# fixed record
grade_fixed <- rbind(fix_grade_issue_1, fix_grade_issue_2)


# filter the unproblematic observations
`%!in%` <- Negate(`%in%`)

grade_ok <- left_join(demog_education, hgc_ever, by = "id") %>%
  filter(id %!in% grade_fixed$id)

# bind already clean grade
grade <- rbind(grade_ok, grade_fixed) %>%
  select(id, year, grade)

# get the highest grade completed reported in the first round of the survey (1979)
hgc_1979 <- grade %>%
  filter(year == 1979) %>%
  select(id, hgc_1979 = grade)

# get the variable oh GED

ged <- new_data_qnames %>%
  select(id = CASEID_1979,
         starts_with("Q3-8A"))%>%
  pivot_longer(!id,
               names_to = "var",
               values_to = "dip_or_ged") %>%
  filter(!is.na(dip_or_ged)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, dip_or_ged)

# join the hgc_ever and GED as education information for NLSY79 demographic data
highest_year <- left_join(hgc_ever, ged, by = "id") %>%
  left_join(hgc_1979, by = "id")

## ---- full-demog
full_demographics <- full_join(dob_tidy, demog_tidy, by = "id") %>%
  full_join(highest_year, by = "id") %>%
  mutate(age_1979 = 1979 - (dob_year + 1900))

# create demographic data
demog_nlsy79 <- full_demographics %>%
  mutate(age_1979 = 1979 - (dob_year + 1900)) %>%
  dplyr::select(id,
                age_1979,
                gender,
                race,
                hgc,
                hgc_i,
                hgc_1979,
                ged = dip_or_ged) %>%
  mutate(id = as.factor(id),
         age_1979 = as.integer(age_1979),
         hgc = as.factor(hgc),
         ged = as.factor(ged))


## ---- save-demog-data
usethis::use_data(demog_nlsy79, overwrite = TRUE)


#==== TIDY THE EMPLOYMENT DATA ===

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

# getting the rate of each job and each year
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

## ---- tidy-start-work
# tidy the variable of year when individual start working
st_work <- new_data_qnames %>%
  select(CASEID_1979,
         `EMPLOYERS_ALL_STARTDATE_ORIGINAL.01~Y_XRND`) %>%
  rename(id = CASEID_1979,
         stwork = `EMPLOYERS_ALL_STARTDATE_ORIGINAL.01~Y_XRND`)

## ---- tidy-work-experience
# calculating individuals' work experience since the first round of the survey
# this is longitudinal value for each round of the survey
exp <- new_data_qnames %>%
  select(CASEID_1979,
         starts_with("WKSWK")) %>%
  pivot_longer(!CASEID_1979, names_to = "exp", values_to = "weeks") %>%
  separate("exp", c("exp", "year"), sep = -4) %>%
  group_by(CASEID_1979) %>%
  mutate(cum_exp = cumsum(coalesce(weeks, 0)) + weeks*0,
         exp_years = cum_exp/52.143,
         year = as.numeric(year)) %>%
  select(CASEID_1979, year, exp_years) %>%
  rename(id = CASEID_1979,
         exp = exp_years)

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
wages_demog <- left_join(mean_hourly_wage, full_demographics, by="id") %>%
  left_join(grade, by = c("id", "year")) %>%
  left_join(st_work, by = "id") %>%
  left_join(exp, by = c("id", "year")) %>%
  mutate(yr_wforce = year - stwork)

# filter IDs with at least three observations
keep_me <- wages_demog %>%
  count(id) %>%
  filter(n >= 3) # XXX why do we need to filter here?

wages_before <- wages_demog %>%
  filter(id %in% keep_me$id)

## ---- save-data
saveRDS(wages_before, here::here("paper/results/wages_before.rds"))

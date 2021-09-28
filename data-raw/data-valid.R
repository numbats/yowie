
library(tidyverse)
library(MASS)
select <- dplyr::select
wages_before <- readRDS(here::here("paper/results/wages_before.rds"))

## ---- rlm

# nest the data by id to build a robust linear model
by_id <- wages_before %>%
  select(id, year, mean_hourly_wage) %>%
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
  select(w)

# bind the property of each observation with their weight
id_aug_w <- cbind(id_aug, id_w) %>%
  select(`id...1`,
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
# since the fitted value is sometimes <0, and wages value could never be negative,
# we keep the mean hourly wage value even its weight < 0.12.

wages_rlm_dat <- id_aug_w %>%
  mutate(wages_rlm = ifelse(w < 0.12  & .fitted >= 0, .fitted,
                            mean_hourly_wage)) %>%
  mutate(is_pred = ifelse(w < 0.12 & .fitted >= 0, TRUE,
                          FALSE)) %>%
  select(id, year, wages_rlm, is_pred)

# join back the `wages_rlm_dat` to `wages_demog_hs`

wages_after <- left_join(wages_before, wages_rlm_dat, by = c("id", "year"))

# save it to rds file so it would faster the knitting process
#+ save-wages-after, eval = FALSE, echo = FALSE
saveRDS(wages_after, here::here("paper/results/wages_after.rds"))

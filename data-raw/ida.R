## ---- load-pkgs2
library(kableExtra)
library(janitor)
library(tsibble)
library(brolgar)
library(patchwork)

## ---- age-table
age_table <- full_demographics %>%
  group_by(age_1979) %>%
  count(age_1979)

age_table %>%
  mutate(n = scales::comma(n)) %>%
  kable(
      caption = "The frequency table of the age at the start of the survey (?CHECK) in the full NSLY79 data",
      col.names = c("Age", "Number of individuals"),
      booktabs = TRUE,
      linesep = "",
      align = "r") %>%
  kable_styling()

## ---- gender-race-table
#' @param x a tabyl with frequency table in "core" attribute
freq_formatting <- function(x) {
  attr(x, "core") %>%
    adorn_totals(c("row", "col")) %>%
    mutate(across(where(is.numeric), ~scales::comma(.x, 1)))
}

gender_race_table <- full_demographics %>%
  tabyl(gender, race) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(., position = "front", ns = freq_formatting(.)) %>%
  mutate(gender = str_to_title(gender))

kable(gender_race_table,
      caption = "The contingency table for gender and race for the full NLSY79 data.",
      col.names = c("Gender", "Hispanic", "Black", "Non-Black, Non-Hispanic", "Total"),
      booktabs = TRUE,
      linesep = "",
      align = "lrrrr") %>%
  kable_classic() %>%
  row_spec(2, hline_after = TRUE) %>%
  add_header_above(c(" " = 1, "Race" = 3, " " = 1))

## ---- summarytable
kable(as.array(summary(wages_before$mean_hourly_wage)),
      caption = "Summary statistics of the hourly wages from the high school dropout subcohort of NLSY79 data",
      col.names = c("Statistics", "Hourly wage ($)"),
      booktabs = TRUE,
      linesep = "") %>%
  kable_styling()

## ---- sample-plot
wages_before_tsibble <- as_tsibble(x = wages_before,
                                     key = id,
                                     index = year,
                                     regular = FALSE)

set.seed(20210225)
ggplot(wages_before_tsibble,
       aes(x = year,
           y = mean_hourly_wage,
           group = id)) +
  geom_line(alpha = 0.7) +
  facet_sample(n_per_facet = 1, n_facets = 20) +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 10, size = 12)) +
  ylab("Hourly wage ($)")


## ---- feature-plot
wages_high <- filter(wages_before, mean_hourly_wage > 500) %>%
  as_tibble() %>%
  head(n = 6)

wages_high2 <- wages_before %>%
  filter(id %in% wages_high$id)

spag <- wages_before %>%
  ggplot(aes(x = year,
             y = mean_hourly_wage,
             group = id)) +
  geom_line(alpha = 0.5) +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     #labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  labs(tag = "(A)") +
  theme_bw() +
  ylab("Hourly wage ($)") #+
  #theme(plot.title = element_text(size = 10))

wages_three_feat <- wages_before_tsibble %>%
  features(mean_hourly_wage,
           feat_three_num
  )
wages_feat_long <- wages_three_feat %>%
  pivot_longer(c(min, med, max),
               names_to = "feature", values_to = "value") %>%
  mutate(feature = factor(feature, levels = c("min", "med", "max")))

feature <- ggplot(wages_feat_long) +
  geom_density(aes(x = value, colour = feature, fill = feature), alpha = 0.3) +
  labs(tag = "(B)") +
  theme_bw() +
  theme(#plot.title = element_text(size = 10),
        #axis.text.x = element_text(angle = 10, size = 6),
        legend.position = "none")

feature_bp <- ggplot(wages_feat_long,
                     aes(y=value, x = feature,
                         fill = feature, color = feature)) +
  geom_boxplot() +
  theme_bw() +
  labs(tag = "(B)")  +
  ylab("Hourly wage ($)") +
  xlab("Feature") +
  theme(legend.position = "none")

plot_high <- ggplot(filter(wages_high2, id == 39)) +
  geom_line(aes(x = year,
                y = mean_hourly_wage)) +
  geom_point(aes(x = year,
                 y = mean_hourly_wage),
             size = 0.5,
             alpha = 0.5) +
  annotate("text", x = 2010, y = 1000, label = "id: 39") +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     #labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  theme_bw() +
  labs(tag = "(C)", y = "Hourly wage ($)")

#spag + feature + feature_bp + plot_high + plot_layout(nrow = 1, guides = "collect") &
#  theme(legend.position = "bottom")
spag + feature_bp + plot_high +
  plot_layout(nrow = 1, guides = "collect") #&
  #theme(legend.position = "bottom")


## ---- compare-data
set.seed(31251587)

wages_cleaned <- readRDS(here::here("paper/results/wages_after.rds"))
sample_id <- sample(unique(wages_cleaned$id), 20)
sample <- subset(wages_cleaned, id %in% sample_id)

wages_compare <- sample %>%
  select(id, year, mean_hourly_wage, wages_rlm) %>%
  rename(mean_hourly_wage_rlm = wages_rlm) %>%
  pivot_longer(c(-id, -year), names_to = "type", values_to = "wages")

## ---- compare-plot
ggplot(wages_compare) +
  geom_line(aes(x = year,
                y = wages,
                colour = type,
                linetype = type),
            alpha = 1, size=1.3) +
  facet_wrap(~id, scales = "free_y") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 10, size = 5),
        legend.position = "bottom") +
  scale_linetype_manual("",
              #values = c("mean_hourly_wage","mean_hourly_wage_rlm"),
              values = c("11", "solid"),
              labels = c("Before", "After")) +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  #xlab("Year") +
  ylab("Hourly wage ($)") +
#  scale_colour_brewer("", palette = "Dark2", direction = -1,
  scale_colour_grey("", labels = c("Before", "After"))
#  scale_color_hue(labels = c("Before", "After"))

## ---- fixed-feature-plot
spag2 <- wages_cleaned %>%
  ggplot(aes(x = year,
             y = wages_rlm,
             group = id)) +
  geom_line(alpha = 0.1) +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  theme_bw() +
  labs(tag = "(A)") +
  theme(plot.title = element_text(size = 10)) +
  ylab("Hourly wage ($)")


wages_hs2020_rlm <- as_tsibble(x = wages_cleaned,
                               key = id,
                               index = year,
                               regular = FALSE)
wages_three_feat_rlm <- wages_hs2020_rlm %>%
  features(wages_rlm,
           feat_three_num
  )
wages_feat_long_rlm <- wages_three_feat_rlm %>%
  pivot_longer(c(min, med, max), names_to = "feature", values_to = "value") %>%
  mutate(feature = factor(feature, levels = c("min", "med", "max")))

feature2 <- ggplot(wages_feat_long_rlm) +
  geom_density(aes(x = value, colour = feature, fill = feature), alpha = 0.3) +
  labs(tag = "(B)") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  xlab("value (log10)")


feature2_bp <- ggplot(wages_feat_long_rlm,
                     aes(y=value, x = feature,
                         fill = feature, color = feature)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Feature",
       y = "Hourly wage ($)",
       tag = "(B)") +
  theme(legend.position = "none")

plot_high_after <- ggplot(filter(wages_cleaned, id == 39)) +
  geom_line(aes(x = year,
                y = wages_rlm)) +
  geom_point(aes(x = year,
                 y = wages_rlm),
             size = 0.5,
             alpha = 0.5) +
  annotate("text", x = 1985, y = 26, label = "id: 39") +
  scale_x_continuous("Year",
                     breaks = seq(1980, 2020, 10),
                     labels = c("80", "90", "00", "10", "20"),
                     minor_breaks = seq(1980, 2020, 5)) +
  theme_bw() +
  labs(tag = "(C)", y = "Hourly wage ($)")


spag2 + feature2_bp + plot_high_after

## ---- save-data
# select out the old value of mean hourly wage and change it with the wages_rlm value
wages_clean <- wages_cleaned %>%
  select(-mean_hourly_wage) %>%
  rename(mean_hourly_wage = wages_rlm)

# rename and select the wages in tidy
wages <- wages_clean %>%
  select(id, year, mean_hourly_wage, age_1979, gender, race, hgc, hgc_i, yr_hgc,
                number_of_jobs, total_hours, is_wm, is_pred) %>%
  mutate(id = as.factor(id),
         hgc = as.factor(hgc),
         year = as.integer(year),
         age_1979 = as.integer(age_1979),
         yr_hgc = as.integer(yr_hgc),
         number_of_jobs = as.integer(number_of_jobs)) %>%
  rename(wage = mean_hourly_wage,
         njobs = number_of_jobs,
         hours = total_hours) %>%
  as_tsibble(key = id,
             index = year,
             regular = FALSE)

# Create a data set for demographic variables
demog_nlsy79 <- full_demographics %>%
  select(id,
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

# Create a data set for the high school dropouts cohort
wages_hs_do <- wages %>%
  mutate(dob = 1979 - age_1979,
         age_hgc = yr_hgc - dob) %>%
  filter((hgc %in% c("9TH GRADE",
                     "10TH GRADE",
                     "11TH GRADE")) |
           (hgc == "12TH GRADE" &
              age_hgc >= 19)) %>%
  filter(age_1979 <= 17,
         gender == "MALE") %>%
  select(-dob,
                -age_hgc) %>%
  as_tsibble(key = id,
             index = year,
             regular = FALSE)

### ---- nrow
a <- nrow(categories_qnames)
b <- nrow(full_demographics)
d <- eligible_wages %>%
  group_by(id) %>%
  count(id) %>%
  nrow()
n_hgc <- wages %>%
  as_tibble() %>%
  group_by(id) %>%
  count(id) %>%
  nrow()
n_obs_hgc <- nrow(wages)
n_obs_hgc_pred <- filter(wages, is_pred == TRUE) %>%
  nrow()

n_do <- wages_hs_do %>%
  as_tibble() %>%
  group_by(id) %>%
  count(id) %>%
  nrow()
n_obs_do <- nrow(wages_hs_do)
n_obs_do_pred <- filter(wages_hs_do, is_pred == TRUE) %>%
  nrow()

### ---- flow-chart
include_graphics(here::here("paper/figures/flowchart.png"))
# grViz("digraph flowchart {
#       node [fontname = Helvetica, shape = rectangle]
#       tab1 [label = '@@1']
#       tab2 [label = '@@2']
#       tab3 [label = '@@3']
#       tab4 [label = '@@4']
#       tab5 [label = '@@5']
#       tab6 [label = '@@6']
#       tab7 [label = '@@7']
#
#       tab1 -> tab2;
#       tab1 -> tab3;
#       tab1 -> tab4 -> tab5 -> tab6;
#       tab5 -> tab7;
#       tab6 -> tab7;
#       tab2 -> tab7
#       }
#
#       [1]: paste0('NLSY79, n = ', a)
#       [2]: paste0('Extract demographic variable, n = ', b)
#       [3]: paste0('Exclude ', a - d, ' IDs whose hourly rate is missing')
#       [4]: paste0('Eligible ID, n = ', d)
#       [5]: paste0('Cohort whose hgc is up to 12th grade and \\n participated at least 5 years in the survey, \\n n = ', n_hgc, ', n_obs = ', n_obs_hgc, ' \\n (', n_obs_hgc_pred, ' observations are predicted value)')
#       [6]: paste0('High school dropouts cohort, \\n n = ', n_do, ', n_obs = ', n_obs_do, ' \\n (', n_obs_do_pred, ' observations are predicted value)')
#       [7]: 'yowie Package'
#       ",
#             height = 700)

### ---- flow-chart-blind
include_graphics(here::here("paper/figures/flowchart_blind.png"))


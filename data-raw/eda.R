## ---- sw_wages

sw <- brolgar::wages

sw_wages <- sw %>%
  ggplot(aes(x = xp,
             y = ln_wages)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE, colour = "pink") +
  labs(tag = "(A)", title = "Original") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Experience in years") +
  #theme(plot.title = element_text(size = 10)) +
  ylim(0, 5)

sw_wages_mod <- sw %>%
  as_tibble() %>%
  mutate(hgc = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  mutate(race = case_when(black == 1 ~ "black",
                          hispanic == 1 ~ "hispanic",
                          TRUE ~ "white")) %>%
  mutate(race = factor(race)) %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             linetype = hgc)) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "black") +
  labs(tag = "(A)", title = "Original") +
  scale_linetype("") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Experience in years") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

## ---- do_refreshed

# singer-willet did not stated how they calculate experience
# in database, there is work experience topic
# (https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/employment/work-experience)
# but there is no variable explicitly mention about experience.
# in this case, we approximate it with year-1979

# filter do data up to 1994 since the textbook data only covers that period.

do <- yowie::wages_hs_do %>%
  mutate(lnwage = log(wage)) #%>%
  #filter(year < 1995)

do_ref <- do %>%
  ggplot(aes(x = year,
             y = lnwage)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE, colour = "pink") +
  labs(tag = "(B)", title = "Refreshed data") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Year of data collection") +
  theme(plot.title = element_text(size = 10)) +
  ylim(0, 5)

do_ref_mod <- do %>%
  mutate(hgc12 = ifelse(hgc_i < 12, "BELOW 12TH", "12TH")) %>%
  ggplot(aes(x = year,
             y = lnwage,
             linetype = hgc12)) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "black") +
  labs(title = "Refreshed data", tag = "(B)") +
  scale_linetype("") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Year of data collection") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# sw_wages + do_ref

# the two plots are not comparable
# since the id who are in Singer-Willet and the id in do-refreshed might be different


## --- compare-sw-do
sw_id <- as_tibble(sw) %>%
  group_by(id) %>%
  count() %>%
  select(id)

do_id <- as_tibble(do) %>%
  group_by(id) %>%
  mutate(id = as.numeric(id)) %>%
  count() %>%
  select(id)

# check common id in data set
sw_do <- inner_join(sw_id, do_id, by = "id")

# only have 94 id that agree upon each other
sw_agree <- filter(sw, id %in% sw_do$id)
do_agree <- filter(do, id %in% sw_do$id)

sw_do_sample <- sample(sw_do$id, 12)

sw_agree_sample <- sw_agree %>%
  filter(id %in% sw_do_sample) %>%
  select(id, xp, ln_wages) %>%
  as_tibble()
do_agree_sample <- do_agree %>%
  mutate(id = as.integer(as.character(id))) %>%
  filter(id %in% sw_do_sample) %>%
  select(id, year, lnwage) %>%
  as_tibble()

agree_ref <- ggplot() +
  geom_line(data=do_agree_sample,
            aes(x = year-1979,
                y = lnwage), colour = "black") +
  geom_line(data=sw_agree_sample,
            aes(x = xp,
                y = ln_wages), colour = "grey70") +
  facet_wrap(~id)

## ---- plotting-sw-do

sw_wages_agree <- sw_agree %>%
  ggplot(aes(x = xp,
             y = ln_wages)) +
  geom_line(aes(group = id),alpha = 0.1) +
  geom_smooth(se = FALSE) +
  labs(tag = "(A)") +
  theme_bw() +
  ylab("ln(Hourly wage) ($)") +
  xlab("Experience (years)") +
  theme(plot.title = element_text(size = 10)) +
  ylim(-3, 5)


do_ref_agree <- do_agree %>%
  ggplot(aes(x = year,
             y = lnwage)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE) +
  labs(tag = "B") +
  theme_bw() +
  ylab("ln(Hourly wage) ($)") +
  xlab("Year") +
  theme(plot.title = element_text(size = 10)) +
  ylim(-3, 5)

# sw_wages_agree + do_ref_agree
# sw_wages + do_ref
sw_wages_mod + do_ref_mod

# ---- summaries
sw2 <- sw %>%
  as_tibble() %>%
  mutate(hgc = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  mutate(race = case_when(black == 1 ~ "black",
                          hispanic == 1 ~ "hispanic",
                          TRUE ~ "white")) %>%
  mutate(race = factor(race))
sw2 %>% select(id, race) %>% distinct() %>% count(race)
sw2 %>% select(id, hgc) %>% distinct() %>% count(hgc)

do2 <- do %>%
  as_tibble() %>%
  mutate(hgc12 = ifelse(hgc_i < 12, "BELOW 12TH", "12TH"))
do2 %>% select(id, race) %>% distinct() %>% count(race)
do2 %>% select(id, hgc12) %>% distinct() %>% count(hgc12)



# Takeaways:

# we don't know how exactly the criteria of drop out,
# resulted view agreements of id being dropped out in the two dataset.

# with the same id, the ln(wages) didn't agree each other, probably due to:

# we don't have experience variable in the original data base.
# we want to calculate it, but we don't know how Singer-Willet compute the experience
# we want to compare it just with year, Singer and Willet dont have year variable
# hence, it is hard to create key-index pair.


# for people who work show data example (book, research, ets), it is very important to disclose how the data derived from the initial source, to make the analysis reproducible, especially for longitudinal data because other researcher often want to compare with the recent data.
# in this case, in Singer-Willet textbook, it important to state how the experience variable derived from the database.
# In this EDA, we found that it is difficult to make apple to apple comparison (and refresh the data in general) cause we don't know how exactly they calculate the variable (even the wages)


# This is why in our paper, we show the practice of reproducible and responsible data cleaning work flow to make sure the data can be refreshed.
# Especially with longitudinal data from a survey that is still being held (so it will be refreshed from year to year).






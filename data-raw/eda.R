## ---- sw_wages

# load the original data
sw <- brolgar::wages %>%
  group_by(id) %>%
  mutate(index = 1:n(),
         id = as.factor(id))

## ---- calculate-summaries-on-subsets
# Comparing the original and refreshed data
# tabulating hgc
sw_hgc <- sw %>%
  as_tibble() %>%
  select(id, high_grade) %>%
  distinct() %>%
  count(high_grade) %>%
  rename(hgc = high_grade, original = n)

do_1994 <- wages_hs_do %>%
  filter(year <= 1994) %>%
  as_tibble() %>%
  group_by(id) %>%
  mutate(hgc_1994 = max(grade[between(year, 1979, 1994)],
                        na.rm=TRUE)) %>%
  ungroup()

d94_hgc <- do_1994 %>%
  select(id, hgc_1979) %>%
  filter(between(hgc_1979, 6, 12)) %>%
  distinct() %>%
  count(hgc_1979) %>%
  rename(hgc = hgc_1979, refreshed = n)

hgc_join <- left_join(sw_hgc, d94_hgc) %>%
  mutate(original = -original, hgc = factor(hgc)) %>%
  pivot_longer(cols = c(original, refreshed),
               names_to = "subset",
               values_to = "count")

hgc_p <- ggplot(hgc_join, aes(x=hgc, y=count, fill=subset)) +
  geom_col(alpha=0.9) +
  facet_wrap(~subset, scales = "free_x") +
  scale_y_continuous("Count",
                     breaks = c(-300, -200, -100, 0, 100, 200, 300),
                     labels = c(300, 200, 100, 0, 100, 200, 300),
                     expand = c(0, 0)) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("(A)") +
  coord_flip() +
  theme_bw() +
  theme(panel.spacing.x = unit(0, "mm"),
        legend.position = "none")

# compare distributions of exp
exp_sw <- sw %>%
  as_tibble() %>%
  select(xp) %>%
  rename(exp=xp) %>%
  mutate(subset = "original")

exp_d94 <- do_1994 %>%
  as_tibble() %>%
  select(exp) %>%
  mutate(subset = "refreshed")

exp_join <- bind_rows(exp_sw, exp_d94)

exp_p <- ggplot(exp_join) +
  geom_density(aes(x=exp, colour=subset, fill=subset), alpha=0.9) +
  facet_wrap(~subset, ncol=1) +
  xlim(c(0,13)) +
  xlab("Experience") + ylab("") +
  scale_fill_brewer("", palette="Dark2") +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("(B)") +
  theme_bw() +
  theme(legend.position = "none")

# tabulate race
sw_d <- sw %>%
  as_tibble() %>%
  select(id, black, hispanic) %>%
  distinct() %>%
  count(black, hispanic)

d94_d <- do_1994 %>%
  select(id, race) %>%
  distinct() %>%
  count(race)

# compare density of wages
wg_sw <- sw %>%
  as_tibble() %>%
  select(ln_wages) %>%
  mutate(subset = "original")

wg_d94 <- do_1994 %>%
  as_tibble() %>%
  mutate(ln_wages = log(wage)) %>%
  select(ln_wages) %>%
  mutate(subset = "refreshed")

wg_join <- bind_rows(wg_sw, wg_d94)

wg_p <- ggplot(wg_join) +
  geom_density(aes(x=ln_wages, colour=subset, fill=subset), alpha=0.9) +
  facet_wrap(~subset, ncol=1) +
  xlim(c(0,4)) +
  xlab("Wages (natural log)") + ylab("") +
  scale_fill_brewer("", palette="Dark2") +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("(C)") +
  theme_bw() +
  theme(legend.position = "none")

##  ---- compare-subsets
hgc_p + exp_p + wg_p

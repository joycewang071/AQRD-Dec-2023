library(haven)
library(tidyverse)
library(fixest)
library(glue)
library(broom)
library(modelsummary)

data <- read_csv("Weibo_1215.csv")

weibo <- data %>%
  select(date, weibo_id, Nationalistic, user_id, IP_May, SH) %>%
  distinct() %>%
  mutate(
    Nationalistic_binary = (Nationalistic > 0.3),
    week = as.numeric(format(date - 1, "%U"))
  )

user <- data %>%
  select(user_id, female, SH, IP_May, verified, user_fans_count) %>%
  distinct()


covid <- data %>%
  select(date, IP_May, today_confirm, today_heal, today_dead, SH) %>%
  distinct()
covid_week <- covid %>%
  mutate(week = as.numeric(format(date - 1, "%U"))) %>%
  group_by(IP_May, week) %>%
  summarize(
    week_confirm = sum(today_confirm),
    week_heal = sum(today_heal),
    week_dead = sum(today_dead)
  )


weibo_week <- weibo |>
  group_by(week, user_id, SH, IP_May) |>
  summarize(
    count = sum(Nationalistic_binary, na.rm = TRUE),
    propotion = mean(Nationalistic_binary, na.rm = TRUE)
  ) |>
  mutate(lockdown_week = (week >= 13 & (SH == TRUE)))

t_week <- weibo_week %>%
  ungroup() %>%
  select(SH, week, lockdown_week) %>%
  distinct() |>
  group_by(SH) |>
  arrange(week) |>
  mutate(
    lockdown_lag_1 = lag(lockdown_week, 1),
    lockdown_lag_2 = lag(lockdown_week, 2),
  )

data1_week <- left_join(weibo_week, t_week)

data_week2 <- left_join(data1_week, user)

data_week3 <- left_join(data_week2, covid_week)

count_model <- modelsummary(
  list(
    "count(1)" = feols(count ~ lockdown_week | IP_May + week, data1_week),
    "(2)" = feols(count ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 |
      IP_May + week, data1_week),
    "(3)" = feols(count ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 + female + verified + scale(user_fans_count) | IP_May + week, data_week2),
    "(4)" = feols(count ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 + female + verified + scale(user_fans_count) + scale(week_confirm) + scale(week_heal) + scale(week_dead) | IP_May + week, data_week3)
  ),
  gof_map = c("nobs", "r.squared", "FE: IP_May", "FE: week"),
  stars = TRUE,
  statistic = "std.error",
  #output = "count.tex"
  output = "gt"
)


proportion_model <- modelsummary(
  list(
    "propotion(1)" = feols(propotion ~ lockdown_week | IP_May + week, data1_week),
    "(2)" = feols(propotion ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 |
      IP_May + week, data1_week),
    "(3)" = feols(propotion ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 + female + verified + scale(user_fans_count) | IP_May + week, data_week2),
    "(4)" = feols(propotion ~ lockdown_week + lockdown_lag_1 + lockdown_lag_2 + female + verified + scale(user_fans_count) + scale(week_confirm) + scale(week_heal) + scale(week_dead) |
      IP_May + week, data_week3)),
  gof_map = c("nobs", "r.squared", "FE: IP_May", "FE: week"),
  stars = TRUE,
  statistic = "std.error",
  #output = "propotion.tex"
  output = "gt"
)

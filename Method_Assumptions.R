library(haven)
library(tidyverse)
library(fixest)
library(broom)
library(patchwork)

data <- read_csv("Weibo_1215.csv")

weibo <- data %>%
  select(date, weibo_id, Nationalistic, user_id,IP_May,SH) %>%
  distinct() %>% 
  mutate(Nationalistic_binary = (Nationalistic> 0.3),
         week=as.numeric(format(date-1, "%U")))

weibo_week <- weibo |> 
  group_by(week,user_id,SH,IP_May) |>
  summarize(count = sum(Nationalistic_binary, na.rm = TRUE),
            propotion = mean(Nationalistic_binary, na.rm = TRUE)) |>
  mutate(lockdown_week=(week >= 13 & (SH == TRUE))) 

t_week <- weibo_week %>% ungroup() %>%
  select(SH, week, lockdown_week) %>%
  distinct() |>
  group_by(SH) |>
  arrange(week) |> 
  mutate(
    lockdown_lag_1 = lag(lockdown_week, 1),
    lockdown_lag_2 = lag(lockdown_week, 2),
    lockdown_lag_3 = lag(lockdown_week, 3),
    lockdown_lag_4 = lag(lockdown_week, 4),
    lockdown_lead_1 = lead(lockdown_week, 1),
    lockdown_lead_2 = lead(lockdown_week, 2),
    lockdown_lead_3 = lead(lockdown_week, 3),
    lockdown_lead_4 = lead(lockdown_week, 4),  )

data1_week <- left_join(weibo_week, t_week)

fit_count_week<-feols(count ~ lockdown_week +lockdown_lag_1+lockdown_lag_2+lockdown_lag_3+ lockdown_lead_1+lockdown_lead_2
                +lockdown_lead_3 
                #+ lockdown_lag_4 + lockdown_lead_4
                | IP_May + week, data1_week)



count_plot <- fit_count_week |>
  tidy() |>
  mutate(term = recode_factor(
    term,
    # lockdown_lead_4TRUE = "-4",
    lockdown_lead_3TRUE = "-3",
    lockdown_lead_2TRUE = "-2",
    lockdown_lead_1TRUE = "-1",
    lockdown_weekTRUE = "0",
    lockdown_lag_1TRUE ="1",
    lockdown_lag_2TRUE ="2",
    lockdown_lag_3TRUE ="3",
    # lockdown_lag_4TRUE ="4",      
  )) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0.1) +
  labs(y = "Lockdown Effect",
       x = "Week",
       title ="DV: Number of Nationalistic Posts")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),) 


fit_prop_week<-feols(propotion ~ lockdown_week +lockdown_lag_1+lockdown_lag_2+lockdown_lag_3+ lockdown_lead_1+lockdown_lead_2
                      +lockdown_lead_3 
                      #+ lockdown_lag_4 + lockdown_lead_4
                      | IP_May + week, data1_week)



prop_plot <- fit_prop_week |>
  tidy() |>
  mutate(term = recode_factor(
    term,
    # lockdown_lead_4TRUE = "-4",
    lockdown_lead_3TRUE = "-3",
    lockdown_lead_2TRUE = "-2",
    lockdown_lead_1TRUE = "-1",
    lockdown_weekTRUE = "0",
    lockdown_lag_1TRUE ="1",
    lockdown_lag_2TRUE ="2",
    lockdown_lag_3TRUE ="3",
    # lockdown_lag_4TRUE ="4",      
  )) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0.1) +
  labs(y = "Lockdown Effect",
       x = "Week",
       title ="DV: Proportion of Nationalistic Posts")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),) 

combined_plots <- count_plot + prop_plot

ggsave("Event_study.png", combined_plots, width = 16, height = 8)

# Covariate  Balance

pre_treatment_data<- data |>
  subset( date <= "2022-03-28")

pre_treatment_user <- pre_treatment_data |> select(SH,user_id,female,verified, user_fans_count) |> distinct()

t.test(pre_treatment_user$user_fans_count ~ pre_treatment_user$SH, na.rm = TRUE)

t.test(pre_treatment_user$female ~ pre_treatment_user$SH, na.rm = TRUE)

t.test(pre_treatment_user$verified ~ pre_treatment_user$SH, na.rm = TRUE)
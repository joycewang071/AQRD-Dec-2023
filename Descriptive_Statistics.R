#install.packages("ggpubr")
#library(ggpubr)

library(haven)
library(tidyverse)
library(gt)
library(patchwork)

data <- read_csv("Weibo_1215.csv")

weibo <- data %>%
  select(date, weibo_id, Nationalistic, user_id,IP_May,SH) %>%
  distinct() %>% 
  mutate(Nationalistic_binary = (Nationalistic> 0.3),
         week=as.numeric(format(date-1, "%U")))

# user-post -----
user_week <- weibo |> 
  group_by(user_id,SH) |>
  summarize(count = sum(Nationalistic_binary, na.rm = TRUE),
            propotion = mean(Nationalistic_binary, na.rm = TRUE)) 

des_user_week <- user_week |> ungroup()|>select(count,propotion) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    max= max(value, na.rm = TRUE),
    min= min(value, na.rm = TRUE),
    n = sum(!is.na(value))) |>
  mutate(name = recode_factor(
    name,
    count = "Number of Nationalistic Posts",
    propotion = "Proportion of Nationalistic Posts")) |>
  arrange(name) |>
  gt() |>
  ## rename column names
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev",max = "Max",
             min= "Min",
             n ="N",name = "") |>
  ## round
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  ## add commas to integers
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# user-info ----
user <- data %>%
  select(user_id, gender,verified,user_fans_count,SH) %>%
  distinct() |>
  mutate(
    gender = if_else(gender == "f", 1, 0),
    verified = if_else(verified == TRUE, 1, 0)
  )
des_user <- user |> 
  select(gender,verified,user_fans_count) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    max= max(value, na.rm = TRUE),
    min= min(value, na.rm = TRUE),
    n = sum(!is.na(value))) |>
  mutate(name = recode_factor(
    name,
    gender = "Gender (female)",
    verified = "Verified",
    user_fans_count = "Fans Number")) |>
  arrange(name) |>
  gt() |>
  ## rename column names
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev",max = "Max",
             min= "Min",
             n ="N",name = "") |>
  ## round
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  ## add commas to integers
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )


# covid -----
covid <- data %>%
  select(date,IP_May,today_confirm, today_heal, today_dead,SH) %>%
  distinct()
covid_week <- covid %>% 
  mutate(week=as.numeric(format(date-1, "%U"))) %>%
  group_by(IP_May,week,SH) %>%
  summarize (
    week_confirm =sum(today_confirm),
    week_heal=sum(today_heal),
    week_dead = sum(today_dead)
  )


des_covid_week <- covid_week |> ungroup()|>
  select(week_confirm, week_heal, week_dead) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    max= max(value, na.rm = TRUE),
    min= min(value, na.rm = TRUE),
    n = sum(!is.na(value))) |>
  mutate(name = recode_factor(
    name,
    week_confirm = "Weekly Confirmed Case",
    week_heal = "Weekly Healed Case",
    week_dead = "Weekly Dead Case")) |>
  arrange(name) |>
  gt() |>
  ## rename column names
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev",max = "Max",
             min= "Min",
             n ="N",name = "") |>
  ## round
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  ## add commas to integers
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

des_covid_week

# Covid case trend -----

library(ggplot2)

covid_week_sh <- covid %>% 
  mutate(week=as.numeric(format(date-1, "%U"))) %>%
  group_by(SH,week) %>%
  summarize (
    week_confirm =sum(today_confirm),
    week_heal=sum(today_heal),
    week_dead = sum(today_dead)
  )


# Create a plot for week_heal comparing SH and non-SH

# plot_week_heal <- ggplot(covid_week_sh, aes(x = week, y = week_heal, color = SH)) +
#   geom_line(size = 1) +
#   labs(title = "Weekly Healed Case",
#        x = "Week",
#        y = "",
#        color = "")+
#   scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey"),
#                      labels = c("Shanghai", "Non-Shanghai")) +  
#   geom_vline(xintercept = 13, linetype = "dashed", color = "red", size = 1) + 
#   theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5),
#         text = element_text(size = 15),
#         panel.grid.major = element_blank(),  # Remove major gridlines
#         panel.grid.minor = element_blank(),  # Remove minor gridlines
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#         legend.text = element_text(size=15)) 

plot_week_heal <- ggplot(covid_week_sh, aes(x = week, y = week_heal, color = SH)) +
  geom_line(size = 1) +
  labs(title = "Weekly Healed Case",
       x = "Week",
       y = "",
       color = "")+
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  guides(color = "none") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red", size = 1) + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),
  ) 

plot_week_confirm <- ggplot(covid_week_sh, aes(x = week, y = week_confirm, color = SH)) +
  geom_line(size = 1) +
  labs(title = "Weekly Confirmed Case",
       x = "Week",
       y = "Case Number") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  guides(color = "none") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red", size = 1) + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),
        ) 




plot_week_dead <- ggplot(covid_week_sh, aes(x = week, y = week_dead, color = SH)) +
  geom_line(size = 1) +
  labs(title = "Weekly Dead Case",
       x = "Week",
       y = "") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  guides(color = "none") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red", size = 1) + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),) 


#figure <- ggarrange(plot_week_confirm, plot_week_dead, plot_week_heal,
#                    labels = c("A", "B", "C"),
#                    ncol = 3, nrow = 1)

combined_plots <- plot_week_confirm + plot_week_dead + plot_week_heal

ggsave("covid.png", combined_plots, width = 16, height = 5)

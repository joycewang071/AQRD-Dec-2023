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

weibo_week <- weibo |> 
  group_by(week,user_id,SH,IP_May) |>
  summarize(count_nationalism = sum(Nationalistic_binary, na.rm = TRUE),
            propotion = mean(Nationalistic_binary, na.rm = TRUE),
            count = n()) |>
  mutate(lockdown_week=(week >= 13 & (SH == TRUE))) 

library(dplyr)
library(ggplot2)

user_summary <- weibo_week %>% ungroup() |>
  group_by(SH, week) %>%
  summarize(
    avg_count = mean(count),
    avg_count_nationalism = mean(count_nationalism),
    avg_proportion = mean(propotion)
  ) %>%
  ungroup() |> filter(week !=5)

count_nationalism_plot <- ggplot(user_summary, aes(x = week, y = avg_count_nationalism, color = SH)) +
  geom_line(size = 1) +
  guides(color = "none") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red", size =1) +  # Add the vertical line
  labs(
    x = "Week",
    y = "Average Nationalistic Count",
    title = "Average Number of Nationalistic Posts"
  ) +
  scale_x_continuous(breaks = unique(weibo_week$week)) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey"))+  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),) 

count_nationalism_plot


proportion_plot <- ggplot(user_summary, aes(x = week, y = avg_proportion, color = SH)) +
  geom_line(size = 1) +
  guides(color = "none") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red", size =1) +
  labs(
    x = "Week",
    y = "Average Proportion",
    title = "Average Proportion of Nationalistic Posts"
  ) +
  scale_x_continuous(breaks = unique(weibo_week$week)) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey"))+  
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        text = element_text(size = 20),) 



combined_plots <- count_nationalism_plot + proportion_plot

ggsave("trend.png", combined_plots, width = 16, height = 5)

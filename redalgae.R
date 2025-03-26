library(tidyverse)
library(janitor)
library(plotrix)
library(cowplot)
theme_set(theme_cowplot())


algae <- read_csv("Lauren-algae.csv")



clean_algae <- clean_names(algae) %>% 
  filter(!is.na(treatment))


clean_algae_sum <- clean_algae %>% 
  group_by(day,treatment, flask_id) %>% 
  summarize(mean_count = mean(cell_count),
            total_count = sum(cell_count),
            STD_count= std.error(cell_count))

algae_growth <- clean_algae_sum %>% 
  select(mean_count, treatment, day, flask_id) %>% 
  group_by(treatment) %>% 
  spread(key= day, value = mean_count) %>% 
  mutate(growth_rate = (`6` - `0`)/6)

algae_growth2 <- clean_algae_sum %>% 
  select(total_count, treatment, day, flask_id) %>% 
  group_by(treatment) %>% 
  spread(key= day, value = total_count) %>% 
  mutate(growth_rate = (`6` - `0`)/6)

algae_growth3 <- clean_algae_sum %>% 
  select(total_count, treatment, day, flask_id) %>% 
  group_by(treatment) %>% 
  spread(key= day, value = total_count) %>% 
  mutate(growth_rate = (`6` - `0`)/6) %>% 
  group_by(treatment) %>% 
  summarize(mean_growth = mean(growth_rate),
            se_growth= std.error(growth_rate))


algae_growth3 %>% 
  ggplot(aes(x= treatment, y= mean_growth)) + geom_point() + 
  geom_point(aes(x = treatment, y = growth_rate), data = algae_growth2, color = "blue") +
  geom_errorbar(aes(x= treatment, ymin = mean_growth - se_growth, ymax= mean_growth + se_growth), width = 0.1) +
  ylab("Growth rate (change in RFU per day)") + xlab("Treatment")
ggsave("figures/growth-rates.png", width = 6, height = 4)




algae_growth2 %>% 
  ggplot(aes(x = treatment, y = growth_rate, color = flask_id)) + geom_point()




clean_algae %>% 
  filter(!is.na(treatment)) %>% 
  ggplot(aes(x = treatment, y = cell_count)) + geom_bar(stat= "identity") +
  facet_wrap( ~ day)

clean_algae_sum %>% 
  filter(!is.na(treatment)) %>% 
  ggplot(aes(x= treatment, y= mean_count)) + geom_point() + 
  geom_errorbar(aes(x= treatment, ymin = mean_count-STD_count, ymax= mean_count + STD_count)) +
  facet_wrap( ~ day, scales = "free")

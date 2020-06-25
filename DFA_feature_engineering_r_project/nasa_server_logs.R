library(tidyverse)
library(anytime)
library(Hmisc)

# Load data
nasa <- read_csv("/Users/Jack/Documents/data.csv")

# Data transformations
nasa <- nasa[c(1:2000),]

nasa <- nasa %>%
  select(-X1, -method, -url) %>%
  filter(bytes<3e+05) %>%
  mutate(time = anytime(time)) %>%
  mutate(day = weekdays(as.Date(time)))

nasa <- nasa[,c(1,4,2,5,3)]

nasa_grouped <- nasa %>% 
  group_by(host) %>% 
  summarise(avg_bytes = mean(bytes), 
            total_bytes = sum(bytes))

nasa <- merge(nasa, 
              nasa_grouped, 
              by = "host")

# Visualizations
nasa %>% ggplot(aes(x=day)) +
  geom_bar()

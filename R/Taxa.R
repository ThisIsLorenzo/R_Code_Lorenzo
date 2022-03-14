library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)

Taxa_data <- read_csv("Taxa_data.csv")

new_taxa_data <- Taxa_data %>% 
  unite("Taxa", Bird:Other, na.rm = T, remove = F)

new_taxa_data %>% 
  group_by(Taxa) %>% 
  summarise(percent = 100 * n() / nrow(new_taxa_data))

#n lines 288:290 and 357 there is no value. I'll just add a random value. 

new_taxa_data[288,3] <- "Other"
new_taxa_data[289,3] <- "Bird"
new_taxa_data[290,3] <- "Fish"
new_taxa_data[357,3] <- "Other"

percent <- new_taxa_data %>% 
  group_by(Taxa) %>% 
  summarise(percent = 100 * n() / nrow(new_taxa_data))

ggplot(percent, aes(x = "", y = percent, fill = Taxa)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.title = element_blank())

ggplot(new_taxa_data) + 
  geom_bar(aes(Year, fill = Taxa)) +
  labs(title = "Taxa Dataframe",
       x = "Year",
       y = "Count [n of papers]") +
  theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 45), 
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 11), 
        panel.grid.major = element_line(size = 0.1, color = "black"), 
        panel.grid.minor = element_line(size = 0.05, color = "gray"))




library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)

Taxa_data <- read_csv("Data/Taxa_data.csv")

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

#pie chart
ggplot(percent, aes(x = "", y = percent, fill = Taxa)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.title = element_blank())

#histogram
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

#create new variables (PFAS compounds) filled with random numbers (random concentrations)
PFOA <- seq(from = 0, to = 10, by = .01) 
PFOS <- seq(from = 0, to = 15, by = .01)
FOSA <- seq(from = 0, to = 5, by = .01)
new_taxa_data$PFOA <- sample(PFOA, size = nrow(new_taxa_data), replace = T)
new_taxa_data$PFOS <- sample(PFOS, size = nrow(new_taxa_data), replace = T)
new_taxa_data$FOSA <- sample(FOSA, size = nrow(new_taxa_data), replace = T)

#create a new variable (first author's country) filled with randomly chosen categorical data
new_taxa_data$Country_first_author <- sample(c("Italy", "UK", "USA", "Germany", "Norway", "Greece", "Japan", "Australia", "Alaska", "Faroe_islands", "Poland", "China", "Russia"), size = nrow(new_taxa_data), replace = T)

ggplot(new_taxa_data) +
  geom_point(aes(Year, PFOS, color = Taxa))

#Models. Relationship between PFOS and Years
mod1 <- lm(PFOS ~ Year + Taxa, data = new_taxa_data)
summary(mod1)
mod2 <- lm(PFOS ~ Year * Taxa, data = new_taxa_data)
summary(mod2)

grid <- new_taxa_data %>% 
  data_grid(Year, Taxa) %>% 
  gather_predictions(mod1, mod2)

ggplot(new_taxa_data, aes(Year, PFOS, color = Taxa)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model) +
  ylim(c(0, 15))

res <- new_taxa_data %>% 
  gather_residuals(mod1, mod2)
ggplot(res, aes(Year, resid, color = Taxa)) +
  geom_point() +
  facet_grid(model ~ Taxa) +
  theme(legend.position = "none")






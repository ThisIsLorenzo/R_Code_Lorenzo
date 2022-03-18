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

#create new variables (PFAS compounds)
PFOA <- abs(rnorm(403, mean = 7, sd = 10)) 
PFOS <- abs(rnorm(403, mean = 2, sd = .3))
FOSA <- abs(rnorm(403, mean = 1.5, sd = 4))

new_taxa_data$PFOA <- PFOA
new_taxa_data$PFOS <- PFOS
new_taxa_data$FOSA <- FOSA

hist(PFOA, breaks = 100)
hist(PFOS, breaks = 100)
hist(FOSA, breaks = 100)

#create a new variable (first author's country) filled with randomly chosen categorical data. THIS IS A FIGMENT OF THE IMAGINATION!!
new_taxa_data$Country_first_author <- sample(c("Italy", "UK", "USA", "Germany", "Norway", "Greece", "Japan", "Australia", "Alaska", "Faroe_islands", "Poland", "China", "Russia"), size = nrow(new_taxa_data), replace = T)

ggplot(new_taxa_data) +
  geom_point(aes(Year, PFOA, color = Taxa))

#Models. Relationship between PFOS and Years
mod1 <- lm(PFOA ~ Year + Taxa, data = new_taxa_data)
summary(mod1)
mod2 <- lm(PFOA ~ Year * Taxa, data = new_taxa_data)
summary(mod2)

library(modelr)

grid <- new_taxa_data %>% 
  data_grid(Year, Taxa) %>% 
  gather_predictions(mod1, mod2)

ggplot(new_taxa_data, aes(Year, PFOA, color = Taxa)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model) +
  ylim(c(1, 3))

res <- new_taxa_data %>% 
  gather_residuals(mod1, mod2)

ggplot(res, aes(Year, resid, color = Taxa)) +
  geom_point() +
  facet_grid(model ~ Taxa) +
  theme(legend.position = "none")

ggplot(res, aes(Year, resid, color = Taxa)) +
  geom_point() +
  facet_grid(model ~ Taxa) +
  theme(legend.position = "none")

# how many papers are on birds?

n_taxa <- new_taxa_data %>% 
  group_by(Taxa) %>% 
  tally()

n_taxa

ggplot(new_taxa_data) +
  geom_bar(aes(x = factor(Taxa)), fill = "coral") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(name = "n of papers") +
  scale_x_discrete(name = "taxa")

# how many papers are on eggs?

development_stage <- read_csv("Data/development_stage.csv")

development_stage$Developmental_stage = tolower(development_stage$Developmental_stage)

ds <- development_stage %>% 
  group_by(Developmental_stage) %>% 
  tally()

ds

ggplot(development_stage) +
  geom_bar(aes(x = Developmental_stage), fill = "coral") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(name = "n of papers") +
  scale_x_discrete(name = "development stage")




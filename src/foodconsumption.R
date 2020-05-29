library(tidyverse)
library(magrittr)
library(plotly)

foodconsumptionraw <- read_csv("data/2020/2020-02-18/food_consumption.csv")
countrycontinentraw <- read_csv("src/Countries-Continents.csv")

plot <- 
foodconsumptionraw %>% 
  left_join(countrycontinentraw, by = c("country" = "Country")) %>% 
  rename(continent = Continent) %>% 
  group_by(country, continent) %>% 
  summarise(
    totalconsumption = sum(consumption),
    totalemission = sum(co2_emmission)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(totalconsumption, totalemission, text = country)) +
  geom_point(
    aes(colour = continent),
    size = 3
  ) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Food Consumption vs Resultant CO2 Emissions Per Country",
    x = "Annual Food Consumption (kg/person)",
    y = "Resultant Annual CO2 Emissions (kg/person)"
  )

ggplotly(plot)

#Package installations 
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gapminder")

#Package libraries
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(gapminder)

# Importing of Files
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#Joining both files
data_join <- merge(unicef_indicator_2, unicef_metadata, by = c( "country" = "country"))

data_join_2 <- full_join(unicef_indicator_2, unicef_metadata, by = c("country" = "country"))


# Maps installation
install.packages("maps")
map_world <- map_data("world")

# World Map Visualisation - World Map 1 2005
data_join_2005 <- unicef_indicator_2 %>%
  filter(time_period == 2005)
map_data_join_2005 <- full_join(data_join_2005, map_world, by = c("country" = "region"))

ggplot(map_data_join_2005) +
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Coverage of Essential Health Services in 2005 (Units)") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11), 
    axis.text.x = element_blank())


# Time series Visualisation

selected_countries <- c("Portugal", "Malta", "Ireland", "Spain", "Slovenia", "Greece", "Romania", "Hungary", "Cyprus", "Latvia")
filtered_data_1 <- unicef_indicator_2 %>% 
  filter(country %in% selected_countries)


time_series_plot_1 <- filtered_data_1 %>% 
  ggplot() +
  aes(x = time_period, y = obs_value, color = country) +
  geom_line() +
  labs(
    x = "Year",
    y = "Coverage (Units)",
    title = "Coverage of Essential Health Services (2000-2021)") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplotly(time_series_plot_1)

 


#Bar Charts 

selected_countries <- c("Portugal", "Malta", "Ireland", "Spain", "Slovenia", "Greece", "Romania", "Hungary", "Cyprus", "Latvia")
filtered_data_2 <- data_join_2 %>%
  filter(country %in% selected_countries)

years_to_include <- c(2010)
filtered_data_2 <- filtered_data_2 %>%
  filter(time_period %in% years_to_include)

barchart_1 <- filtered_data_2 %>%
  group_by(country, time_period) %>%
  summarize(m_lifeexp = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = country, y = m_lifeexp, fill = country) +  
  geom_bar(stat = "identity") +
  facet_wrap(~ time_period) +
  labs(
    x = "Country",
    y = "Mean Life Expectancy (Years)",
    title = "An analysis of the mean life expectancy of each country in 2005") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11), 
    axis.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(barchart_1)

#Scatterplot Trial 

# Filter the data for the year 2021
filtered_data_2021 <- data_join %>%
  filter(year == 2021 & !is.na(obs_value) & !is.na(`Life expectancy at birth, total (years)`))

scatter_plot_2 <- ggplot(filtered_data_2021) +
  aes(x = obs_value, y = `Life expectancy at birth, total (years)`, color = country) +
  geom_point(alpha = 2, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 0.8) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(25, 75), 
    labels = scales::unit_format(unit = "Units", scale = 1)
  ) +
  scale_size_continuous(guide = FALSE) +
  labs(x = "Coverage of Essential Health Services (Units)",
       y = "Life expectancy at birth", 
       title = "An analysis of the relationship between life expectancy and the coverage of EHS in 2021") +
  guides(color = "none", size = "none") +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 11))

ggplotly(scatter_plot_2)



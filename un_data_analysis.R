library(tidyverse)

setwd("C:/Users/fishc/Desktop/un-report/data")
getwd()

gapminder_data <- read_csv("gapminder_data.csv")

# summarise() - summarise our data frame

summarise(gapminder_data, averageLIfeExp=mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>%
  summarise(averageLifeExp=mean(lifeExp))
gapminder_data_summarized

gapminder_data %>%
  summarise(recent_year=max(year))

# filter() - subsets the rows in a dataframe

gapminder_data %>%
  filter(year==2007)

gapminder_data %>% filter(year==2007) %>%
  summarise(averageLifeExp=mean(lifeExp))

# What is the average GDP per capita for the first year
# 1. Find what is the first year? min()

# 2. find the average gdpPercap for that year

gapminder_data %>%
  summarise(firstyear=min(year)) 

gapminder_data %>%
  filter(year==1952)%>%
  summarise(average_gdp=mean(gdpPercap))

## group_by() - group values from a column

gapminder_data %>%
  group_by(year) %>%
  summarise(average_lifeExp=mean(lifeExp))

## calculate the average life expectancy by continent

gapminder_data %>%
  group_by(continent) %>%
  summarise(average_lifeExp=mean(lifeExp))
##

gapminder_data %>%
  group_by(continent) %>%
             summarise(average_lifeexp=mean(lifeExp), min_lifeexp=min(lifeExp))

# mutate() - add or change a variable/column in a dataframe

gapminder_data %>%
  mutate(gdp = pop * gdpPercap)

# make a new column called popInMillions that is the population in millions

gapminder_data %>%
  mutate(popInMillions = pop/1000000)

## if we want to keep any of these changes, we would need to create an assignment
## to override gapminder

gapminder_data_with_mutation <
gapminder_data %>%
  mutate(popInMillions = pop/1000000)



## select() - subsets columns in a dataframe

gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(-continent)

# Create a dataframe wit only country, continent, year and lifeExp


gapminder_data %>%
  select(country, continent, year, lifeExp)

## pivot_wider() makes it wider; pivot_longer() takes something that is wide and makes it long

gapminder_data %>%
  select(country, continent, year, lifeExp) %>% ## the order of country and continent can be changed if they are swaped with each other
  pivot_wider(names_from = year, values_from = lifeExp) ## take one of the columns and use it for rows,value populated by is lifeExp


# subsetting the gapminder_data to the year 2007 and drop the year and continent columns

gapminder_data %>%
  filter(year==2007) %>%
  select(-year,-continent)

## data that is only from 2007 and only from the americas continent

gapminder_data_2007 <-
    gapminder_data %>%
    filter(year==2007 & continent=="Americas") %>%
    select(-year, -continent)
    

## is CO2 emissions related to GDP?
## skip=1 means skip 1 

co2_emissions_dirty <-

  read_csv("co2-un-data.csv",skip=2,
                col_names=c("region","country",
                          "year","series","value",
                          "footnotes","source"))
co2_emmisions <-

co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%

  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)


view(co2_emmisions)

view(gapminder_data_2007)

## inter-joint 2 datasets by column present in both datasets
# left joint I'm going to keep everything that in the left dataframe, and when I join them, but if there is something in the right that is not in the left, I will drop it.
## right joint

# inner_join()

inner_join(gapminder_data_2007, co2_emmisions)

## anti_join()

anti_join(gapminder_data_2007, co2_emmisions) # what elements are present in the left, but not in the right


co2_emmisions <-
  
  co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)"="Bolivia",
                          "United States of America"="United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))


view(co2_emmisions)

anti_join(gapminder_data_2007, co2_emmisions)

## the anti_join shows that Puerto Rico is not going to be added because it's not a state, 
## so we want to make changes to include that as part of the US.

gapminder_data_2007 <-
  gapminder_data %>%
  filter(year==2007 & continent=="Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico"="United States"))
view(gapminder_data_2007)

gapminder_data_2007 <-
  gapminder_data %>%
  filter(year==2007 & continent=="Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico"="United States")) %>%
  group_by(country) %>%
  summarise(lifeExp = sum(lifeExp * pop)/ sum(pop),
            gdpPercap= sum(gdpPercap * pop)/ sum(pop),
            pop = sum(pop))
View(gapminder_data_2007)

## our goal is to keep everything from gapminder_data_2007, and only some things from co2_emissions. this is why we do anti_join in this order, gapminder_data_2007 first
## anti_join shows us what is not going to join from co2_emissions

anti_join(gapminder_data_2007, co2_emmisions)

gapminder_co2 <-
  inner_join(gapminder_data_2007, co2_emmisions)

view(gapminder_co2)

# is cos emissions related to GDP?

ggplot(gapminder_co2, aes(x = gdpPercap, y= per_capita_emissions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="GDP(per capita", y="CO2 emitted per capita")

## we'll do this, so we don't have to run everything again

write_csv(gapminder_co2, "gapminder_Co2.csv")

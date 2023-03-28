library(tidyverse)


## to create data object ##

gapminder_1997 <- read_csv("gapminder_1997.csv")


view(gapminder_1997)

str(gapminder_1997)

?read_csv

Sys.Date()

sum(5,6)

round(3.1415)

round(3.1415,3)
round(x=3.1415, digits = 2)

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y="Life Expectancy") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) + ## scale size to population ## 
  labs(size = "Population (in millions)")


ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita",y="Life Expectancy",title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)")

## Load in a larger dataset

gapminder_data <- read_csv("gapminder_data.csv")


dim(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, group = country,color = continent) +
  geom_line()

## Discrete plots to look at the spread of our data##

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_boxplot()

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin() +
  geom_point()

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_jitter() +
  geom_violin() 

## Master aesthetics
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin() +
  geom_jitter(aes(size=pop), color="pink") # inside or outside of the aes function##

### if we want to color in relation to a variable, we need to add color to aes instead##

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(fill = "pink")   # inside or outside of the aes function##


## now we are mapping color to our data and we have to include it in aes ##

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = continent))   # inside or outside of the aes function##


ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = "springgreen"))   # inside or outside of the aes function##


ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(fill = "springgreen")   # inside or outside of the aes function##



## Univariate plots

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram() ## bin default is 30#


ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins=10, binwidth = 0.5)

## Plot themes

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic()

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_minimal()

## how to adjust elements of a theme

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic() + ## to override these##
  theme(axis.text.x = 
          element_text(angle = 90,
          vjust = 0.5,
          hjust = 1)) ##vjust vertical adjustment#


# facet wrap
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent)) # we are separating data by continent, vars is the variable function#

# facet grid: assumes what?? ##

awesome_plot <-

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

### # To save the plot. it's assuming it needs to save the last plot we did
 
ggsave("awesome_plot.jpg", width = 6, height = 4)

## Now we are specifying the name of the plot

ggsave(awesome_plot,
       file = "awesome_plot.jpg",
       width = 6, height = 4)

## Exercise:
## -violin plot of continent and life expectancy, 
## -color mapped to continent
# Assigned to an object "violin plot"
# Black and white theme
#save it as "awesome_violin_plot.jpg"

violin_plot <-  

ggplot(gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(color = continent)) +
  theme_bw()
  
violin_plot   ## when we assign something to an object, we have to call it to see it##
 
ggsave(violin_plot,
         file = "awesome_violin_plot.jpg",
         width = 6, height = 4)
  
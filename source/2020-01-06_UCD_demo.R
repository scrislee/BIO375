library("tidyverse")
library("gapminder")
library("magrittr")

head(gapminder)
head(gapminder,3)
str(gapminder)

y <- 9
# 9 <- y
y <= 15

# y == 5
# y != 10

filter(gapminder, lifeExp <29)
# filter(gapminder, year = 1967)
filter(gapminder, year == 1967)

# filter(gapminder, "country" == Mexico)
# filter(gapminder, country == Mexico)
# filter(gapminder, "country" == "Mexico")
filter(gapminder, country == "Mexico")


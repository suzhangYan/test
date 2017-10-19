download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv", destfile = "data/gapminder-FiveYearData.csv")
gapminder <- read.csv("data/gapminder-FiveYearData.csv")
table(gapminder$lifeExp)
table(gapminder$year)
summary(gapminder$year == 2007)
summary(gapminder[gapminder$year == 2007,])


min(gapminder$year)
mean(gapminder[gapminder$year == 1952, 5])

attach(gapminder)
names(gapminder)


library(tidyverse)
ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp)) + 
  geom_point()
ggplot(data=gapminder, aes(x=year, y=lifeExp)) + 
  geom_point()
table(year)


ggplot(data=gapminder, aes(x=year, y=lifeExp, by=country, color=continent)) + 
  geom_point() + 
  geom_line()


ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp, color=continent)) + 
  geom_point() +
  scale_x_log10() +
  geom_smooth(method="lm") +
  facet_wrap( ~ continent)



install.packages("plotly")
library("plotly")


p <- ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp, color=continent)) + 
  geom_point() +
  scale_x_log10() +
  geom_smooth(method="lm") +
  facet_wrap( ~ continent)

ggplotly(p)

install_github('hadley/ggplot2')
devtools::install_github('hadley/ggplot2')


if(!require("plotly")) {install.packages("plotly")}

# making nice labels
p + labs(x="GDP Per Capita", y="Life Expectacy in Years", title="Figure 1") +
  scale_color_discrete(name="Continent")



#### make own functions: 
se <- function(x) {
  sd(x)/sqrt(length(x))
}

cars <- c(3,4,5,6,7,10)

se(cars)



##### can save above function as a separate R script and read in the R script file
source("functions.R")
se(cars)


### use "dplyr"
install.packages(dplyr)

# explore "select"
# select certain columns
year_country_gdp <- select(gapminder,year,country,gdpPercap)

# get rid of certain columns
year_country_gdp <- select(gapminder,-pop,-country,-lifeExp)


# explore "filter"
year_country_gdp_euro <- gapminder %>% 
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)

# %>%, pipe operator means that whaterver comes before it will be the first argument in the
# function that follows it; this is the default; if not, can use "." in the 
# second or third spot: select(data, ., country)
# press control, shift and m at the same time to get "%>%"

#### most important R functions, according to Remi, are "grouup_by" and 
# "summarize" functions
# "summarize" can compute more than one statistic
# select is for columns
# filter is for rows

mean_gdp_percountry <- gapminder %>% 
  group_by(country) %>% 
  summarize(mean_gdp=mean(gdpPercap), 
            se_gdp=se(gdpPercap))

mean_gdp_percountry


# "n" doesn't take any argument; it just tells # of observations/rows in the 
# current group (subsetted data by "group_by")
# can only be used from within summarise(), mutate() and filter().
mean_gdp_cont <- gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_lifeExp=mean(lifeExp), 
            se_gdp=se(lifeExp),
            size=n())

mean_gdp_cont  


mean_gdp_contcoun <- gapminder %>% 
  group_by(continent, country) %>% 
  summarize(mean_lifeExp=mean(lifeExp), 
            se_gdp=se(lifeExp),
            size=n())

mean_gdp_contcoun


View # need to capitalize letter "V"


# "starts_with" only works with "select", but not "filter", because this function
# selects variables or columns that start with letter "C"
twocolumns_C <- gapminder %>% 
  select(starts_with("C"))



euco_countries <- gapminder %>% 
  filter(continent=="Europe") %>% 
  ggplot(aes(x=year, y=lifeExp, color=country)) + 
  geom_line() +
  facet_wrap(~country)

euco_countries
  
ggsave("euro.png")



##### package "tidyr"
library(tidyr)
download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/data/gapminder_wide.csv", destfile = "data/gapminder_wide.csv")


gapminder_wide <- read.csv("data/gapminder_wide.csv")


gap_long <- gapminder_wide %>% 
  gather(obstype_year, obs_values, 
         starts_with('pop'), 
         starts_with('lifeExp'), 
         starts_with('gdpPercap'))  #single quotation and double quotation are both ok.


head(gap_long)


gap_long <- gapminder_wide %>% 
  gather(obstype_year, obs_values, 
         3:38) # 3:38 is equivalent to the codes above


# separate the obs_type column
gap_normal <- gap_long %>% 
  separate(obstype_year, into=c("obs_type", "year"), sep="_") %>% 
  spread(obs_type, obs_values)

head(gap_normal)

gap_normal <- gap_normal %>% 
  arrange(country, continent, year)

all.equal(gapminder, gap_normal)


names(gapminder)
names(gap_normal)

# test

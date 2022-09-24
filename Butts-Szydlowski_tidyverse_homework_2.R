# dplyr/tidyverse homework
library(tidyverse)

# Outline
# 1. filter
# 2. mutate
# 3. summarize
# 4. group_by
# 5. pipes
# 6. arrange/select/count
# 7. joins


# 1. filter
# filter iris data by each species 
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

# 2. mutate
# create 2 new columns in iris data that is petal/sepal length x width
iris_area <- mutate(
  iris, 
  petal.area = Petal.Length * Petal.Width,
  sepal.area = Sepal.Length * Sepal.Width
)

# plot petal area ~ length - is the relationship linear? Why?
ggplot(data = iris_area, aes(x = Petal.Length, y = petal.area)) + geom_point()

#It is not linear, likely because a petal is not shaped like a rectangle so there is not a linear relationship
# between petal length and area. It is allometric

# 3. summarize
# compute the mean petal length of each species dataset from above
setosa_mean <- mean(setosa$Petal.Length) #1.462
versicolor_mean <- mean(versicolor$Petal.Length) #4.26
virginica_mean <- mean(virginica$Petal.Length) #5.552

# now do it using summarize
setosa_summarize_mean <- summarize(setosa, mean.petal.length = mean(Petal.Length))
versicolor_summarize_mean <- summarize(versicolor, mean.petal.length = mean(Petal.Length))
virginica_summarize_mean <- summarize(virginica, mean.petal.length = mean(Petal.Length))

# 4. group by
# we can do the above summarize so much easier when combined with group_by
iris_means <- summarize(group_by(iris, Species), mn.petal.length = mean(Petal.Length))

# 5. pipes
# the above can get unwieldy - rearrange iris_means from 4 using pipes

# just arrange so it is sorted by mean, weren't sure what else needs to be rearranged
iris_means = iris_means %>% arrange(by = mn.petal.length)


## On Your Own #1 
# now compute mean petal area for each species - how would you go about it using dplyr

iris_mean_area <- iris %>% group_by(Species) %>% 
  summarise(mn.petal.area = mean(Petal.Length * Petal.Width)) %>% 
  ungroup()

# Q: What is the mean petal area for each species

#-------------------------------Question 1 answer ----------------------------------
# Setosa = 0.366
# versicolor = 5.72
# virginica = 11.3
#------------------------------------------------------------------------------------

# 6. arrange/select/count
# determine which species has the longest mean petal length
iris_size <- 
  iris %>% 
  select(Species, Petal.Length) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.petal.length = mean(Petal.Length)) %>%
  arrange(desc(mn.petal.length))

# virginica has the longest mean petal length of 5.552

# On Your Own #2
# do the same for the other measurements (i.e. petal.width, sepal.length, sepal.width, etc)
iris_size <- 
  iris %>% 
  select(Species, Petal.Width) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.petal.width = mean(Petal.Width)) %>%
  arrange(desc(mn.petal.width))

# virginica has the longest mean petal width of 2.026

iris_size <- 
  iris %>% 
  select(Species, Sepal.Length) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.sepal.length = mean(Sepal.Length)) %>%
  arrange(desc(mn.sepal.length))

# virginica also has the longest mean sepal length of 6.588

iris_size <- 
  iris %>% 
  select(Species, Sepal.Width) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.sepal.width = mean(Sepal.Width)) %>%
  arrange(desc(mn.sepal.width))

# setosa has the longest mean sepal width of 3.428

# Q: What is the mean petal and sepal lengths and widths for each species

# ------------------------- Question 2 answer ---------------------------------
# petal length:
# virginica             5.55
# versicolor            4.26
# setosa                1.46

# petal width:
# virginica           2.03 
# versicolor          1.33 
# setosa              0.246

# sepal length: 
# virginica             6.59
# versicolor            5.94
# setosa                5.01

# sepal width:
# setosa               3.43
# virginica            2.97
# versicolor           2.77

#-------------------------------------------------------------------------------

# count the number of records for each species
(iris_spp_n <- count(iris, Species))

# On Your Own #3
# count the number of samples that are >= mean.petal.length for each species
iris_spp_n <- iris %>% 
  group_by(Species) %>% 
  mutate(mean = mean(Petal.Length)) %>% 
  mutate(exceed_mean = Petal.Length > mean)

iris_spp_exceed_mean = iris_spp_n %>% group_by(Species) %>% 
  summarize(sum(exceed_mean))


# Q: How many samples where Petal.Length >= mean.petal.length does each species have

#------------------------ Question 3 answer ------------------------------------

# 1 setosa                     26
# 2 versicolor                 27
# 3 virginica                  25

#-------------------------------------------------------------------------------

# 7. joins
set.seed(123)
ht <- data.frame(level = LETTERS[1:5], height = sample(40:80, 5, replace = TRUE))
wt <- data.frame(level = LETTERS[1:6], weight = sample(50:500, 6, replace = TRUE))

# join together height and weight by level
# what happens when you reverse ht and wt (i.e. put wt first, then ht)
ht_wt <- left_join(ht, wt, by = "level")

ht_wt <- left_join(wt, ht, by = "level")

# when ht and wt are reversed, the columns are swapped and there is an extra observation
# for wt which was previously dropped because height was missing





# On Your Own #4 - Extra Credit
# work with the nycflights13 data set to determine what airport had the 
#     most departing flights in 2013
# must use combination of dplyr verbs
# data.frames are airports, flights, planes and weather
# HINT: faa column in airports links to origin column in flights
library(nycflights13)
airports
flights = flights 

nyc_departing = flights %>%
  select(year, dep_time, flight, origin) %>% # Use departure time to get unique values 
  filter(year == 2013) %>%
  rename(faa = origin)
airport = airports %>%
  select(faa, name)

departing_flights = left_join(nyc_departing, airport, by = 'faa') %>% arrange(name) 
departing_flights

final = departing_flights %>%
  group_by(name) %>%
  summarize(count = n())
final

#=================================ANSWER==============================# 
## Newark Liberty International had the greatest amount of departing flights in 2013 ## 
#=====================================================================# 

# Q: Which airport (name) had the greatest number of arriving flights in 2013?
destinations = flights %>%
  select(year, dest, flight) %>% # Use departure time to get unique values 
  filter(year == 2013) %>%
  rename(faa = dest)
airport = airports %>%
  select(faa, name)

arriving = left_join(destinations, airport, by = 'faa') %>% arrange(name) 
arriving


final2 = arriving %>%
  group_by(name) %>%
  summarize(count = n()) %>% 
  arrange(-count)
final2

#=================================ANSWER==============================# 
## Chicago O'Hare International had the greatest amount of arriving flights from NYC airports in 2013 ## 
#=====================================================================# 

# Q: Which airport (name) had the greatest number of delayed arriving flights?
flights

arr_delay = flights %>%
  filter(arr_delay > 0) %>%
  select(year, arr_delay, dest) %>%
  filter(year == 2013) %>%
  rename(faa = dest)

airport = airports %>%
  select(faa, name)

delayed = left_join(arr_delay, airport, by = 'faa') %>% arrange(name) 
delayed 

final3 = delayed %>%
  group_by(name) %>%
  summarize(count = n()) %>% 
  arrange(-count)
final3

##====================================ANSWER===============================## 
## Hartsfield Jackson Atlanta International had the greatest number of delayed arriving flights ## 
##=========================================================================##

# Q: What is the manufacturer, model, year, and type of airplane that flew the 
#    most flights in 2013 (only include planes with all relevant information)?
#    How man flights was it?

plane_join = left_join(flights, planes, by = c('tailnum', 'year'))
plane_join

dat_plane = plane_join %>%
  select(year, flight, tailnum, manufacturer, model, type) %>%
  filter(year == 2013) %>%
  drop_na() %>% 
  arrange(manufacturer)
dat_plane


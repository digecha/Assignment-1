# Part 1:Set up -----------------------------------------------------------
# Load the data from https://countlove.org/data/data.csv
# into a variable called `protests`
install.packages("tidyverse")
library(tidyverse)
protest <- read_csv("https://countlove.org/data/data.csv")
install.packages("dplyr")
library(dplyr)

# How many protests are in the dataset? `num_protests`
num_protests <- count(protests)
num_protests

# How much information is available about each protest? `num_features`
num_features <- ncol(protests)
num_features

# Part 2: Attendees -------------------------------------------------------
# Extract the `Attendees` column into a variable called `num_attendees`
num_attendees <- protests[,3]
num_attendees

# What is the lowest number of attendees? `min_attendees`
# (hint for this and other calculations: you'll need to consider missing values)
min_attendees <- min(num_attendees, na.rm = TRUE)
min_attendees

# What is the highest number of attendees? `max_attendees`
max_attendees <- max(num_attendees, na.rm = TRUE)
max_attendees

# What is the mean number of attendees? `mean_attendees`
mean_attendees <- mean(protests$Attendees, na.rm = TRUE)
mean_attendees


# What is the median number of attendees? `median_attendees`
median_attendees <- median(protests$Attendees, na.rm = TRUE)
median_attendees 

# What is the difference between the mean and median number of attendees?
# `mean_median_diff`
mean_median_diff <- mean_attendees - median_attendees
mean_median_diff

# To further assess the distribution of values, create a boxplot of the number
# of attendees using the `boxplot()` function.
# Store the plot in a variable called `attendees_distribution`
# (Note, we'll use much more refined plotting methods, and pay far
# more attention to detail later in the course)
attendees_distribution <- boxplot(num_attendees, na.rm = TRUE)

# Create another boxplot of the *log* of the number of attendees.
# Store the plot in a variable `log_attendees_distribution`.
# (note, you will see a warning in the console, which is expected)
log_attendees_distribution <- boxplot(log10(num_attendees), na.rm = TRUE)


# Part 3: Locations -------------------------------------------------------
# Extract the `Location` column into a variable called `locations`
locations <- protests[,2]
locations

# How many *unique* locations are in the dataset? `num_locations`
num_locations <- nrow(unique(locations))
num_locations

# How many protests occurred in Washington? `num_in_wa`
# (hint: use a function from the stringr package to detect the letters "WA")
num_in_wa <- nrow(protests[str_detect(protests$Location,"WA"), "Location"])
num_in_wa

# What proportion of protests occurred in Washington? `prop_in_wa`
prop_in_wa <- num_in_wa / nrow(locations)
prop_in_wa 

# Write a function `count_in_location()` that accepts (as a parameter)
# a `location` name, and returns the sentence (note: spacing and punctuation):
# "There were N protests in LOCATION.", where N is the number of
# protests that occurred in that location, and LOCATION is the parameter that
# was provided into the function.
# Note, you should count the number of locations that *match* the parameter
# put into the function, so `Seattle` should be a match for "Seattle, WA"
count_in_location <- function(location_choice) {
  t <- protests[str_detect(protests$Location,location_choice), "Location"]
  n <- nrow(t)
  print(paste("There were", n, "protests in", location_choice))
}
count_in_location("WA")

# Use your function above to describe the number of protests in "Washington, DC"
# `dc_summary`
dc_summary <- count_in_location("Washington, DC")

# Use your function above to describe the number of protests in "Minneapolis"
# `minneapolis_summary`
minneapolis_summary <- count_in_location("Minneapolis")

# Create a new vector `states` which is the last two characters of each
# value in the `locations` vector. Hint, you may want to again use the
# `stringr` package
states <- transmute(locations, state = str_sub(locations$Location, -2))
states

# Create a vector of the unique states in your dataset. `uniq_states`
uniq_states <- unique(str_upper(states))
uniq_states

# Create a summary sentence for each state by passing your `uniq_states`
# variable and `count_in_location` variables to the `sapply()` function.
# Store your results in `state_summary`
# (don't miss how amazing this is! Very powerful to apply your function to an
# entire vector *at once* with `sapply()`)
state_summary <- sapply(uniq_states, count_in_location)
state_summary

# Create a summary table by passing your `states` variable to the `table()`
# function, and storing the result in a variable `state_table`.
state_table <- table(state_summary)
state_table

# Optional: use the View() function to more easily read the table
View(state_table)

# What was the maximum number of protests in a state? `max_in_state`
# (hint: use your `state_table` variable)
max_in_state <- max(uniq_states$state_table)
max_in_state

# Part 4: Dates -----------------------------------------------------------
# Extract the `Date` column into a variable called `dates` by passing the
# column to the `as.Date()` function (this will process the values as dates,
# which are *luckily* already in an optimal format for parsing)
dates <- as.Date(protests$Date)
dates

# What is the most recent date in the dataset? `most_recent`
most_recent <- max(dates)
most_recent

# What is the earliest date in the dataset? `earliest`
earliest <- min(dates)
earliest

# What is the length of the timespan of the dataset? `time_span`
# hint: R can do math with dates pretty well by default!
time_span <- most_recent - earliest
time_span

# Create a vector of the dates that are in 2020 `in_2020`
install.packages("lubridate")
library(lubridate)
in_2020 <- protests[year(protests$Date) == "2020", "Date"]
in_2020

# Create a vector of the dates that are in 2019. `in_2019`
in_2019 <-  protests[year(protests$Date) == "2019", "Date"]
in_2019

# What is the ratio of the number of protests in 2020 compared to 2019?
# `ratio_2020_2019`
ratio_2020_2019 <- nrow(in_2020)/nrow(in_2019)
ratio_2020_2019

# Write a function `count_on_date()` that accepts as a parameter a `date`,
# and returns the sentence:
# "There were N protests on DATE.", where N is the number of protests on that
# date, and DATE is the date provided
count_on_date <- function(date_choice) {
  a <- protests[protests$Date == date_choice, "Date"]
  b <- nrow(a)
  print(paste("There were", b, "protests on", date_choice))
}
count_on_date("2021-01-30")

# Using your function you just wrote, how many protests were there on
# May 24th, 2020? `num_may_24`
num_may_24 <- count_on_date("2020-05-24")

# Using your function you just wrote, how many protests were there on
# May 31th, 2020? `num_on_may_31`
num_on_may_31 <- count_on_date("2020-05-31")

# How many protests occurred each month in 2020? `by_month_table`
# Hint: use the `months()` function, your `in_2020` dates, and the `table()`
# Function. If you like, you can do this in multiple different steps.
by_month_table <- data.frame(pd = in_2020$Date)
by_month_table[!(is.na(by_month_table$pd)), ]
by_month_table <- transmute(by_month_table, md = month(by_month_table$pd))
by_month_table <- table(by_month_table)
by_month_table

# As a comparison, let's assess the change between July 2019 and July 2020.
# What is the *difference* in the number of protests between July 2020 and
# July 2019? You'll want to do this in multiple steps as you see fit, though
# your answer should be stored in the variable `change_july_protests`.
how_many <- function(y, m){
  h <- protests[year(protests$Date) == y & month(protest$Date) == m, "Date"]
  b <- nrow(h)
  b
}
how_many(2020, 1)
change_july_protests <- how_many(2019, 7) - how_many(2020, 7)
change_july_protests

# Part 5: Protest Purpose -------------------------------------------------

# In this section, you're exploring *why* protests happened
# Extract the `Event..legacy..see.tags.` column into a variable called `purpose`
purpose <- data.frame(event=protests$`Event (legacy; see tags)`)
purpose

# How many different purposes are listed in the dataset? `num_purposes`
num_purpose <- unique(purpose)
num_purpose

# That's quite a few -- if you look at -- View() -- the vector, you'll notice
# a common pattern for each purpose. It's listed as:
# SOME_PURPOSE (additional_detail)
# To get a higher level summary, create a variable `high_level_purpose` by
# extracting *everything before the first parenthesis* in each value
# in the vector. For example, from "Civil Rights (Black Women's March)"
# you would extract "Civil Rights". You'll also have to *remove the space*
# before the first parenthesis.
# Hint: this will take a little bit of Googling // trial and error. Be patient!
high_level_purpose <- transmute(purpose, 
    higher_purpose = gsub(" \\(.*", "", purpose$event))
high_level_purpose

# How many "high level" purposes have you identified? `num_high_level`
num_high_level <- nrow(high_level_purpose)
num_high_level
  
# Create a table that counts the number of protests for each high level purpose
# `high_level_table`
high_level_table <- table(high_level_purpose)
high_level_table

# Part 6: Independent Exploration -----------------------------------------

# As a last step, you should write your own function that allows you to
# quickly ask questions of the dataset. For example, in the above sections,
# you wrote functions to ask the same question about different months, or
# locations. If you need any guidance here, feel free to ask!

user.location <- readline(prompt = "Enter desired location: ")
count_in_location(user.location)

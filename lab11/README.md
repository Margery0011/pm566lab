---
title: "Lab 11 - Interactive Visualization"
date: "11/09/2022"
output: html_document
link-citations: yes
editor_options: 
  chunk_output_type: console
always_allow_html: true
---

HTML Report is **[** click here**] (https://rawcdn.githack.com/Margery0011/pm566lab/3cfe0a75ed3ac505ac7933def45cba9fd844ab4b/lab11/11-lab.html)**

```sh
# Step 1
cd ~/Documents
mkdir lab11
cd lab11

# Step 2
wget https://raw.githubusercontent.com/USCbiostats/PM566/master/website/content/assignment/11-lab.Rmd

```

**And remember to set `eval=TRUE`**

```{r setup, message=FALSE, echo=FALSE, warning=FALSE}
library(data.table)
library(tidyverse)
library(dplyr)
library(plotly)
library(knitr)
library(widgetframe)

opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  eval=TRUE,
  echo = TRUE,
  cache=FALSE,
  include=TRUE,
  fig.width = 7, 
  fig.align = 'center',
  fig.asp = 0.618,
  out.width = "700px")


```

# Learning Goals

- Read in and process the COVID dataset from the New York Times GitHub repository
- Create interactive graphs of different types using `plot_ly()` and `ggplotly()` functions
- Customize the hoverinfo and other plot features
- Create a Choropleth map using `plot_geo()`

# Lab Description

We will work with COVID data downloaded from the New York Times. The dataset consists of COVID-19 cases and deaths in each US state during the course of the COVID epidemic.

**The objective of this lab is to explore relationships between cases, deaths, and population sizes of US states, and plot data to demonstrate this**

# Steps

## I. Reading and processing the New York Times (NYT) state-level COVID-19 data

### 1. Read in the data

- Read in the COVID data with data.table:fread() from the NYT GitHub repository: "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
- Read in the state population data with data.table:fread() from the repository: "https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv""
- Merge datasets

```{r, echo=TRUE, message=FALSE}

## data extracted from New York Times state-level data from NYT Github repository
# https://github.com/nytimes/covid-19-data

## state-level population information from us_census_data available on GitHub repository:
# https://github.com/COVID19Tracking/associated-data/tree/master/us_census_data

### FINISH THE CODE HERE ###
# load COVID state-level data from NYT
cv_states <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

### FINISH THE CODE HERE ###
# load state population data
state_pops <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv"))
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL

### FINISH THE CODE HERE
cv_states <- merge(cv_states, state_pops, by="state")
```

### 2. Look at the data

- Inspect the dimensions, `head`, and `tail` of the data
- Inspect the structure of each variables. Are they in the correct format?

```{r}
dim(cv_states)
head(cv_states)
tail(cv_states)
str(cv_states)
```

### 3. Format the data

- Make date into a date variable
- Make state into a factor variable
- Order the data first by state, second by date
- Confirm the variables are now correctly formatted
- Inspect the range values for each variable. What is the date range? The range of cases and deaths?

```{r}
# format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")

# format the state and state abbreviation (abb) variables
state_list <- unique(cv_states$state)
cv_states$state <- factor(cv_states$state, levels = state_list)
abb_list <- unique(cv_states$abb)
cv_states$abb <- factor(cv_states$abb, levels = abb_list)

### FINISH THE CODE HERE 
# order the data first by state, second by date
cv_states = cv_states[order(cv_states$state, cv_states$date),]

# Confirm the variables are now correctly formatted
str(cv_states)
head(cv_states)
tail(cv_states)

# Inspect the range values for each variable. What is the date range? The range of cases and deaths?
head(cv_states)
summary(cv_states)
min(cv_states$date)
max(cv_states$date)
```

### 4. Add `new_cases` and `new_deaths` and correct outliers

- Add variables for new cases, `new_cases`, and new deaths, `new_deaths`: 
  - Hint: You can set `new_cases` equal to the difference between cases on date i and date i-1, starting on date i=2
  
- Filter to dates after June 1, 2022

- Use `plotly` for EDA: See if there are outliers or values that don't make sense for `new_cases` and `new_deaths`. Which states and which dates have strange values?

- Correct outliers: Set negative values for `new_cases` or `new_deaths` to 0

- Recalculate `cases` and `deaths` as cumulative sum of updated `new_cases` and `new_deaths`

- Get the rolling average of new cases and new deaths to smooth over time

- Inspect data again interactively

```{r}
# Add variables for new_cases and new_deaths:
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  cv_subset = cv_subset[order(cv_subset$date),]
  # add starting level for new cases and deaths
  cv_subset$new_cases = cv_subset$cases[1]
  cv_subset$new_deaths = cv_subset$deaths[1]
  #### FINISH THE CODE HERE ###
  for (j in 2:nrow(cv_subset)) {
    cv_subset$new_cases[j] = cv_subset$cases[j] - cv_subset$cases[j-1] 
    cv_subset$new_deaths[j] = cv_subset$deaths[j] - cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$new_cases[cv_states$state==state_list[i]] = cv_subset$new_cases
  cv_states$new_deaths[cv_states$state==state_list[i]] = cv_subset$new_deaths
}
cv_states <- cv_states %>% dplyr::filter(date >= "2022-06-01")

### FINISH THE CODE HERE ###
p1<-ggplot(cv_states, 
           aes( x=date, y=new_cases, color=state  )
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p1)
p1<-NULL # to clear from workspace
### FINISH THE CODE HERE ###
p2<-ggplot(cv_states, 
           aes(x=date, y=new_deaths, color=state )
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
p2<-NULL # to clear from workspace
# set negative new case or death counts to 0
cv_states$new_cases[cv_states$new_cases<0] = 0
cv_states$new_deaths[cv_states$new_deaths<0] = 0
# Recalculate `cases` and `deaths` as cumulative sum of updates `new_cases` and `new_deaths`
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  # add starting level for new cases and deaths
  cv_subset$cases = cv_subset$cases[1]
  cv_subset$deaths = cv_subset$deaths[1]
  for (j in 2:nrow(cv_subset)) {
    cv_subset$cases[j] = cv_subset$new_cases[j] + cv_subset$cases[j-1]
    cv_subset$deaths[j] = cv_subset$new_deaths[j] + cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$cases[cv_states$state==state_list[i]] = cv_subset$cases
  cv_states$deaths[cv_states$state==state_list[i]] = cv_subset$deaths
}
```

```{r}
# Smooth new counts
cv_states$new_cases = zoo::rollmean(cv_states$new_cases, k=7, fill=NA, align='right') %>% round(digits = 0)
cv_states$new_deaths = zoo::rollmean(cv_states$new_deaths, k=7, fill=NA, align='right') %>% round(digits = 0)

# Inspect data again interactively
p2<-ggplot(cv_states, aes(x = date, y = new_deaths, color = state)) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
#p2=NULL
```


```{r}
# Inspect data again interactively
p2<-ggplot(cv_states, aes(x = date, y = new_deaths, color = state)) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
#p2=NULL
```


### 5. Add additional variables

- Add population-normalized (by 100,000) variables for each variable type (rounded to 1 decimal place). Make sure the variables you calculate are in the correct format (`numeric`). You can use the following variable names:
  - `per100k` = cases per 100,000 population
  - `newper100k`= new cases per 100,000
  - `deathsper100k` = deaths per 100,000
  - `newdeathsper100k` = new deaths per 100,000

- Add a "naive CFR" variable representing `deaths / cases` on each date for each state

- Create a dataframe representing values on the most recent date, `cv_states_today`, as done in lecture

```{r}

### FINISH CODE HERE
# add population normalized (by 100,000) counts for each variable
cv_states$per100k =  as.numeric(format(round(cv_states$cases/(cv_states$population/100000),1),nsmall=1))
cv_states$newper100k =  as.numeric(format(round(cv_states$new_cases/(cv_states$population/100000),1),nsmall=1))
cv_states$deathsper100k =  as.numeric(format(round(cv_states$deaths/(cv_states$population/100000),1),nsmall=1))
cv_states$newdeathsper100k =  as.numeric(format(round(cv_states$new_deaths/(cv_states$population/100000),1),nsmall=1))

# add a naive_CFR variable = deaths / cases
cv_states = cv_states %>% mutate(naive_CFR = round((deaths*100/cases),2))

# create a `cv_states_today` variable
cv_states_today = subset(cv_states, date==max(cv_states$date))

```


## CS544 Final Project: Analysis of Boston Property Assessments
## Sylvie Xiang, Dawn Graham

library(plotly)
library(plyr)

## Read in data
pa <- read.csv("https://raw.githubusercontent.com/dawngraham/cs544-boston-properties/main/pa.csv",colClasses=c("ZIPCODE"="character"))

# Remove duplicate rows
pa <- pa[!duplicated(pa),]

# Derive change in values from 2015 to 2021
pa$AMT_CHANGE <- pa$VALUE_2021 - pa$VALUE_2015
pa$PCT_CHANGE <- round(pa$AMT_CHANGE / pa$VALUE_2015 * 100)

summary(pa)

## Analysis
#Do the analysis as in Module3 for at least one categorical variable and at least one numerical variable. Show appropriate plots for your data.

# Categorical variable: YR_BUILT (Sylvie)

# Categorical variable: OWN_OCC (Dawn)
# Map values for OWN_OCC
pa$OWN_OCC <- mapvalues(pa$OWN_OCC,
                        from=c("Y", "N"),
                        to=c("Yes", "No"))

own_occ <- table(pa$OWN_OCC)

prop.table(own_occ)*100

plot_ly(x = names(own_occ),
        y = as.numeric(own_occ),
        type = "bar"
        ) %>%
  layout(title = "Owner Occupied",
         xaxis = list(categoryorder = "category descending"),
         yaxis = list(title = "Single Family Dwellings")
         )

# Numerical variable: BED_RMS (Sylvie)

# Numerical variable: FULL_BTH (Dawn)
full_bth <- table(pa$FULL_BTH)
full_bth_names <- as.numeric(names(full_bth))

plot_ly(x = full_bth_names,
        y = as.numeric(full_bth),
        type = "bar"
        )%>%
  layout(title = "Full Bathrooms",
         xaxis = list(title = "Bathrooms",
                      tickvals = seq(1:max(full_bth_names))),
         yaxis = list(title = "Single Family Dwellings")
  )

#Do the analysis as in Module3 for at least one set of two or more variables. Show appropriate plots for your data.

# CITY & TOTAL_VALUE (Dawn)
fig <- plot_ly(pa, y = ~VALUE_2021, color = ~CITY, type = "box")
fig <- fig %>% layout(title = "2021 Total Assessment Values",
                      yaxis = list(title = ""))
fig

fig <- plot_ly(pa, y = ~AMT_CHANGE, color = ~CITY, type = "box")
fig <- fig %>% layout(title = "Total Assessment Value Change from 2015 to 2021 ($)",
                      yaxis = list(title = "Change in Dollars"))
fig

fig <- plot_ly(pa, y = ~PCT_CHANGE, color = ~CITY, type = "box")
fig <- fig %>% layout(title = "Total Assessment Value Change from 2015 to 2021 (%)",
                      yaxis = list(title = "Change in Percentage"))
fig

# LIVING_AREA, EXT_COND & TOTAL_VALUE (Sylvie)

## Distribution
#Pick one variable with numerical data and examine the distribution of the data.

# TOTAL_VALUE (Dawn)
fig <- plot_ly(pa, y=pa$VALUE_2015, type = "box", name="2015")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2016, name="2016")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2017, name="2017")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2018, name="2018")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2019, name="2019")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2020, name="2020")
fig <- fig %>% add_trace(pa, y=pa$VALUE_2021, name="2021")
fig <- fig %>% layout(title = "2015-2021 Total Assessment Values")
fig

fig <- plot_ly(pa, x = ~VALUE_2021, type = "histogram")
fig <- fig %>% layout(title = "2021 Total Assessment Values",
                      xaxis = list(title = ""))
fig

## Central Limit Theorem
#Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable.

# TOTAL_VALUE (Sylvie)

## Sampling
#Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset.

# Simple random sampling (Sylvie)

# Systematic (Dawn)
N <- nrow(pa)
n <- 50
k <- ceiling(N / n)
r <- sample(k, 1)

# select every kth item
s <- seq(r, by = k, length = n)
sample <- pa[s, ]

fig <- plot_ly(sample, x = ~VALUE_2021, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "Systematic Sampling: 2021 Total Assessment Values",
                      xaxis = list(title = ""))
fig

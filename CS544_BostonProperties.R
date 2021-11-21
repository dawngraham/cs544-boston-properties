## CS544 Final Project: Analysis of Boston Property Assessments
## Sylvie Xiang, Dawn Graham

library(plotly)
library(plyr)

## 2021
pa <- read.csv("https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/c4b7331e-e213-45a5-adda-052e4dd31d41/download/data2021-full.csv",
               colClasses=c("ZIPCODE"="character"))

# Get only desired columns
cols <- c("PID", "CITY", "ZIPCODE", "LU_DESC", "OWN_OCC", "LIVING_AREA", "TOTAL_VALUE", "YR_BUILT", "EXT_COND", "BED_RMS", "FULL_BTH")

pa <- pa[, cols]

# Get only "SINGLE FAM DWELLING"
pa <- pa[pa$LU_DESC=="SINGLE FAM DWELLING",]

# Convert string "$ddd,ddd.dd" to numeric
pa$TOTAL_VALUE <- as.numeric(gsub('[$,]', '', pa$TOTAL_VALUE))

# Map values for OWN_OCC
pa$OWN_OCC <- mapvalues(pa$OWN_OCC,
                        from=c("Y", "N"),
                        to=c("Yes", "No"))

summary(pa)

## Analysis
#Do the analysis as in Module3 for at least one categorical variable and at least one numerical variable. Show appropriate plots for your data.

# Categorical variable: YR_BUILT (Sylvie)

# Categorical variable: OWN_OCC (Dawn)
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
plot_ly(pa,
        x = ~CITY,
        y = ~TOTAL_VALUE,
        type = "bar")

# LIVING_AREA, EXT_COND & TOTAL_VALUE (Sylvie)

## Distribution
#Pick one variable with numerical data and examine the distribution of the data.

# TOTAL_VALUE (Dawn)
plot_ly(pa,
        x = ~TOTAL_VALUE,
        type = "histogram")

## Central Limit Theorem
#Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable.

# TOTAL_VALUE (Sylvie)

## Sampling
#Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset.

# Simple random sampling (Sylvie)

# Systematic (Dawn)

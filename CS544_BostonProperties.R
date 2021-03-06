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
View(pa)



## Analysis
#Do the analysis as in Module3 for at least one categorical variable and at least one numerical variable. Show appropriate plots for your data.

##### Categorical variable: EXT_COND (Sylvie)
t <- table( pa$EXT_COND) ;t

plt <- barplot(t,ylim=c(0,25000), width = c(10,16,10,10,10),
               xlab="Exterior Conditions", ylab="Freq.",
               main="Single Family Dwellings Exterior Conditions Barplot", 
               col="light blue");plt

a <- prop.table(t)*100 ;a

b <- paste(round(a,2), "%", sep="") ;b
text(plt, 1600+a, labels=b)

#plt <- plot_ly(pa, x = ~EXT_COND, type = "histogram") ; plt

## Majority of the properties have "Average" condition, following with "Good"
## Properties with "Excellent" and "Poor" conditions together are 
## less than 0.3% of all the single family dwellings.


###### Categorical variable: OWN_OCC (Dawn)
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


##### Numerical variable: Property_Age (Sylvie)
pa$Property_Age <- rep(2021,nrow(pa))-pa$YR_BUILT

hist1 <- hist( pa$Property_Age, breaks = seq(0,320, 10), 
               xlim=c(0,320), ylim = c(0, 5000), col="blue",
               xlab="Property Age (in Years)", main="Single Family Dwellings Age Histogram")

axis(at=seq(0,320,50),side=1)

text(hist1$mids, 29 + hist1$counts, 
     labels = hist1$counts, adj = c(0, 0.5), srt = 90)

summary(pa$YearBuilt)
summary(pa$YR_BUILT)

### The oldest property was built in 1710 and newest was built in 2019
### Most of the single family dwellings were built 50 to 130 years ago.
### Average age of single family dwellings is 92 years, but with a right-tailed 
### distribution, the center of the data should be the median which is 94 years.


##### Numerical variable: BED_RMS (Dawn)
bed_rms <- table(pa$BED_RMS)
bed_rms_names <- as.numeric(names(bed_rms))

plot_ly(x = bed_rms_names,
        y = as.numeric(bed_rms),
        type = "bar"
        )%>%
  layout(title = "Bedrooms",
         xaxis = list(title = "Bedrooms",
                      tickvals = seq(1:max(bed_rms_names))),
         yaxis = list(title = "Single Family Dwellings")
  )

median(pa$BED_RMS, na.rm=T)

#Do the analysis as in Module3 for at least one set of two or more variables. Show appropriate plots for your data.

##### CITY & TOTAL_VALUE (Dawn)
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

###### LIVING_AREA, Property_Age, BD, BTH & TOTAL_VALUE (Sylvie)
data <- pa[c(6,20,9,10,17)];data
pairs(data)
cor(data)

### Total living area seems to have a positive relationship with total assessment values
### But year built, number of bedrooms and bathrooms don't seem to have relationship with TAV
### Living area, year built, numbers of bedrooms and bathrooms don't seem to 
### have correlations with each other.



## Distribution
#Pick one variable with numerical data and examine the distribution of the data.

##### TOTAL_VALUE (Dawn)
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

##### TOTAL_VALUE (Sylvie)
options(scipen=999)
par(mfrow=c(2,2))

c <- c()

set.seed(2)

for (size in c(300, 400, 500, 600)) {
  for (i in 1:6000) {
    c[i] <- mean( sample(pa$VALUE_2021, size = size, replace = F) )
  }
  
  hist(c, prob = T, xlab="inputs",
       xlim=c(500000,950000), ylim=c(0,0.000014),
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, 
      " Mean = ", mean(c),
      " SD = ", sd(c), "\n")
}

### After drawing 6000 samples with different sizes from 300 to 600, 
### the averages of sample means are showing normal distributions. 
### Total assessment values of 2021 does have applicability of Central Limit Theorem.



## Sampling
#Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset.

##### Simple random sampling without replacement (Sylvie)
library(sampling)
library(UsingR)

par(mfrow=c(2,2))
set.seed(2)

N <- nrow(pa)
n <- 500

srs <- srswor(n,N);srs

sample <- pa[srs!= 0,]

hist(sample$VALUE_2021, xlim=c(0,14000000),ylim=c(0,500),
     xlab="Sampling Assessments", 
     main="Using SRS without replace")

# City Representation
fig <- plot_ly(sample, x = ~CITY, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "SRS without Replacement: City Representation",
                      xaxis = list(title = ""))
fig

##### simple random sampling with replacement (Sylvie)
set.seed(2)

srs2 <- srswr(n,N) ;srs2
sample2 <- pa[srs2 != 0, ]

hist(sample2$VALUE_2021, xlim=c(0,14000000),ylim=c(0,500),
     xlab="Sampling Assessments Values", 
     main="Using SRS with replace")


## distribution without sampling
hist(pa$VALUE_2021, xlim=c(0,14000000), 
     xlab="Original Assessments Values",
     main="Assessment Values")


mean(sample$VALUE_2021)
mean(sample2$VALUE_2021)
mean(pa$VALUE_2021)
mean(pa$VALUE_2021) - mean(sample$VALUE_2021)
mean(pa$VALUE_2021) - mean(sample2$VALUE_2021)

# City Representation
fig <- plot_ly(sample2, x = ~CITY, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "SRS with Replacement: City Representation",
                      xaxis = list(title = ""))
fig

### The whole data set has mean of assessment values = $688,743.5,
### The simple random sampling without replacement has mean of assessment values = $727,306.4,
### The simple random sampling with replacement has mean of assessment values =$680,200.
### If used SRS without replacement instead of the whole data set, mean of the assessment values would be much larger,
### If used SRS with replacement, mean of assessment values would be smaller but not that much.


###### Systematic (Dawn)
N <- nrow(pa)
n <- 500
k <- floor(N / n)
r <- sample(k, 1)

# select every kth item
s <- seq(r, by = k, length = n)
sample.systematic <- pa[s, ]

mean(sample.systematic$VALUE_2021)

fig <- plot_ly(sample.systematic, x = ~VALUE_2021, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "Systematic Sampling: 2021 Total Assessment Values",
                      xaxis = list(title = ""))
fig

# City Representation
fig <- plot_ly(sample.systematic, x = ~CITY, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "Stratified Sampling: City Representation",
                      xaxis = list(title = ""))
fig

###### Stratified (Dawn)
set.seed(42)
pa <- pa[order(pa$CITY), ]
freq <- table(pa$CITY)

# Drop groups that are too small
pa2 <- pa[!(pa$CITY=="BROOKLINE" | pa$CITY=="DEDHAM"),]

freq <- table(pa2$CITY)
st.sizes <- 500 * freq / sum(freq)
st <- sampling::strata(pa2, stratanames = c("CITY"),
                       size = st.sizes, method = "srswor",
                       description = TRUE)

sample.stratified <- getdata(pa2, st)

mean(sample.stratified$VALUE_2021)

fig <- plot_ly(sample.stratified, x = ~VALUE_2021, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "Stratified Sampling: 2021 Total Assessment Values",
                      xaxis = list(title = ""))
fig

# City Representation
fig <- plot_ly(sample.stratified, x = ~CITY, type = "histogram", histnorm='probability')
fig <- fig %>% layout(title = "Stratified Sampling: City Representation",
                      xaxis = list(title = ""))
fig

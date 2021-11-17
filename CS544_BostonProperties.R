## CS544 Final Project: Analysis of Boston Property Assessments
## Sylvie Xiang, Dawn Graham

## 2021
pa21 <- read.csv("https://raw.githubusercontent.com/dawngraham/cs544-boston-properties/main/data/data2021-full.csv")

# Get only desired columns
cols <- c("PID", "CITY", "ZIPCODE", "LUC", "OWN_OCC", "LIVING_AREA", "TOTAL_VALUE", "YR_BUILT", "OVERALL_COND", "BED_RMS", "FULL_BTH")

# CITY is not included in 2019 data

pa21 <- pa21[, cols]

# Get only "SINGLE FAM DWELLING"
pa21 <- pa21[pa21$LUC==101,]

# Convert string "$ddd,ddd.dd" to numeric
pa21$TOTAL_VALUE <- as.numeric(gsub('[$,]', '', pa21$TOTAL_VALUE))

summary(pa21)

## 2019

pa19 <- read.csv("https://raw.githubusercontent.com/dawngraham/cs544-boston-properties/main/data/fy19fullpropassess.csv")

# Get only desired columns
cols <- c("PID", "ZIPCODE", "PTYPE", "OWN_OCC", "LIVING_AREA", "AV_TOTAL", "YR_BUILT", "R_OVRALL_CND", "R_BDRMS", "R_FULL_BTH")

# ZIPCODE would need to be cleaned for 2019 - missing leading 0's
# LUC in 2021 is PTYPE in 2019
# TOTAL_VALUE in 2021 is AV_TOTAL in 2019
# OVERALL_COND in 2021 is R_OVRALL_CND in 2019
# BED_RMS in 2021 is R_BDRMS in 2019
# FULL_BTH in 2021 is R_FULL_BTH in 2019

pa19 <- pa19[, cols]

# Get only "SINGLE FAM DWELLING"
pa19 <- pa19[pa19$PTYPE==101,]

# Convert string "$ddd,ddd.dd" to numeric
pa19$AV_TOTAL <- as.numeric(gsub('[$,]', '', pa19$AV_TOTAL))

summary(pa19)




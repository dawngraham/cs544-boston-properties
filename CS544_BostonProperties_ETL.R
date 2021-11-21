# Get columns of interest from 2021 Boston property assessments
# and merge Total Assessment Values for previous years.

cols2021 <- c("PID", "CITY", "ZIPCODE", "LUC", "OWN_OCC", "LIVING_AREA", "YR_BUILT", "EXT_COND", "BED_RMS", "FULL_BTH", "TOTAL_VALUE")

pa21 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/c4b7331e-e213-45a5-adda-052e4dd31d41/download/data2021-full.csv", colClasses=c("ZIPCODE"="character"))[ , cols2021]
# Limit to "SINGLE FAMILY DWELLINGS"
pa21 <- pa21[pa21$LUC==101, ]
names(pa21)[names(pa21) == 'TOTAL_VALUE'] <- 'VALUE_2021'
# Convert string "$ddd,ddd.dd" to numeric
pa21$VALUE_2021 <- as.numeric(gsub('[$,]', '', pa21$VALUE_2021))

pa20 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/8de4e3a0-c1d2-47cb-8202-98b9cbe3bd04/download/data2020-full.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa20 <- pa20[pa20$PTYPE==101, cols]
names(pa20)[names(pa20) == 'AV_TOTAL'] <- 'VALUE_2020'

pa19 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/695a8596-5458-442b-a017-7cd72471aade/download/fy19fullpropassess.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa19 <- pa19[pa19$PTYPE==101, cols]
names(pa19)[names(pa19) == 'AV_TOTAL'] <- 'VALUE_2019'

pa18 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/fd351943-c2c6-4630-992d-3f895360febd/download/ast2018full.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa18 <- pa18[pa18$PTYPE==101, cols]
names(pa18)[names(pa18) == 'AV_TOTAL'] <- 'VALUE_2018'

pa17 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/062fc6fa-b5ff-4270-86cf-202225e40858/download/property-assessment-fy2017.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa17 <- pa17[pa17$PTYPE==101, cols]
names(pa17)[names(pa17) == 'AV_TOTAL'] <- 'VALUE_2017'
#Remove trailing character and make PID numeric
pa17$PID <- as.numeric(gsub("[^0-9.-]", "", pa17$PID))

pa16 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/cecdf003-9348-4ddb-94e1-673b63940bb8/download/property-assessment-fy2016.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa16 <- pa16[pa16$PTYPE==101, cols]
names(pa16)[names(pa16) == 'AV_TOTAL'] <- 'VALUE_2016'
#Remove trailing character and make PID numeric
pa16$PID <- as.numeric(gsub("[^0-9.-]", "", pa16$PID))

pa15 <- read.csv(file = "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/bdb17c2b-e9ab-44e4-a070-bf804a0e1a7f/download/property-assessment-fy2015.csv")[ ,c('PID', 'PTYPE', 'AV_TOTAL')]
cols <- c('PID', 'AV_TOTAL')
pa15 <- pa15[pa15$PTYPE==101, cols]
names(pa15)[names(pa15) == 'AV_TOTAL'] <- 'VALUE_2015'
#Remove trailing character and make PID numeric
pa15$PID <- as.numeric(gsub("[^0-9.-]", "", pa15$PID))

# Merge
pa <- merge(pa21, pa15, by = "PID")
pa <- merge(pa, pa16, by = "PID")
pa <- merge(pa, pa17, by = "PID")
pa <- merge(pa, pa18, by = "PID")
pa <- merge(pa, pa19, by = "PID")
pa <- merge(pa, pa20, by = "PID")

# Move VALUE_2021 to end
pa <- pa[c(setdiff(names(pa), "VALUE_2021"), "VALUE_2021")]

write.csv(pa, file="pa.csv", row.names = FALSE)

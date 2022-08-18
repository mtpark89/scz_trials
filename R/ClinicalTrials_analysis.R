library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(matrixStats)

gf <- read_csv("SearchResults_allcolumns_20220817.csv")

###Filtering for only "Drug" interventions
gf %<>% filter(grepl('Drug:', Interventions))

###Function to parse dates--either in date (July 13, 2018) or month/year form
parse_dates <- function(input) {
	length <- lengths(strsplit(input, " "))
	
	if(length==2) {
		month <- unlist(strsplit(input, " "))[1]
		date <- NA
		year <- unlist(strsplit(input, " "))[2]
	}

	if(length==3) {
		month <- unlist(strsplit(input, " "))[1]
		date <- unlist(strsplit(input, " "))[2] %>% gsub(",", "", .)
		year <- unlist(strsplit(input, " "))[3]
	}

	return(c(month,date,year))
}

###Parse dates and add to dataframe
ct_dates <- data.frame(NULL)
for (i in 1:nrow(gf)) {
	ct_dates <- rbind(ct_dates, parse_dates(gf$`Start Date`[i]))
}
colnames(ct_dates) <- c("start_month", "start_date", "start_year")

gf %<>% cbind(., ct_dates)

summary(as.factor(gf$start_year)) ###To exclude start year 2050 (error)

###Filtering medications under study

gf$Interventions %>% strsplit(., "\\|") %>% lengths()

test <- gf$Interventions %>% strsplit(., "\\|") %>% lengths()
test %<>% as.data.frame()

what <- gf[test$.==1,]

###Filtering outcome measures, primary is the first one? Recheck later...
patterns <- c("positive", "negative", "PANSS", "CGI", "clinical")

test <- gf %>% filter(grepl(paste(patterns, collapse="|"), `Outcome Measures`, ignore.case = TRUE))

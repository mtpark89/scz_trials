library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(matrixStats)

gf <- read_csv("data/SearchResults_allcolumns_20220817.csv")

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
	
	if(is.na(strsplit(input, " "))==TRUE) {
	month="NA"
	date="NA"
	year="NA"
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

summary(as.factor(gf$start_year)) ###To exclude start year 2050 later?

###Filtering medications under study

gf$Interventions_lengths <- gf$Interventions %>% strsplit(., "\\|") %>% lengths()

#Most likely main intervention--to check after
gf %<>% rowwise() %>% mutate(First_intervention =( Interventions%>% strsplit(., "\\|") %>% unlist() %>% .[1] %>% strsplit(., " ") %>% unlist() %>% .[2]))

library(webchem)

#Get CIDs, and record descriptions from PubChem
gf$CID <- get_cid(gf$First_intervention, match="first")

pc_sect(test$cid, "Record Description")

descriptions <- pc_sect(gf$CID, "Record Description")
descriptions_compound <- pc_sect(gf$CID, "Record Description", domain="compound")

patterns <- c("choline", "muscari")

descriptions_cholinergic <- descriptions %>% filter(grepl(paste(patterns, collapse="|"), Result, ignore.case=TRUE))

################################################################################33
###Not used

###Filtering outcome measures, primary is the first one? Recheck later...
patterns <- c("positive", "negative", "PANSS", "CGI", "clinical")

test <- gf %>% filter(grepl(paste(patterns, collapse="|"), `Outcome Measures`, ignore.case = TRUE))



if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ChemmineDrugs")



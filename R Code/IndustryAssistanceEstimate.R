# Project: CA Cap-and-Trade Allowances and Revenue
# File: Industry Assistance Estimate
# -----------------
# Use 2010 MRR to get 90% of emissions for industry sectors
# Use formula to calculate industry assistance
# Table 8-1: Industry Assistance Factor
# Table 9-2: Cap Adjustment Factor (c)
# Table 9-1: Product-Based Emissions Efficiency Benchmarks
# Product Output-Based Allocation formula (§ 95891, p. 104): sum output * efficiency benchmark * AF * c
# assume that output * efficiency benchmark ~ 90% mean emissions per sector

###################
# GLOBALS & DEPENDENCIES
###################
source("R Code/HelperFunctions.R")

# MRR summary file converted to csv, from 
# http://www.arb.ca.gov/cc/reporting/ghg-rep/reported_data/ghg-reports.htm
# http://www.arb.ca.gov/cc/reporting/ghg-rep/reported_data/mandatory_reporting_facility_summary_2010_10_28_2011.xlsx
MRR_SUMMARY_FILE     <- "Data/mandatory_reporting_facility_summary_2010_10_28_2011-1.csv"
INDUSTRY_ASSISTANCE_FILE <- "Data/IndustryAssistance.csv"   # from § 95870 tbl.8-1

# Import MRR Summary data
# -----------------
MRR_2010 <- read.csv(MRR_SUMMARY_FILE, skip=15, header=T)

# set column names from the data file
colnames(MRR_2010) <- c("Facility.ID", "Facility.Name", "CO2e.Total", "CO2e.Non.Biomass", "CO2e.Biomass",
                        "Verification", "Free.of.Material.Misstatement", "Free.of.Non.Conformance", "City", "State", "Zip.Code", 
                        "Primary.Reporting.Sector", "Secondary.Reporting.Sector", "NAICS.Code")

# remove the commas from numbers
MRR_2010$CO2e.Total <- as.numeric(gsub(pattern=",", replacement="", MRR_2010$CO2e.Total)) 
MRR_2010$CO2e.Non.Biomass <- as.numeric(gsub(pattern=",", replacement="", MRR_2010$CO2e.Non.Biomass)) 
MRR_2010$CO2e.Biomass <- as.numeric(gsub(pattern=",", replacement="", MRR_2010$CO2e.Biomass)) 

# Note there are some NAs, which we need to omit
to_omit <- is.na(MRR_2010$CO2e.Non.Biomass)
MRR_2010 <- MRR_2010[!to_omit, ]

# pull out the NAICS codes
MRR_2010$NAICS.Num <- sapply(as.character(MRR_2010$NAICS.Code), function(x) { as.numeric(strsplit(x, split=" - ", fixed=T)[[1]][1]) } )


# Import Industry Assistance data
# -----------------
AF.tbl <- read.csv(INDUSTRY_ASSISTANCE_FILE)
AF.tbl[, c("AF2013", "AF2015", "AF2018")] <- apply(AF.tbl[, c("AF2013", "AF2015", "AF2018")], MARGIN=c(1,2), ConvertToPercentage)

# Identify NAICS codes in the Industry Assistance for which there is no corresponding MRR data
missing_NAICS <- setdiff(AF.tbl$NAICS.Code, MRR_2010$NAICS.Num)
message("These NAICS from Industry Assistance have no corresponding MRR data:")
print(AF.tbl[AF.tbl$NAICS.Code %in% missing_NAICS, c("NAICS.Code", "Activities")])

# Select MRR data according to AF NAICS.Code
# -----------------
nonbiomass_by_industry <- by(MRR_2010$CO2e.Non.Biomass, MRR_2010$NAICS.Num, sum)
total_industry_emissions <- nonbiomass_by_industry[as.character(AF.tbl$NAICS.Code)]

# get rid of NAs
na_index <- is.na(total_industry_emissions) 
total_industry_emissions <- total_industry_emissions[!na_index]


# total industry emissions
INDUSTRY_PORTION <- sum(total_industry_emissions) * .9
message("90% of Total Industry Emissions: ", prettyNum(INDUSTRY_PORTION, big.mark=","))


# Set INDUSTRY_PORTION based on the AF * c factors for specific industry
# -----------------
cap_adj_factor <- matrix(0, nrow=8, ncol=2)
rownames(cap_adj_factor) <- 2013:2020
colnames(cap_adj_factor) <- c("Normal", "Special")
SPECIAL_CAP_ADJ_NAICS <- c(325311, 327311, 327410)

cap_adj_factor[,"Normal"] <- c(.981, .963, .944, .925, .907, .888, .869, .851)
cap_adj_factor[,"Special"] <- c(.991, .981, .972, .963, .953, .944, .935, .925)

# Industry x Year
industry_emissions.mat <- matrix(total_industry_emissions, nrow=8, ncol=length(total_industry_emissions), byrow=T)
rownames(industry_emissions.mat) <- 2013:2020
colnames(industry_emissions.mat) <- names(total_industry_emissions)

# Multiply by 90%
industry_emissions.mat <- industry_emissions.mat * .9

#  Multiply by Cap adjustment factor
is_special <- colnames(industry_emissions.mat) %in% as.character(SPECIAL_CAP_ADJ_NAICS)
industry_emissions.mat[,is_special] <- industry_emissions.mat[,is_special] * cap_adj_factor[,"Special"]
industry_emissions.mat[,!is_special] <- industry_emissions.mat[,!is_special] * cap_adj_factor[,"Normal"]

# Multiply by AF
af_index <- as.character(AF.tbl$NAICS.Code) %in% colnames(industry_emissions.mat)
NAICS_index <- as.character(AF.tbl$NAICS.Code[af_index])

# AF2013
industry_emissions.mat[c("2013", "2014"), NAICS_index] <- industry_emissions.mat[c("2013","2014"), NAICS_index] * AF.tbl$AF2013[af_index]

# AF2015
industry_emissions.mat[c("2015", "2016", "2017"), NAICS_index] <- industry_emissions.mat[c("2015", "2016", "2017"), NAICS_index] * AF.tbl$AF2015[af_index]

# AF2018
industry_emissions.mat[c("2018", "2019", "2020"), NAICS_index] <- industry_emissions.mat[c("2018", "2019", "2020"), NAICS_index] * AF.tbl$AF2018[af_index]

# shift to millions of tons
INDUSTRY_PORTION <- rowSums(industry_emissions.mat) * 10^-6


# Modify the allowance allocation table
#######################################
if(!exists("allowance_alloc.tbl")) source("R Code/BasicAllowanceAllocation.R")

allowance_alloc.tbl <- cbind(allowance_alloc.tbl, Industry=0, Remainder=0)
allowance_alloc.tbl[,"Industry"] <- INDUSTRY_PORTION
allowance_alloc.tbl[,"Remainder"] <- allowance_alloc.tbl[,"Industry.Plus.Remainder"] - INDUSTRY_PORTION

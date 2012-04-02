# Project: CA Cap-and-Trade Allowances and Revenue
# File: Import Utility Percentages
# -----------------
# Using a csv of the utility percentages table (§ 95892 tbl.9-3), determines the number of allowances going to IOUs and POUs
# Note: co-ops lumped with POUs

###################
# GLOBALS & DEPENDENCIES
###################
source("R Code/HelperFunctions.R")
UTILITY_PERCENTAGES_FILE <- "Data/UtilityPercentages.csv"


###################
# LOAD UTILITY CSV FILE
###################
UtilityPercentages.tbl <- read.csv(UTILITY_PERCENTAGES_FILE, header=T)
UtilityPercentages.tbl[,3:10] <- apply(UtilityPercentages.tbl[,3:10], MARGIN=c(1,2), ConvertToPercentage)

# note that the percentages should sum to basically 1, within rounding error:
# colSums(UtilityPercentages.tbl[,3:10])

###################
# CREATE NEW COLUMNS IN THE ALLOWANCE TABLE SUMMING POU AND IOU TOTALS BY YEAR
###################
if(!exists("allowance_alloc.tbl")) source("R Code/BasicAllowanceAllocation.R")

type_totals <- by(UtilityPercentages.tbl[,3:10], UtilityPercentages.tbl[,"Utility.Type"], colSums)
allowance_alloc.tbl <- cbind(allowance_alloc.tbl, POU=0, IOU=0)
allowance_alloc.tbl[,"POU"] <- allowance_alloc.tbl[,"Utilities"] * (type_totals$COOP + type_totals$POU)
allowance_alloc.tbl[,"IOU"] <- allowance_alloc.tbl[,"Utilities"] - allowance_alloc.tbl[,"POU"]
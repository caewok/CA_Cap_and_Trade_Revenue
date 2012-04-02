# Project: CA Cap-and-Trade Allowances and Revenue
# File: Basic Allowance Allocation
# -----------------
# Constructs table of allowance allocation taking data from the Final cap-and-trade regs (§ 95801 et seq)
# Table has rows 2013 through 2020
# Table columns: Total.Allowances, Allowance.Price.Reserve, Advance.Auction, VRE.Reserve, Utilities, Industry.Plus.Remainder

###################
# CREATE INITIAL TABLE
###################
# See § 95870 for a description of each column
allowance_alloc.tbl <- matrix(0, nrow=8, ncol=6)
rownames(allowance_alloc.tbl) <- 2013:2020
colnames(allowance_alloc.tbl) <- c(
     "Total.Allowances", 
     "Allowance.Price.Reserve", 
     "Advance.Auction", 
     "VRE.Reserve", 
     "Utilities", 
     "Industry.Plus.Remainder"
     )

###################
# ALLOWANCE ALLOCATIONS FROM REGS
###################
# Total Allowances. § 95841 tbl.6-1
TOTAL_ALLOWANCES <- list(
     '2013'=162.8,
     '2014'=159.7,
     '2015'=394.5,
     '2016'=382.4,
     '2017'=370.4,
     '2018'=358.3,
     '2019'=346.3,
     '2020'=334.2
     )

# Percentage of allowances devoted to the auction reserve. § 95870(a)
RESERVE_MULTIPLIER <- list(
     '2013'=.01,
     '2014'=.01,
     '2015'=.04,
     '2016'=.04,
     '2017'=.04,
     '2018'=.07,
     '2019'=.07,
     '2020'=.07
     )

# Percentage of allowances devoted to advance auction. § 95870(b)
ADVANCE_AUCTION_MULTIPLIER = .1

# Percentage of allowances devoted to the VRE Reserve. § 95870(c)
VRE_RESERVE_MULTIPLIER <- list(
     '2013'=.005,
     '2014'=.005,
     '2015'=.0025,
     '2016'=.0025,
     '2017'=.0025,
     '2018'=.0025,
     '2019'=.0025,
     '2020'=.0025
     )

# Allowances devoted to utilities are decreased by this factor each year. § 95981 tbl.9-2
# See also § 95870(d): 97.7 million metric tons * cap adjustment factor
UTILITY_CAP_ADJUSTMENT_FACTOR <- list(
     '2013'=.981,
     '2014'=.963,
     '2015'=.944,
     '2016'=.925,
     '2017'=.907,
     '2018'=.888,
     '2019'=.869,
     '2020'=.851
     )

# Allocation to utilties before taking cap adjustment factor into account. § 95870(d)
UTILITY_INITIAL_ALLOCATION <- 97.7

###################
# POPULATE TABLE WITH RELEVANT DATA
###################
allowance_alloc.tbl[,"Total.Allowances"] <- unlist(TOTAL_ALLOWANCES)
allowance_alloc.tbl[,"Allowance.Price.Reserve"] <- mapply("*", TOTAL_ALLOWANCES, RESERVE_MULTIPLIER)
allowance_alloc.tbl[as.character(2015:2020),"Advance.Auction"] <- mapply("*", TOTAL_ALLOWANCES[as.character(2015:2020)], ADVANCE_AUCTION_MULTIPLIER)
allowance_alloc.tbl[,"VRE.Reserve"] <- mapply("*", TOTAL_ALLOWANCES, VRE_RESERVE_MULTIPLIER)
allowance_alloc.tbl[,"Utilities"] <- mapply("*", UTILITY_INITIAL_ALLOCATION, UTILITY_CAP_ADJUSTMENT_FACTOR)
allowance_alloc.tbl[,"Industry.Plus.Remainder"] <- allowance_alloc.tbl[,"Total.Allowances"] - 
     rowSums(allowance_alloc.tbl[,c("Allowance.Price.Reserve", "Advance.Auction", "VRE.Reserve", "Utilities")])
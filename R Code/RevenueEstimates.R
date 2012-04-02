# Project: CA Cap-and-Trade Allowances and Revenue
# File: Revenue Estimates
# -----------------
# Given allowance taable and prices, calculates revenues for advance auction, remainder, and consignment IOUs

source("R Code/ImportUtilityPercentages.R")
source("R Code/IndustryAssistanceEstimate.R")
source("R Code/PriceFloorandCeiling.R")

# ADVANCE AUCTION
# in millions of $

# Need to move these to the corresponding year 
adv_auction <- c(allowance_alloc.tbl[c('2015', '2016', '2017', '2018', '2019', '2020'),"Advance.Auction"], 0, 0, 0)
names(adv_auction) <- 2012:2020
r1 <- round(allowance_prices.tbl * adv_auction)
r1

# REMAINDER
# in millions of $
r2 <- round(allowance_prices.tbl[-1,] * allowance_alloc.tbl[,"Remainder"])
r2

# CONSIGNMENT: IOUs
# in millions of $
r3 <- round(allowance_prices.tbl[-1,] * allowance_alloc.tbl[,"IOU"])
r3

# TOTAL
r_total <- r1 + rbind('2012'=c(0,0,0,0), r2) + rbind('2012'=c(0,0,0,0), r3)
r_total
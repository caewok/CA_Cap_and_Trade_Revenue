# Project: CA Cap-and-Trade Allowances and Revenue
# File: PlotAllowances
# -----------------
# Creates bar charts of allowance distribution

###################
# DEPENDENCIES
###################
source("R Code/ImportUtilityPercentages.R")
source("R Code/PriceFloorandCeiling.R")


library(ggplot2)

# UTILITIES
# ---------
ln <- nrow(allowance_alloc.tbl)
dat <- data.frame(
     Year=as.numeric(rep(times=2,rownames(allowance_alloc.tbl))), 
     Utility=c(rep(times=ln, "POU"), rep(times=ln, "IOU")),
     Allowances=c(allowance_alloc.tbl[,"POU"], allowance_alloc.tbl[,"IOU"])
     )

utilities_gg <- ggplot(dat, aes(as.factor(Year), Allowances, fill=Utility))
utilities_gg + geom_bar(alpha=3/4, show_guide=F) + facet_wrap(~ Utility) + labs(x="Year", y="Allowances (MMT CO2e)")


ggsave("Figures/Allowances for Utilities.jpg")


# ADVANCE AUCTION
# ---------------
dat <- data.frame(Auction_Year=2012:2020, Advance_Allowances=c(allowance_alloc.tbl[3:8,"Advance.Auction"], 0, 0, 0))
dat$Vintage.Year <- c(2015:2020, NA, NA, NA)

advance_gg <- ggplot(dat, aes(as.factor(Auction_Year), Advance_Allowances, fill=Vintage.Year))
advance_gg + geom_bar(alpha=3/4) + labs(x="Auction Year", y="Advance Allowances (MMT CO2e)")


ggsave("Figures/Fig 3 - Advance Allowances.jpg")


# PRICE FLOOR AND RESERVE PRICES
# Plots prices given average cpi, with lines representing a range between min and max
# -----------------
ANNUAL_CPI_ESTIMATE <- .02
MAX_CPI_ESTIMATE <- .04
MIN_CPI_ESTIMATE <- 0

dat <- as.data.frame(cbind(CalculatePriceLimits(ANNUAL_CPI_ESTIMATE), CalculatePriceLimits(MIN_CPI_ESTIMATE), CalculatePriceLimits(MAX_CPI_ESTIMATE)))
colnames(dat) <- c("Price.Floor", "Reserve.Tier.1", "Reserve.Tier.2", "Reserve.Tier.3", 
                   "Price.Floor.Low", "Reserve.Tier.1.Low", "Reserve.Tier.2.Low", "Reserve.Tier.3.Low",
                   "Price.Floor.High", "Reserve.Tier.1.High", "Reserve.Tier.2.High", "Reserve.Tier.3.High")

# move to factor for reserve type
dat2 <- as.data.frame(matrix(unlist(dat), ncol=3))
colnames(dat2) <- c("Estimate", "Low", "High")
dat2$Year = rep(times=4, 2012:2020)
dat2$Type = c(rep(times=9, "Price.Floor"), 
              rep(times=9, "Reserve.Tier.1"),
              rep(times=9, "Reserve.Tier.2"),
              rep(times=9, "Reserve.Tier.3")
              )


floor_gg2 <- ggplot(dat2, aes(Year, Estimate, ymin=Low, ymax=High, group=Type, colour=Type))
floor_gg2 + geom_pointrange() + labs(x="Auction Year", y="$") + annotate("text", label="Dots: prices based on constant 2% annual CPI.\nBars: range of prices based on 0% to 4% CPI.", x=2015, y=90, size=4, color="black", fontface="italic")
# will have 3 NAs b/c no reserve sales in 2012, only advance auction


ggsave("Figures/Fig 2 - Price Floor and Ceiling.jpg")


# TOTAL ALLOWANCES OFFERED AT AUCTION
# -----------------
# Include remainder + industry
# by auction year

dat <- data.frame(Allowance.Amount=c(
     allowance_alloc.tbl[,"Advance.Auction"],
     allowance_alloc.tbl[,"IOU"],
     allowance_alloc.tbl[,"POU"],
     allowance_alloc.tbl[,"Industry.Plus.Remainder"]
     ),
                  Allowance.Type=c(
                       rep(times=8, "Advance"),
                       rep(times=8, "IOU"),
                       rep(times=8, "POU"),
                       rep(times=8, "Industry.Plus.Remainder")
                       ),
                  Year=as.integer(rep(times=4, rownames(allowance_alloc.tbl)))
                  )
# change year for Advance auction
dat[dat$Allowance.Type == "Advance", "Year"] <- c(NA, NA, 2012:2017)
dat <- na.omit(dat)

auction_gg <- ggplot(dat, aes(Allowance.Type, weight=Allowance.Amount, fill=Allowance.Type))
auction_gg + geom_bar(alpha=3/4) + facet_wrap(~ Year) + labs(y="Number of Allowances (MMT CO2e)") + opts(axis.text.x=theme_blank(), axis.title.x=theme_blank())

ggsave("Figures/Fig 4 - Auction Allowances.jpg")



# TOTAL ALLOWANCE ALLOCATION
# -----------------
# Show industry plus remainder calculated together
# by vintage year

dat <- data.frame(Allowance.Amount=c(
     allowance_alloc.tbl[,"Allowance.Price.Reserve"],
     allowance_alloc.tbl[,"Advance.Auction"],
     allowance_alloc.tbl[,"VRE.Reserve"],
     allowance_alloc.tbl[,"POU"],
     allowance_alloc.tbl[,"IOU"],
     allowance_alloc.tbl[,"Industry.Plus.Remainder"]
     ),
                  Allowance.Type=c(
                       rep(times=8, "Reserve"),
                       rep(times=8, "Advance"),
                       rep(times=8, "VRE"),
                       rep(times=8, "POU"),
                       rep(times=8, "IOU"),
                       rep(times=8, "Industry.Plus.Remainder")
                       ),
                  Year=rep(times=6, rownames(allowance_alloc.tbl))
                  )

allowance_gg <- ggplot(dat, aes(x=Allowance.Type, y=Allowance.Amount, fill=Allowance.Type))
allowance_gg + geom_bar() + facet_wrap(~ Year) + opts(axis.text.x=theme_blank(), axis.title.x=theme_blank())

ggsave("Figures/Total Allowance Allocation.jpg")

# TOTAL ALLOWANCE ALLOCATION ASSUMING CONSTANT INDUSTRY EMISSIONS FOR FREE ALLOWANCES
# just like the previous figure, except industry allowances are estimated separately from remainder
source("R Code/IndustryAssistanceEstimate.R")
dat <- data.frame(Allowance.Amount=c(
     allowance_alloc.tbl[,"Allowance.Price.Reserve"],
     allowance_alloc.tbl[,"Advance.Auction"],
     allowance_alloc.tbl[,"VRE.Reserve"],
     allowance_alloc.tbl[,"POU"],
     allowance_alloc.tbl[,"IOU"],
     allowance_alloc.tbl[,"Industry"],
     allowance_alloc.tbl[,"Remainder"]
     ),
                  Allowance.Type=c(
                       rep(times=8, "Reserve"),
                       rep(times=8, "Advance"),
                       rep(times=8, "VRE"),
                       rep(times=8, "POU"),
                       rep(times=8, "IOU"),
                       rep(times=8, "Industry"),
                       rep(times=8, "Remainder")
                       ),
                  Year=rep(times=7, rownames(allowance_alloc.tbl))
                  )

allowance_gg <- ggplot(dat, aes(x=Allowance.Type, y=Allowance.Amount, fill=Allowance.Type))
allowance_gg + geom_bar() + facet_wrap(~ Year) + opts(axis.text.x=theme_blank(), axis.title.x=theme_blank())

ggsave("Figures/Total Allowance Allocation with Industry Estimate.jpg")

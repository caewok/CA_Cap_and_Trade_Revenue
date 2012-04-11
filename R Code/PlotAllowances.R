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
utilities_gg + geom_bar(alpha=3/4, show_guide=F) + facet_wrap(~ Utility) + 
     labs(x="Year", y=expression(paste("Number of Allowances (", MMTCO[2], plain(e), ")"))) +
     opts(theme_text(size=14), title="Utility Allowances")


ggsave("Figures/Allowances for Utilities.jpg", height=5, width=7, units="in")


# ADVANCE AUCTION
# ---------------
dat <- data.frame(Auction_Year=2012:2020, Advance_Allowances=c(allowance_alloc.tbl[3:8,"Advance.Auction"], 0, 0, 0))
dat$Vintage.Year <- c(2015:2020, NA, NA, NA)

advance_gg <- ggplot(dat, aes(as.factor(Auction_Year), Advance_Allowances, fill=Vintage.Year))
advance_gg + geom_bar(alpha=3/4) + labs(x="Auction Year", y=expression(paste("Number of Allowances (", MMTCO[2], plain(e), ")"))) +
     opts(theme_text(size=14), title="Advance Allowances")


ggsave("Figures/Fig 3 - Advance Allowances.jpg", height=5, width=7, units="in")


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
                       rep(times=8, "Industry +\nRemainder")
                       ),
                  Year=as.integer(rep(times=4, rownames(allowance_alloc.tbl)))
                  )
# change year for Advance auction
dat[dat$Allowance.Type == "Advance", "Year"] <- c(NA, NA, 2012:2017)
dat <- na.omit(dat)

auction_gg <- ggplot(dat, aes(Allowance.Type, weight=Allowance.Amount, fill=Allowance.Type))
auction_gg + geom_bar(alpha=3/4) + facet_wrap(~ Year) + labs(y="Number of Allowances (MMT CO2e)") + 
     opts(axis.text.x=theme_blank(), axis.title.x=theme_blank(), theme_text(size=14), title="Potential Allowances for CA Cap-and-Trade Auction")

ggsave("Figures/Fig 4 - Auction Allowances.jpg", height=5, width=7, units="in")



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
                       rep(times=8, "Industry +\nRemainder")
                       ),
                  Year=rep(times=6, rownames(allowance_alloc.tbl))
                  )

allowance_gg <- ggplot(dat, aes(x=Allowance.Type, y=Allowance.Amount, fill=Allowance.Type))
allowance_gg + geom_bar() + facet_wrap(~ Year) + ylab(expression(paste("Number of Allowances (", MMTCO[2], plain(e), ")"))) +
     opts(axis.text.x=theme_blank(), axis.title.x=theme_blank(), theme_text(size=14), title="Allowance Categories by Vintage Year")

ggsave("Figures/Total Allowance Allocation.jpg", height=5, width=7, units="in")

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
# or use reshape
library(reshape)
colnames(allowance_alloc.tbl)[2] <- "Reserve"
dat <- melt(subset(allowance_alloc.tbl, select=c(-Total.Allowances, -Utilities, -Industry.Plus.Remainder)))
colnames(dat) = c("Year", "Allowance.Type", "Allowance.Amount")

allowance_gg <- ggplot(dat, aes(x=Allowance.Type, y=Allowance.Amount, fill=Allowance.Type))
allowance_gg + geom_bar() + facet_wrap(~ Year) + ylab(expression(paste("Number of Allowances (", MMTCO[2], plain(e), ")"))) + 
     opts(axis.text.x=theme_blank(), axis.title.x=theme_blank(), theme_text(size=14), title="Allowance Categories with Industry Estimate")

ggsave("Figures/Total Allowance Allocation with Industry Estimate.jpg", height=5, width=7, units="in")


# INDUSTRY ALLOCATION
dat <- data.frame(Year=rownames(allowance_alloc.tbl), Industry=allowance_alloc.tbl[, "Industry"])

industry_gg <- ggplot(dat, aes(as.factor(Year), Industry))
industry_gg + geom_bar(alpha=3/4, fill="dark blue") + labs(x="Industry Allowances", y=expression(paste("Number of Allowances (", MMTCO[2], plain(e), ")"))) +
     opts(theme_text(size=14), title="Estimated Industry Allowances")

ggsave("Figures/Industry Allowances.jpg", height=5, width=7, units="in")


# POTENTIAL ALLOWANCES FOR AUCTION WITH INDUSTRY ESTIMATE
# -----------------
source("R Code/IndustryAssistanceEstimate.R")
library(reshape)
dat <- allowance_alloc.tbl
colnames(dat)[3] <- "Advance"
colnames(dat)[6] <- "Industry +\nRemainder"

# shift advance allowances to auction year
dat <- rbind(dat[1,], dat)
rownames(dat) <- 2012:2020
dat["2012",] <- 0
dat[,"Advance"] <- c(dat[as.character(2015:2020), "Advance"], 0 ,0, 0)
     
dat <- melt(subset(dat, select=c(3, 6, 5)))
colnames(dat) <- c("Auction.Year", "Allowance.Type", "Allowance.Amount")

# add lines for remainder/industry split, IOU/POU split
dat <- cbind(dat, hline=c(rep(times=9, NA), NA, allowance_alloc.tbl[,"Remainder"], NA, allowance_alloc.tbl[,"IOU"]),
             ymax=c(rep(times=9, NA), NA, allowance_alloc.tbl[,"Industry.Plus.Remainder"], NA, allowance_alloc.tbl[,"Utilities"]))

auction_gg <- ggplot(dat, aes(Allowance.Type, weight=Allowance.Amount, fill=Allowance.Type))
p <- auction_gg + geom_bar(alpha=3/4) + facet_wrap(~ Auction.Year) + labs(y="Number of Allowances (MMT CO2e)") + 
     opts(axis.text.x=theme_blank(), axis.title.x=theme_blank(), theme_text(size=14), 
          title="Potential Allowances for CA Cap-and-Trade Auction")

p + geom_errorbar(aes(y=hline, ymax=ymax, ymin=hline), col="red", linetype="dashed") + xlab("Red dashed areas represent estimated Industry\nand POU allowances, respectively.") +opts(axis.title.x=theme_text(face="italic"))

ggsave("Figures/Auction Allowances with Industry.jpg", height=5, width=7, units="in")



# AUCTION REVENUE 2012-2013 WITH INDUSTRY ESTIMATE
# -----------------
# break down by specific auction and revenue type: advance, remainder, Utility (IOU)
# display spread between $10 and $20
# Auctions: Nov 2012, Feb 2013, May 2013, August 2013, November 2013
LOW_PRICE <- 10
MID_PRICE <- 15
HIGH_PRICE <- 20

dat <- matrix(0, nrow=5, ncol=3, dimnames=list(c("2012-11-01", "2013-02-01", "2013-05-01", "2013-08-01", "2013-11-01"), c("Advance", "Remainder", "Utility (IOU)")))
dat[,"Advance"] <- c(allowance_alloc.tbl["2015", "Advance.Auction"], rep(times=4, allowance_alloc.tbl["2016", "Advance.Auction"] / 4))
dat[,"Remainder"] <- c(NA, rep(times=4, allowance_alloc.tbl["2013", "Remainder"] / 4))
dat[,"Utility (IOU)"] <- c(NA, rep(times=4, allowance_alloc.tbl["2013", "IOU"] / 4))
dat <- rbind(dat, 'Total 2012-13'=c(sum(dat[,"Advance"]), colSums(na.omit(dat[,c("Advance", "Utility (IOU)")]))))

revenue_low <- melt(dat * LOW_PRICE)
revenue_mid <- melt(dat * MID_PRICE)
revenue_high <- melt(dat * HIGH_PRICE)

revenue <- cbind(revenue_low, revenue_mid$value, revenue_high$value)
colnames(revenue) <- c("Auction", "Allowance.Type", "LOW", "MID", "HIGH")

# FY 2012-13 revenue
auction_index <- revenue$Auction %in% c("2012-11-01", "2013-02-01", "2013-05-01")
colSums(revenue[auction_index,])

#revenue$Auction <- as.Date(as.character(revenue$Auction), format="%Y-%m-%d")
auction_labeller <- function(var, value) {
     value <- as.character(value)
     if(var == "Auction") {
          d <- as.Date(value)
          value[!is.na(d)] <- strftime(na.omit(d), format="%b %Y")
     }
     return(value)
}


floor_gg2 <- ggplot(revenue, aes(Allowance.Type, MID, ymin=LOW, ymax=HIGH, group=Allowance.Type, colour=Allowance.Type))
p <- floor_gg2 + geom_pointrange(size=1) + labs(y="Millions of $") + facet_grid(~ Auction, labeller=auction_labeller) +
     opts(axis.text.x=theme_blank(), axis.title.x=theme_text(face="italic"), theme_text(size=14),  
          title="Potential California GHG Auction Revenue 2012-13") +
          xlab("Price range of $10-$20 per allowance. \nThe dot represents $15 / allowance.") 

p

# Save the original definition of the guide_grid
guide_grid_orig <- ggplot2:::guide_grid

# Create the replacement function
guide_grid_no_vline <- function(theme, x.minor, x.major, y.minor, y.major) {  
     x.minor <- setdiff(x.minor, x.major)
     y.minor <- setdiff(y.minor, y.major)
     
     ggname("grill", grobTree(
          theme_render(theme, "panel.background"),
          if(length(y.minor) > 0) theme_render(
               theme, "panel.grid.minor", name = "y",
               x = rep(0:1, length(y.minor)), y = rep(y.minor, each=2), 
               id.lengths = rep(2, length(y.minor))
               ),
          if(length(y.major) > 0) theme_render(
               theme, "panel.grid.major", name = "y",
               x = rep(0:1, length(y.major)), y = rep(y.major, each=2), 
               id.lengths = rep(2, length(y.major))
               )
          ))
}
# Set the environment to be the same as original
environment(guide_grid_no_vline) <- environment(ggplot2:::guide_grid)

# Assign the function inside ggplot2
assignInNamespace("guide_grid", guide_grid_no_vline, ns="ggplot2")

# Draw the graph
p

ggsave("Figures/Auction Revenue 2012-13.jpg", height=5, width=8, units="in")



# Restore the original guide_grid function so that it will draw all gridlines again
assignInNamespace("guide_grid", guide_grid_orig, ns="ggplot2")

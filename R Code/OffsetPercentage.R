# Project: CA Cap-and-Trade Allowances and Revenue
# File: OffsetPercentage
# -----------------
# Estimates the theoretical maximum for offsets
# Offsets are limited to 8% of an emitter's compliance obligation.  § 95854
# In theory, all compliance entities' emissions should not exceed BAU or the cap, whichever is less.
# Offsets are thus limited to 8% of emissions.  
# But because offsets are in addition to the cap, this puts the maximum offsets at either:
#     8% of BAU or
#     8% of emissions, where E = cap / .92
# Creates a chart displaying allowances, offsets and business as usual emissions.

###################
# DEPENDENCIES
###################
source("R Code/BasicAllowanceAllocation.R")

library(ggplot2)

OFFSET_PERCENTAGE <- .08      # § 95854
EXCLUDE <- c("VRE.Reserve", "Allowance.Price.Reserve") # these allowances will likely be unavailable for compliance entities looking to use offsets

# DETERMINE TOTAL ALLOWANCES AVAILABLE BY YEAR
# -----------------
max_allowances<- allowance_alloc.tbl[,"Total.Allowances"] - rowSums(allowance_alloc.tbl[,EXCLUDE])
emissions_with_offsets <- max_allowances / (1-OFFSET_PERCENTAGE) # E = cap / .92

# USE BAU EMISSIONS
# -----------------
# 2020 ghg forecast
# http://www.arb.ca.gov/cc/inventory/data/tables/2020_ghg_emissions_forecast_2010-10-28.pdf
# only looking at the capped sector
# remember the cap expands in 2015
bau_emissions_capped <- c(167.26, 168.07, 406.71, 406.89, 407.61, 407.99, 408.41, 408.84)
names(bau_emissions_capped) <- 2013:2020

reductions_needed <- bau_emissions_capped - max_allowances

# Determine the max emissions: less of bau or allowances available
max_emissions <- ifelse(emissions_with_offsets > bau_emissions_capped, bau_emissions_capped, emissions_with_offsets)

# Offsets are emissions * .08
max_offsets <- OFFSET_PERCENTAGE * max_emissions

message("Total Reductions Required: ", round(sum(reductions_needed)), " MMT CO2e.")
message("Total Offsets in Theory: ", round(sum(max_offsets)))
message("Percentage Offsets of total reductions: ", round(sum(max_offsets) / sum(reductions_needed), 2))

offsets.tbl <- cbind(Allowances.Available=max_allowances, Reductions.Needed=reductions_needed, Max.Emissions=max_emissions, Max.Offsets=max_offsets)
cumsum(offsets.tbl[,"Reductions.Needed"])
cumsum(offsets.tbl[,"Max.Offsets"]) # total offsets exceed reduction needed until 2017

# PLOT ALLOWANCES, OFFSETS AND BAU LINE
# -----------------
dat <- data.frame(MMTCO2e=c(max_allowances, max_offsets), 
                  Allowance.Type=c(rep(times=length(max_allowances), "Available.Allowances"), 
                                   rep(times=length(max_offsets), "Max.Offsets")),
                  Year = as.factor(c(names(max_allowances), names(max_offsets))),
                  BAU_MMT=c(rep(times=2, bau_emissions_capped)))

bau_dat <- data.frame(Year=as.factor(names(bau_emissions_capped)), MMTCO2e=bau_emissions_capped)

allowance_gg <- ggplot(dat, aes(x=Year, y=MMTCO2e))

# bar plot
p <- allowance_gg + geom_bar(dat=dat, aes(fill=Allowance.Type), alpha=2/3) + scale_fill_manual(values=c("dark blue", "light blue"))

# add bau lines
p + geom_line(dat=dat[1:2,], aes(x=as.numeric(Year), y=BAU_MMT), colour="red") + geom_line(dat=dat[3:8,], aes(x=as.numeric(Year), y=BAU_MMT), colour="red") + ylab(expression(paste(MMTCO[2], plain(e)))) + annotate("text", x=6.5, y=420, label="Business as Usual Emissions", size=4, col="red")

ggsave("Figures/Offsets and BAU.jpg", width=7, height=5, units="in")


# PLOT OFFSETS VS REDUCTION NEEDED
# -----------------
dat <- data.frame(MMTCO2e=c(max_offsets, reductions_needed),
                  Allowance.Type=c(rep(times=length(max_offsets), "Max.Offsets"),
                                   rep(times=length(reductions_needed), "Reductions.Needed")),
                  Year=as.factor(c(names(max_offsets), names(reductions_needed))))

reduction_gg <- ggplot(dat, aes(x=Year, y=MMTCO2e))
reduction_gg + geom_bar(dat=dat, aes(fill=Allowance.Type), alpha=2/3, position="dodge") +scale_fill_manual(values=c("light blue", "red")) + ylab(expression(paste(MMTCO[2], plain(e))))

ggsave("Figures/Offsets vs Reductions.jpg")
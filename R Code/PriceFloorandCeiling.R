# Project: CA Cap-and-Trade Allowances and Revenue
# File: Price Floor and Ceiling
# -----------------
# Function to calculate the price floor and ceiling given an estimate of annual cpi
# can take either a single cpi estimate, to be recycled, or a vector corresponding to estimated CPI for each year

ANNUAL_CPI_ESTIMATE <- .02    # example inflation estimate
# ANNUAL_CPI_ESTIMATE <- rnorm(7, mean=.02, sd=.001)   # example vector

# FUNCTION: CalculatePriceLimits
# ---------
# Creates table of price floor and ceiling given cpi estimate
# 2012 through 2020
# If cpi estimate is a vector, must be of length 7 (1 per year from 2014:2020)
# § 95911(b)(6) (auction reserve price / price floor): $ 10 in 2012, 2013, 5% plus inflation thereafter
# § 95913(d) (reserve tier prices / ceiling): 2013: $40 / $45 / $50 then 5% plus inflation thereafter
# ---------
CalculatePriceLimits <- function(cpi_estimate) {
     stopifnot((length(cpi_estimate) == 1 | length(cpi_estimate) == 7))
     
     if(length(cpi_estimate) == 1) cpi_estimate <- rep(times=7, cpi_estimate)
     
     allowance_prices.tbl <- matrix(NA, nrow=8, ncol=4)
     rownames(allowance_prices.tbl) <- 2013:2020
     colnames(allowance_prices.tbl) <- c("Price.Floor", "Reserve.Tier.1", "Reserve.Tier.2", "Reserve.Tier.3")
     
     allowance_prices.tbl["2013",] <- c(10, 40, 45, 50)
     
     for(i in 2:nrow(allowance_prices.tbl)) {
          allowance_prices.tbl[i,] <- allowance_prices.tbl[i-1,] * (1+.05+cpi_estimate[i-1])
     }
     
     # add 2012
     allowance_prices.tbl <- rbind('2012'=c(10, NA, NA, NA), allowance_prices.tbl)
     
     return(allowance_prices.tbl)
}

allowance_prices.tbl <- CalculatePriceLimits(ANNUAL_CPI_ESTIMATE)


# Project: CA Cap-and-Trade Allowances and Revenue
# File: Helper Functions
# -----------------
# Basic functions used to manipulate the data

# FUNCTION: ConvertToPercentage
# --------
# Converts numbers with percentage sign into decimal version
# So 10% becomes 0.10
ConvertToPercentage <- function(ch) {
     ch <- sub("\\(", "-", ch)
     ch <- sub("\\)", "", ch)
     
     if (grepl("%", ch)) {
          # remove percentage sign, divide by 100
          ch <- sub("%", "", ch)
          ch <- as.numeric(ch)
          ch <- ch / 100
     }
     return(ch)         
}
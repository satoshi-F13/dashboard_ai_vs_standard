# load_tips.R
# This file loads the tips dataset for the Shiny application

# Install and load reshape2 if needed
if (!require(reshape2, quietly = TRUE)) {
  install.packages("reshape2")
  library(reshape2)
}

# Load the tips dataset
data(tips, package = "reshape2")

# Verify the dataset structure
if (!all(c("total_bill", "tip", "sex", "smoker", "day", "time", "size") %in% names(tips))) {
  stop("Tips dataset is missing required columns")
}

# The tips dataset is now available in the global environment
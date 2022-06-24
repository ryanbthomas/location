
library(rvest)

sales <- read_html("https://www.redfin.com/neighborhood/547464/CA/Los-Angeles/Central-Hollywood/filter/min-beds=2,min-baths=2,include=sold-6mo")

sales %>%
    html_elements(".HomeStatsV2")
# of beds, baths and sqft

tmp2 <- sales %>%
    html_elements(".homecardV2Price")
# sale price
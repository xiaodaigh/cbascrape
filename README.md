# cbascrape
A CBA transaction scraper in R

# Usage

```r
devtools::install_github("xiaodaigh/cbascrape")
library(cbascrape)

# need you to enter your Netbank ID and password.
# It's not stored anyway and is thrown away after use
cba_transactions = cbascrape::get_cba_trans() 
View(cba_transactions)
```

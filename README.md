# cbascrape
A CBA transaction scraper in R

# Usage
devtools::install_github("xiaodiagh/cbascrape")
library(cbascrape)
# need you to enter your netbank ID and password. It's not stored anyway and is thrown away after use.
cbascrape::get_cba_trans() 

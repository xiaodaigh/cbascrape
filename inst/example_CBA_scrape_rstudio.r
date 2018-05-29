# library(RSelenium)
# library(magrittr)
# library(XML)
# library(stringr)
# library(data.table)
# library(lubridate)
# library(dplyr)
# library(dtplyr)
# library(tidyr)
# library(jsonlite)


# sapply(dir("R"), function(f) source(file.path("R",f)))

library(cbascrape)


cbascrape::get_cba_trans()
# this is your CBA account transactions data
View(res)


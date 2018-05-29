#' Click on an element; if it's not in view try to scroll down t find it until it's found
#' @param elem The element you wish to click
#' @param yloc  The y axis location before it tries to click
#' @param inview_fn A function that returns TRUE if the elem is in view and false otherwise
#' @param iter Number of iterations to ry before giving up
clickElementInView <- function(remDr, elem, yloc = 650, inview_fn = function(elem) elem$getElementLocationInView()$y <= yloc, iter = 10) {
  # remDr$setWindowSize(width = 4000, height = 4000)
  #browser()

  # if((Sys.info())["sysname"] != "Windows") {
  #   elem$clickElement()
  #   return()
  # }

  tries <- 1
  while(tries <= iter) {
    tryCatch({
      elem$clickElement();
      return("click success")
      }, error = function(e) {
      # scroll down if an error occurred
        # browser()
      remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))
    }, finally = {})

    # scroll down
    Sys.sleep(1)
    tries <- tries + 1
  }
  return("too many tries to click")
}

#' Downloads CBA transactions
#' @export
get_cba_trans <- function() {
  pwsd <- data.frame(username = .rs.askForPassword("Your NetBank ID"), password = .rs.askForPassword("Your NetBank Password"))
  a <- cba_scrap(as.character(pwsd$username)  , pwsd$password, rd = 4567L)
  pwsd$password = NULL
  rm(pwsd)
  gc()

  process_cba_dl(a[[2]])
}

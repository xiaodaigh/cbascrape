#' Rename from to to only when from exists
#' @export
rename_file_when_exists <- function(from, to, time_out = 10) {
  # browser()
  tries <- 0
  while(!file.exists(from)) {
    if(tries >= time_out) return("waiting for from")
    tries <- tries + 1
    Sys.sleep(1)
    print("waiting for from")
  }

  fr <- file.rename(
    from,
    to
  )

  tries <- 0
  while(!file.exists(to)  ) {
    if(tries >= time_out) return("waiting for to")
    tries <- tries + 1
    Sys.sleep(1)
    print("waiting for to")
  }

  return(T)
}

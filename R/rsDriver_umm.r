
#' Start a server based on UMM needs
#' @param dl_dir The directory to download any download files to. It defaults to the tempdir()
#' @param headless T/F Whether to run the Chrome browser in headless mode
#' @import RSelenium
rsDriver_umm <- function(port = 4567L,
                         #dl_dir = "c:/temp/",
                         dl_dir = tempdir(),
                         headless = F) {
  # if dl_dir does not exist create it
  if(!dir.exists(dl_dir)) dir.create(dl_dir)

  ecaps_args <- c('--disable-gpu', '--window-size=4000,4000')

  if(headless) {
    ecaps_args <- c('--headless', ecaps_args)
  }

  eCaps <- list(
    chromeOptions =
      list(prefs = list(
        "profile.default_content_settings.popups" = 0L,
        "download.prompt_for_download" = FALSE,
        "download.default_directory" = dl_dir
      )
      ,args = ecaps_args
      )
  )

  rd <- RSelenium::rsDriver(port= port, browser = "chrome",chromever = "latest", extraCapabilities  = eCaps, geckover = NULL, phantomver = NULL,  iedrver = NULL)

  #rd <- RSelenium::rsDriver(browser = "chrome",chromever = "latest", extraCapabilities  = eCaps, geckover = NULL, phantomver = NULL,  iedrver = NULL,port = 3030L)
  # rd <- RSelenium::rsDriver(browser = "phantomjs",chromever = NULL, extraCapabilities  = eCaps, geckover = NULL, phantomver = "2.1.1",  iedrver = NULL)

  return(rd)

}


wait_till_visible <- function(webElem, time_out = 5) {
  for(i in 1:time_out) {
    if(webElem$isElementDisplayed() %>% unlist) {
      return()
    }

    Sys.sleep(1)
  }
  stop(paste0("expected element did not display "))
}

# gethtmljs = "html = document.getElementsByTagName('html')[0];return html.outerHTML;"
#' Login to the CBA homepage
#' @rdname cba_scrap
#' @return TRUE/FALSE returns true if login successful ful
login_cba <- function(remDr, username = .rs.askForPassword("username") %>% as.character, password = .rs.askForPassword("password") %>% as.character) {
  remDr$navigate("https://www.my.commbank.com.au/netbank/Logon/Logon.aspx")
  # login
  remDr$findElement("id", "txtMyClientNumber_field")$sendKeysToElement(list(username))
  remDr$findElement("id", "txtMyPassword_field")$sendKeysToElement(list(password))
  remDr$findElement("id", "btnLogon_field")$clickElement()

  # check if login was successful
  # browser()
  # if I am still at that url then login must have failed
  if(remDr$getCurrentUrl() == "https://www.my.commbank.com.au/netbank/Logon/Logon.aspx") {
   return(F)
  }

  return(T)
}

#' Obtain the list of tables from the first page after successful login
#' @import magrittr
#' @rdname cba_scrap
get_accts_cba <- function(remDr) {
  # get rid of some ugly ul from the table listing before returning the table in HTML
  table_ok <- F
  tryCatch({
    accts_tbl <- remDr$executeScript("
                                   $('table#MyPortfolioGrid1_a').find('ul').remove();
                                   return $('table#MyPortfolioGrid1_a').html()",
                                     args=list("dummy"))
                  # for some reason this version of selenium requires us to add this
    table_ok <- TRUE & length(accts_tbl) >0
  }, finally = {})

  if (!table_ok) {
    tryCatch({
      accts_tbl <- remDr$executeScript("
                                     $('table.FieldPanel.FullGrid.SortedGrid').find('ul').remove();
                                     return $('table.FieldPanel.FullGrid.SortedGrid').html()")
      table_ok <- TRUE
    }, finally = {})
  }





  # get rid of newlines and tabs from the HTML
  acct_tbl_clean <- accts_tbl[1] %>% stringr::str_replace_all("\n","") %>%
    str_replace_all("\t","")

  # add the table tags so that it looks like a table and the read it into a data.frame
  accts_tbla <- readHTMLTable(paste0("<table>",acct_tbl_clean,"</table>"), head = T)[[1]]
  setDT(accts_tbla)

  # use easier to use names
  setnames(accts_tbla, names(accts_tbla), c("nickname_type", "bsb_details", "account_number", "account_balance", "available_funds"))

  # remove the last row which is just a summary
  accts_tblb <- accts_tbla[-nrow(accts_tbla) & !is.na(bsb_details),]
  # get rid of accoutns without any information
  setDT(accts_tblb)

  # there is the Open actions menue etc, get rid of them
  accts_tblb[,new_nickname_type := str_sub(nickname_type,1,
                                           str_locate(nickname_type, "Open actions menu")[,1]-1) %>%
               str_trim(c( "right")) %>%
               str_trim(c( "left"))
             ]
  accts_tblb[,nickname_type := ifelse(is.na(new_nickname_type), nickname_type %>%
                                        str_trim(c( "right")) %>%
                                        str_trim(c( "left")), new_nickname_type)]

  accts_tblb[, new_nickname_type := NULL]

  #browser()
  accts_tblb[,account_balance := account_balance %>% as.character %>%
               str_replace_all("\\$","") %>%
               str_replace_all(",","") %>%
               as.numeric
             ];

  accts_tblb[,available_funds := available_funds %>% as.character %>%
               str_replace_all("\\$","") %>%
               str_replace_all(",","") %>%
               as.numeric
               ];

  # classify the accounts
  accts_tblb[, account_type := "CASA"];
  accts_tblb[bsb_details == "", account_type := "Superannuation"];
  accts_tblb[bsb_details == "View in Portfolio", account_type := "Shares"];

  # print(accts_tblb)
  return(accts_tblb)
}

#' Go into one of the accoutns from the account first page after a succesful login
#' @param acct The account nickname from which I want to download all the accounts
dl_csv_cba <- function(remDr, acct, time_out) {
  #' click on the link
  remDr$findElement(using = "link text", acct)$clickElement()

  # check to see if there's a table
  if (remDr$executeScript("return $('table').length",args=list("dummy")) == 0)  {
    print('this account has no nothing')
    return("No Transaction")
  }

  # download the last 2 years of data by default
  clickElementInView(remDr,remDr$findElement(using = "link text", "Advanced search"))
  # browser()

  sd = Sys.Date()
  warning("this would fail in Feb 29")
  # browser()
  clickElementInView(remDr,remDr$findElement("css", "label#ctl00_BodyPlaceHolder_radioSwitchDateRange_field_1_label"))
  #remDr$screenshot(display = T, useViewer = T)
  #browser()
  remDr$findElement("css", "input#ctl00_BodyPlaceHolder_fromCalTxtBox_field") -> webElem
  #wait_till_visible(webElem, 5);

  remDr$findElement("css", "input#ctl00_BodyPlaceHolder_fromCalTxtBox_field")$sendKeysToElement(list(paste0(c(add0(day(sd)),add0(month(sd)),year(sd)-2),collapse = "/")))
  remDr$findElement("css", "input#ctl00_BodyPlaceHolder_toCalTxtBox_field")$sendKeysToElement(list(paste0(c(add0(day(sd)),add0(month(sd)),year(sd)),collapse = "/")))

  clickElementInView(remDr,remDr$findElement("css", "a#ctl00_BodyPlaceHolder_lbSearch"))
  #remDr$findElement("css", "a#ctl00_BodyPlaceHolder_lbSearch")$clickElement()

  # it is searching and hence we should wait until
  wait_time <- 0
  print("waiting for transactions to load")
  while(remDr$executeScript('return $("a:contains(\'Stop searching\')").is(":visible")',args=list("dummy"))[[1]] == TRUE) {
    Sys.sleep(1)
    wait_time <- wait_time + 1
    if(wait_time > time_out) {
      print("time out waiting for transactions to load")
      remDr$goBack()

      return("Timeout waiting for all transactions to loads")
    }
  }
  print("finished loading transactions")

  # some of the icons seems to be doubled up
  remDr$executeScript("$('.icon-export')[0].remove()", args=list("dummy"))
  # clickElementInView(remDr$findElement("css", "a.toolbarlink.icon-export"))

  remDr$executeScript("$('span.MandatoryField.Width18.field.ddl_select.reserveSpace')[0].remove()", args=list("dummy"))
  # click on the export icon
  #remDr$findElement("css", "a.toolbarlink.icon-export")$clickElement()

  remDr$executeScript(
    'var a = $(".p-grid.customFooter");
    a.addClass("customFooter_active");', args=list("dummy"))
  #remDr$executeScript('$("span.ddl_select_scroll").css("display","block")')


  # show the drop down list
  webelem <- remDr$findElement("css", "span.MandatoryField.Width18.field.ddl_select.reserveSpace")
  webelem$clickElement()

  # browser()
  # selec the csv option
  remDr$findElement("css","li.option1 span.exportFormat")$clickElement()

  # click export
  remDr$findElement(using = "link text", "Export transactions")$clickElement()
  # browser()
  return("Likely Success")
}

#' Scrap from cba
#' @param username Netbank ID. Usually a number such as 12345678
#' @param password Your Netbank password
#' @param time_out time out in seconds
#' @param rd Either pass the result of rsDriver_umm or a port number in which case the server will be started (and hopefully closed) by this function
#' @export
#' @import data.table
#' @import RSelenium
cba_scrap <- function(username = .rs.askForPassword("Your CBA Username"), password = .rs.askForPassword("CBA Password"), rd = 4567L , remDr = NULL, dl_dir = tempdir(), time_out = 60, time_out_load_trans = time_out, time_out_files = time_out, ...) {
  username <- as.character(username)

  if(is.numeric(rd)) {
    rd <- rsDriver_umm(rd, ...)
    remDr <- rd$client
    # if the server is started by this function it will close it down
    on.exit(rd$server$stop(), T)
  } else if (is.null(remDr)) {
    remDr <- rd$client
  }

  dl_file_path <- file.path(dl_dir, "CSVData.csv")

  # ensure username and password are characters
  # browser()

  login_success <- login_cba(remDr, username, password)

  if(login_success == F) {
    return("Login unsuccessful")
  }

  # check if login is successful

  # result password as soon as possible
  password <- NULL; gc()

  accts_tbl <- get_accts_cba(remDr)
  warning("currently only doing CASA; what about credit card?")

  sapply(accts_tbl[account_type == "CASA", nickname_type], function(acct) {
    print("doing " %>% paste0(acct))

    rename_to_file_path <- paste0(tempfile(),".csv")

    if(file.exists(dl_file_path)) {
      # warning("what if it cannot remove the file?")
      file.remove(dl_file_path)

      # wait until the file gets deleted
      sec <- 0
      while(sec <= time_out_files & file.exists(dl_file_path)) {
        Sys.sleep(1)
        sec <- sec + 1
      }

      if(sec >= time_out_files) {
        stop("File still there")
      }
    }

    # this will download the file
    # the default download file name is csvdata.csvf
    dl_status <- dl_csv_cba(remDr, acct, time_out = time_out)

    if(dl_status == "No Transaction") {
      remDr$goBack()
      return(c("No Transaction",""))
    }

    if (file.exists(rename_to_file_path)) {
      file.remove(rename_to_file_path)
    }

    # wait until the file has finished downloading
    res <- rename_file_when_exists(dl_file_path, rename_to_file_path)
    print("finished " %>% paste0(acct))

    remDr$goBack()

    if(res == T) {
      return(c("Likely Success", rename_to_file_path))
    } else {
      return(c(res, rename_to_file_path))
    }

  }) -> dl_results

  # log off
  # browser()
  # remDr$screenshot(display = T, useViewer = T)
  try(remDr$findElement("link text","Log off")$clickElement())

  return(list(accts_tbl, dl_results))
}


process_cba_dl <- function(dl_results) {
  # browser()
  # do some renaming and output them
  mapply(function(file_path, acct_name) {
    if(file_path == "") return()
    tmp <- fread(file_path, header = F)
    names(tmp) <- c("date","value","cba_description", "running_balance")
    #tmp[,nickname := acct_name][,cba_id := username]
    tmp[,nickname := acct_name]
    tmp[,intraday_transaction_order := .N:1, by = date]
    return(tmp)
  }, dl_results[2,], colnames(dl_results), SIMPLIFY = F) %>%
    rbindlist -> a


  # process it
  a[,`:=`(date = as.Date(date,"%d/%m/%Y") %>% {year(.)*10000 + month(.)*100+day(.)},
          value = as.numeric(value),
          running_balance = running_balance %>% str_replace("$","") %>% as.numeric())
    ]

  return(a)
}

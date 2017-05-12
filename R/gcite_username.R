#' @name gcite_username
#' @title Google Citation Username Seracher
#' @description Search Google Citation for an author username
#' @param author author name separated by spaces
#' @param verbose Verbose diagnostic printing
#' @param ask If multiple authors are found, should a menu be given
#' @param secure use https vs. http
#' 
#' @return A character vector of the username of the author
#' @examples
#' \dontrun{
#' gcite_username("John Muschelli")
#' }
#' @export
#' @importFrom rvest html_attr html_text
#' @importFrom utils menu
gcite_username <- function(author,
                           verbose = TRUE,
                           ask = TRUE,
                           secure = TRUE) {
  auth.names <- strsplit(author, " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)],
                      sep = "", collapse = "+")
  ### search the page
  url <- paste0(
    "http", ifelse(secure, "s", ""), "://scholar.google.com/",
    "citations?hl=en&view_op=search_authors&mauthors=",
    auth.names
  )
  
  res = httr::GET(url = url)
  stop_for_status(res)
  doc = httr::content(res)
  
  
  doc = html_nodes(doc, ".gs_scl")
  doc = html_nodes(doc, xpath = '//div[ @class= "gsc_1usr_text"]')
  
  users = html_nodes(doc, xpath = '//h3[ @class = "gsc_1usr_name"]//a')
  hrefs = html_attr(users, "href")
  unames = sapply(hrefs, function(x){
    x = parse_url(x)
    x$query$user
  })
  users = html_text(users)    
  
  L = as_list(doc)
  affils = sapply(L, function(x) {
    nx = names(x)
    x = x[ nx == "div"]
    keep = sapply(x, attr, ".class") == "gsc_1usr_aff"
    x = x[keep]
    x = x$div[[1]]
    if (length(x) == 0) {
      x = NA
    }
    x
  })
  # affils = html_nodes(doc, xpath = '//div[ @class = "gsc_1usr_aff"]')
  # affils = html_text(affils)
  
  
  dat <- data.frame(cbind(
    names = users,
    insts = affils,
    username = unames
  ),
  stringsAsFactors = FALSE)
  rownames(dat) = NULL
  
  dat$fullnames <- paste(dat$names, dat$insts, sep = ": ")
  dat$fullnames[ is.na(dat$insts)] <- dat$names[ is.na(dat$insts)]
  
  if (nrow(dat) > 1) {
    ### if they have someone for a hit
    ##grab the first hit
    if (ask) {
      choice <- menu(dat$fullnames,
                     title = "More than One Author, Please Choose")
      if (choice == 0) {
        return("No Choice given, skipped")        
      }
    } else {
      warning("Multiple authors found, first chosen")
      print(dat)
      choice = 1
    }
  } else {
    choice = 1
  }
  dat$fullnames = NULL
  dat = dat[choice, ]
  if (verbose) {
    print(dat)
  }
  return(dat$username)
}

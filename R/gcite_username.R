#' @name gcite_username
#' @title Google Citation Username Searcher
#' @description Search Google Citation for an author username
#' @param author author name separated by spaces
#' @param verbose Verbose diagnostic printing
#' @param ask If multiple authors are found, should a menu be given
#' @param secure use https vs. http
#' @param ... arguments passed to \code{\link[httr]{GET}}
#' 
#' @return A character vector of the username of the author
#' @examples
#' if (!is_travis() & !is_cran()) {
#' gcite_username("John Muschelli")
#' }
#' @export
#' @importFrom rvest html_attr html_text
#' @importFrom utils menu
gcite_username <- function(
  author,
  verbose = TRUE,
  ask = TRUE,
  secure = TRUE,
  ...) {
  auth.names <- strsplit(author, " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)],
                      sep = "", collapse = "+")
  ### search the page
  url <- paste0(
    "http", ifelse(secure, "s", ""), "://scholar.google.com/",
    "citations?hl=en&view_op=search_authors&mauthors=",
    auth.names
  )
  
  res = httr::GET(url = url, ...)
  httr::stop_for_status(res)
  doc = httr::content(res)
  
  
  # doc = html_nodes(doc, ".gs_scl")
  doc = html_nodes(doc, ".gsc_1usr")
  if (length(doc) == 0) {
    stop(paste0("No names found, see: ", url))
  }
  # doc = html_nodes(doc, xpath = '//div[ @class= "gsc_1usr_text"]')
  
  # users = html_nodes(doc, xpath = '//h3[ @class = "gsc_1usr_name"]//a')
  users = html_nodes(doc, ".gs_ai_name")
  if (length(users) == 0) {
    warning("No users, this may be a bug in gcite due to changing", 
            " Google website, please open an issue at ",
            "https://github.com/muschellij2/gcite/issues")
  }
  # users = html_nodes(doc, ".gs_ai_pho")
  hrefs = html_attr(html_nodes(users, "a"), "href")
  unames = sapply(hrefs, function(x){
    x = parse_url(x)
    x$query$user
  })
  users = html_text(users)    
  
  L = as_list(doc)
  # affils = html_nodes(doc, ".gsc_oai_aff")
  # affils = html_text(affils)
  affils = sapply(L, function(x) {
    nx = names(x)
    x = x[ nx == "div"]
    x = x$div
    keep = sapply(x, attr, ".class") == "gsc_oai_aff"
    x = x[keep]
    if (length(x) == 0) {
      x = NA
    } else {
      x = paste(unlist(x), collapse = "")
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
    if (ask || !interactive()) {
      choice <- menu(dat$fullnames,
                     title = "More than One Author, Please Choose")
      if (choice == 0) {
        message("No Choice given, skipped")
        return(NULL)        
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

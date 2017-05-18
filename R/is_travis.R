#' @title Check if on Travis CI
#' @description Simple check for Travis CI for examples
#'
#' @return Logical if user is named travis
#' @export
#'
#' @examples
#' is_travis()
is_travis = function(){
  users = system("users", intern = TRUE)
  users = trimws(users)
  any(grepl("travis", users))
}
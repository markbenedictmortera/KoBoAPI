#' Retrieve API token
#'
#' @param .auth list containing username, password or token. Only either the token, or the username and password is needed.
#' If both are provided token is used.
#'
#' @return string. The API token
#' @export
#'
#' @examples
#' getToken(list(username = "username", password = "password"))
#'
getToken <- function(.auth) {
  token <- jsonlist::fromJSON(
    httr::content(
      getKoBo(endpoints$token, .auth),
      "text"
      )
    )
  .token <- unname(.token)
}

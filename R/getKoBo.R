#' Make an API call. Wraps the credentials (username, password and token) into httr::GET
#'
#' @param .url can be any URL, for convenience use the object endpoints
#' @param .auth list containing username, password or token. Only either the token, or the username and password is needed.
#' If both are provided token is used.
#'
#' @return results of the API call
#' @details to hide
#' @export
#'
#' @examples
#' endpoints <- list(assets = "https://kf.kobotoolbox.org/api/v2/assets")
#' auth <- list(username = "username" , password = "password")
#' assets <- getKoBo(endpoints$assets, .auth = auth)
#'
#' ##To extract the JSON in the results:
#' assets <- jsonlite::fromJSON(httr::content(assets, "text"))
#'
#' ##You can use the token too
#' auth <- list(token = "Token")
getKoBo <- function(.url,
                    .auth) {
  if(!is.list(.auth)) {
    stop(".auth must be a list of credentials containing either the username and password, or the token")
         }
  if("token"%in%ls(.auth)) {
    .auth <- httr::add_headers(Authorization = paste0("Token ", .auth$token))
  } else if (c("password")%in%ls(.auth) & c("username")%in%ls(.auth)) {
    .auth <- httr::authenticate(.auth$username,
                                .auth$password)
  } else {
    stop(".auth must be a list of credentials containing either the username and password, or the token")
    }
  .data <- httr::GET(.url,
                     .auth
                     )
  return(.data)
}





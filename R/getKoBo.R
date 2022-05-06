#' Make an API call. Wrapper of httr::GET() and httr::authenticate().
#'
#' @param .url can be any URL, for convenience use the object endpoints
#' @param .auth list containing username, password or token
#'
#' @return results of the API call
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
  if(!is.list(.auth)) {".auth must be a list of credentials"}
  if(is.character(auth$token)) {


  }

  .data <- httr::GET(.url,
               httr::authenticate(.auth$username,
                                  .auth$password)
  )
  return(.data)
}




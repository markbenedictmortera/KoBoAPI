#' Download KoBo data
#'
#' @param .id asset id
#' @param .auth list containing username, password or token. Only either the token, or the username and password is needed.
#' If both are provided token is used.
#' @param .version .version the API version to use
#'
#' @return
#' @export
#'
#' @examples
getData <- function(.id, .auth, .version = "v2") {
  if(!methods::hasArg(.id)|!is.character(.id)) {stop(".id must be a string")}
  .url <- switch(.version,
         v1 = paste0(endpoints$data["v1"], .id, ".csv"),
         v2 = paste0(endpoints$data["v2"], .id, "/data")
  )
  getKoBo(.url,
          .auth)
  }

#' Download the xls source file for the questionnaire
#'
#' @param .id asset id of the source xls. This will be used as the file name.
#' @param .auth list containing username, password or token. Only either the token, or the username and password is needed.
#' If both are provided token is used.
#' @param .version the API version to use
#' @return Save the source xls
#' @export
#'
#' @examples
getSource <- function(.id,
                      .auth,
                      .version = "v2") {

  if(!(methods::hasArg(.username)&methods::hasArg(.password))) {
    stop("Username and password is missing")
  }

  if(methods::hasArg(.id)) {
    .assets <- paste0("https://kf.kobotoolbox.org/api/v2/assets", .id, ".xls")
  } else {
    stop("Needs an id or the source URL.")
  }

  writeBin(
    content(
      getKoBo(.assets,
              .username,
              .password),
      "raw")
  )
}

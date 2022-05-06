#' Title
#'
#' @param .id The asset id of the source xls. This will be used as the file name.
#' @param .url The URL for the source xml/xls
#' @param .username the username used to authenticate the API call
#' @param .password the password used to authenticate the API call
#' @return Save the source xls
#' @export
#'
#' @examples
getSource <- function(.id,
                      .username,
                      .password,
                      .server = getOption("kobo_server")) {

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

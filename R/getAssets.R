#' Get/extract list of assets in you KoBo account. Wraps the getKoBo function to obtain the data frame or extract the data frame from the JSON output.
#'
#' @param .data (optional) the JSON output of a prior call to the asset endpoint
#' @param .username username for your KoBo account. If there is no prior call to the asset endpoint you have to make a new call
#' @param .password the password for your KoBo account.
#'
#' @return dataframe containing the: name, asset url, number of submissions, description, tags, deployment status, and the asset type
#' for reference see https://kf.kobotoolbox.org/api/v2/assets in your browser.
#' @export
#'
#' @examples
#' assets_df <- getAssets("https://kf.kobotoolbox.org/api/v2/assets", "username", "password")
getAssets <- function(.data,
                      .username,
                      .password) {
  if(methods::hasArg(.data)) {
    .assets <- .data
  }
  else if (methods::hasArg(.username)&methods::hasArg(.password)) {
    .assets <- getKoBo("https://kf.kobotoolbox.org/api/v2/assets",
                       .auth,
                       .server = getOption("kobo_server"))
  }
  else {
    stop("Please check missing variable. If you want to get an updated list of assets .username and .password is necessary")
  }
  .assets <- jsonlite::fromJSON(
    httr::content(
      .assets, "text")
    )

  .asset_df <-  data.frame(
    name = .assets$results$name,
    id = .assets$results$uid,
    url = .assets$results$url,
    n_submissions = .assets$results$deployment__submission_count,
    description = .assets$results$settings$description,
    tags = .assets$results$tag_string,
    active = .assets$results$deployment__active,
    asset_type = .assets$results$asset_type,
    data_link = .assets$results$data,
    source_link = gsub("/$", ".xls", .assets$results$url)
  )
  print(.asset_df)
}

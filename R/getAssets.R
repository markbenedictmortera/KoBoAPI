#' Get/extract list of assets in you KoBo account. Wraps  function to obtain the data frame or extract the data frame from the JSON output.
#'
#' @param .data (optional) the JSON output of a prior call to the asset endpoint
#' @param .auth list containing username, password or token. Only either the token, or the username and password is needed.
#' If both are provided token is used.
#' @param .version the API version to use
#'
#' @return dataframe containing the: name, asset url, number of submissions, description, tags, deployment status, and the asset type
#' for reference see https://kf.kobotoolbox.org/api/v2/assets in your browser.
#' @export
#'
#' @examples
#' assets_df <- getAssets("https://kf.kobotoolbox.org/api/v2/assets", "username", "password")
getAssets <- function(.data,
                      .auth,
                      .version = "v2") {
  if(methods::hasArg(.data)) {
    .assets <- .data
  } else {
    .assets <- getKoBo(endpoints$assets[.version],
                       .auth)
  }

  .assets <- httr::content(
      .assets,
      "text"
      )

  if(.version == "v1") {
    .asset_df <- jsonlite::fromJSON(
      .assets,
      T
      )
    } else {
      .asset_df <-  data.frame(
        name = .assets$results$name,
        id = .assets$results$uid,
        url = .assets$results$url,
        date_created = .assets$results$date_created,
        date_modified = .assets$results$date_modified,
        owner = .assets$results$owner,
        n_submissions = .assets$results$deployment__submission_count,
        description = .assets$results$settings$description,
        tags = .assets$results$tag_string,
        active = .assets$results$deployment__active,
        asset_type = .assets$results$asset_type,
        data_link = .assets$results$data,
        source_link = gsub("/$", ".xls", .assets$results$url),
        owner = .assets$results$owner__username,
        )
    }
  print(.asset_df)
}

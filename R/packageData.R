packageData <- function(.data, .submission_id = names(.data)[1], .grouping = "auto") {
  if(!hasArg(.payload)|"data.frame"%in%class(.payload)) {
    stop("payload has to be a dataframe of edited segments")
  }

  if(!hasArg(.grouping)) {
    stop("please choose grouping")
  }

  .id_index <- which(names(.data)==.submission_id)
  names(.data)[.id_index] <- "submission_ids"

  n_rows <- nrow(.data)
  n_groups <- sum(
    sapply(
      s[, -.id_index],
      function(x) {
        length(unique(x))
        }
      )
    )

  if(.grouping == "auto") {
    n_rows <- nrow(.data)
    n_groups <- sum(
      sapply(
        s[, -.id_index],
        function(x) {
          length(unique(x))
          }
        )
      )
    .grouping <- switch(n_groups<=n_rows,
                        TRUE = "by_group",
                        FALSE = "by_row"
                        )
  }

  .payload <- list()

  if(.grouping=="row") {
    .item <- for(ii in 1:nrow(.data)) {
      toJSON(
        list(submission_ids = .data$submission_ids[ii],
             data = .data[ii,-.id_index]
        )
      )
      append(.payload, .item)
    }
  }

  .payload
}

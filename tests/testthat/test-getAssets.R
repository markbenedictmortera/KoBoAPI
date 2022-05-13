test_that("Endpoint for assets is reponding", {
  expect_equal(
    httr::status_code(getAssets("https://kf.kobotoolbox.org/api/v2/assets",
                              get
                              )),
    403
  )
})

test_that("Endpoint for assets parses correctly", {
  expect_equal(
    httr::status_code(getAssets("https://kf.kobotoolbox.org/api/v2/assets",
                                getOptions(auth)
    )),
    403
  )
})


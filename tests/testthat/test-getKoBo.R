test_that("API v2 is responding with forbidden code when pinged without authentication", {
  expect_equal(
    httr::status_code(getKoBo("https://kf.kobotoolbox.org/api/v2/assets",
                              "",
                              "")),
    403
  )
})

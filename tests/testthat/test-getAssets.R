test_that("Endpoint for assets is reponding", {
  expect_equal(
    httr::status_code(getKoBo("https://kf.kobotoolbox.org/api/v2/assets",
                              "",
                              "")),
    403
  )
})

test_that("detect_column infers common occurrence columns", {
  names_vec <- c("scientificName", "decimalLongitude", "decimalLatitude", "institutionCode")
  expect_equal(detect_column(names_vec, c("decimal.*lon", "^lon")), "decimalLongitude")
  expect_equal(detect_column(names_vec, c("decimal.*lat", "^lat")), "decimalLatitude")
  expect_equal(detect_column(names_vec, c("institutioncode", "source")), "institutionCode")
  expect_true(is.na(detect_column(names_vec, c("countrycode"))))
})

test_that("clean_occurrences removes invalid coordinates and duplicates", {
  occ <- data.frame(
    species = "Demo species",
    decimalLongitude = c(seq(140, 161), 200, 140),
    decimalLatitude = c(seq(-39, -18), -25, -39),
    institutionCode = c(rep("Museum A", 12), rep("Tiny Source", 10), "Bad", "Museum A"),
    countryCode = "AU",
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(occ, path, row.names = FALSE)

  cleaned <- clean_occurrences(path, min_source_records = 11, merge_small_sources = TRUE)

  expect_equal(nrow(cleaned$occ), 22)
  expect_equal(cleaned$removed_bad_coordinates, 1)
  expect_equal(cleaned$removed_duplicates, 1)
  expect_equal(cleaned$columns$longitude, "decimalLongitude")
  expect_equal(cleaned$columns$latitude, "decimalLatitude")
  expect_true("Other_institutions" %in% cleaned$occ$source)
})

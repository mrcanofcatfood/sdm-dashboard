test_that("validate_extent accepts only valid lon/lat bounds", {
  expect_equal(validate_extent(c(112, 155, -45, -10)), c(112, 155, -45, -10))
  expect_error(validate_extent(c(155, 112, -45, -10)), "invalid bounds")
  expect_error(validate_extent(c(112, 181, -45, -10)), "outside valid")
  expect_error(validate_extent(c(112, 155, -45)), "four numeric")
})

test_that("normalize_threshold preserves valid thresholds", {
  expect_equal(normalize_threshold(0), 0)
  expect_equal(normalize_threshold("0.75"), 0.75)
  expect_equal(normalize_threshold(c(0.25, 0.75)), 0.25)
  expect_error(normalize_threshold(1.1), "between 0 and 1")
  expect_error(normalize_threshold("not-a-number"), "between 0 and 1")
})

test_that("safe_slug creates filesystem-friendly lowercase names", {
  expect_equal(safe_slug("Demo species / Test"), "demo_species_test")
  expect_equal(safe_slug("  A---B___C  "), "a_b_c")
  expect_equal(safe_slug("!!!"), "sdm")
})

test_that("perfect welfare state returns score 1 and se 0, scaled by years", {
  res <- AQALYscore(welfare_state = "1111", number_years = 1)
  expect_equal(res$score, 1)
  expect_equal(res$se, 0)

  res_half <- AQALYscore(welfare_state = "1111", number_years = 0.5)
  expect_equal(res_half$score, 0.5)
  expect_equal(res_half$se, 0)
})

test_that("AQALYscore scales linearly with number_years", {
  r1 <- AQALYscore(welfare_state = "7979", number_years = 1)
  r2 <- AQALYscore(welfare_state = "7979", number_years = 2)

  expect_equal(r2$score, 2 * r1$score)
  expect_equal(r2$se, 2 * r1$se)
})

test_that("AQALYscore returns a list with score and se", {
  res <- AQALYscore(welfare_state = "7979", number_years = 30 / 365)
  expect_type(res, "list")
  expect_named(res, c("score", "se"))
  expect_type(res$score, "double")
  expect_type(res$se, "double")
})

test_that("AQALYscore returns a list with score and se and bounds", {
  res <- AQALYscore(welfare_state = "7979", number_years = 30 / 365, alpha_level = 0.05)
  expect_type(res, "list")
  expect_named(res, c("score", "se", "lower_bound", "upper_bound"))
  expect_type(res$score, "double")
  expect_type(res$se, "double")
  expect_type(res$lower_bound, "double")
  expect_type(res$upper_bound, "double")
})
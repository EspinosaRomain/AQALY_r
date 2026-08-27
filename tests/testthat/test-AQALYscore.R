test_that("perfect welfare state returns score 1 and se 0, scaled by years", {
  res <- AQALYscore(welfare_state_funct = "1111", number_years_funct = 1)
  expect_equal(res$score, 1)
  expect_equal(res$se, 0)

  res_half <- AQALYscore(welfare_state_funct = "1111", number_years_funct = 0.5)
  expect_equal(res_half$score, 0.5)
  expect_equal(res_half$se, 0)
})

test_that("AQALYscore scales linearly with number_years_funct", {
  r1 <- AQALYscore(welfare_state_funct = "7979", number_years_funct = 1)
  r2 <- AQALYscore(welfare_state_funct = "7979", number_years_funct = 2)

  expect_equal(r2$score, 2 * r1$score)
  expect_equal(r2$se, 2 * r1$se)
})

test_that("AQALYscore returns a list with score and se", {
  res <- AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365)
  expect_type(res, "list")
  expect_named(res, c("score", "se"))
  expect_type(res$score, "double")
  expect_type(res$se, "double")
})

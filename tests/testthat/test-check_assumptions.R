test_that("check_assumptions works correctly", {
  # Sample data
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    response = c(rnorm(10, mean = 5, sd = 1),
                 rnorm(10, mean = 6, sd = 1),
                 rnorm(10, mean = 5.5, sd = 1))
  )

  # Run the function and capture the output
  levene_result <- car::leveneTest(data$response ~ data$group)
  shapiro_result <- shapiro.test(data$response)

  # Check if Levene's Test result is a valid object
  expect_true(!is.null(levene_result))
  expect_true(is.numeric(levene_result$`Pr(>F)`[1]))

  # Check if Shapiro-Wilk result is a valid object
  expect_true(!is.null(shapiro_result))
  expect_true(is.numeric(shapiro_result$p.value))

  # Confirm assumptions output valid results
  expect_true(levene_result$`Pr(>F)`[1] >= 0 && levene_result$`Pr(>F)`[1] <= 1)
  expect_true(shapiro_result$p.value >= 0 && shapiro_result$p.value <= 1)
})

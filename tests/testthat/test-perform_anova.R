test_that("perform_anova computes ANOVA correctly", {
  # Sample data
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    response = c(rnorm(10, mean = 5, sd = 1),
                 rnorm(10, mean = 6, sd = 1),
                 rnorm(10, mean = 5.5, sd = 1))
  )

  # Run the ANOVA function
  aov_result <- perform_anova(data, "group", "response")

  # Validate that the result is an object of class "summary.aov"
  expect_true(inherits(aov_result, "summary.aov"))

  # Check that the ANOVA table includes relevant statistics
  df <- as.data.frame(aov_result[[1]])
  expect_true("F value" %in% names(df))
  expect_true("Pr(>F)" %in% names(df))

  # Ensure the p-value is within a valid range
  p_value <- df$`Pr(>F)`[1]
  expect_true(!is.na(p_value))
  expect_true(p_value >= 0 && p_value <= 1)
})

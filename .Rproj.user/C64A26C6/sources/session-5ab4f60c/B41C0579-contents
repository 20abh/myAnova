test_that("generate_anova_table generates ANOVA table correctly", {
  # Sample data
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    response = c(rnorm(10, mean = 5, sd = 1),
                 rnorm(10, mean = 6, sd = 1),
                 rnorm(10, mean = 5.5, sd = 1))
  )

  # Create an ANOVA model
  aov_model <- aov(response ~ group, data = data)

  # Run the generate_anova_table function
  anova_table <- generate_anova_table(aov_model)

  # Validate the structure of the ANOVA table
  expect_true(inherits(anova_table, "summary.aov"))

  # Convert the first element of the table to a data frame for inspection
  anova_df <- as.data.frame(anova_table[[1]])

  # Check for required columns in the ANOVA table
  expect_true("Df" %in% names(anova_df))
  expect_true("Sum Sq" %in% names(anova_df))
  expect_true("Mean Sq" %in% names(anova_df))
  expect_true("F value" %in% names(anova_df))
  expect_true("Pr(>F)" %in% names(anova_df))

  # Validate that p-values are in the correct range
  p_values <- anova_df$`Pr(>F)`
  expect_true(all(p_values >= 0 & p_values <= 1, na.rm = TRUE))
})

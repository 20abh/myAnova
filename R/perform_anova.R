perform_anova <- function(data, group, response) {
  aov_model <- aov(data[[response]] ~ data[[group]])
  summary(aov_model)
}

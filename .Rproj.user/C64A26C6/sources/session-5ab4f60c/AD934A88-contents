check_assumptions <- function(data, group, response) {
  library(car)
  leveneTest(data[[response]] ~ data[[group]])
  shapiro.test(data[[response]])
}

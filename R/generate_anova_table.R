generate_anova_table <- function(aov_model) {
  anova_table <- summary(aov_model)
  return(anova_table)
}

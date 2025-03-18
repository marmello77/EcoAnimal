remover_outliers <- function(df, coluna) {
  Q1 <- quantile(df[[coluna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[coluna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df %>% filter(df[[coluna]] >= (Q1 - 1.5 * IQR) & df[[coluna]] <= (Q3 + 1.5 * IQR))
}
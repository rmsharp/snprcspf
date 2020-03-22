#' Returns mfi_df with each value divided by the appropriate divisors
#'
#' @param mfi_df dataframe with substracted background
#' @param divisors named numeric vector of divisors
#' @export
apply_divisor <- function(mfi_df, divisors) {
  df <- data.frame(row_order = 1:nrow(mfi_df), mfi_df)
  for (name in names(divisors)) {
    df$val[df$name == name] <- 3 * df$val[df$name == name] /
      divisors[[name]]
    df$sd[df$name == name] <- 3 * df$sd[df$name == name] /
      divisors[[name]]
  }
  df <- df[order(df$row_order), -1]
  df
}

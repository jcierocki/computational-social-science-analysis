histogram_grid <- function(
  df,
  n_cols = ifelse(ncol(df) > 4, 3, min(ncol(df), 2)),
  n_rows = ceiling(ncol(df) / n_cols),
  n_bins = 120
) {
  df |> 
    imap(
      ~ ggplot(
          data = df[, .y],
          aes_string(.y)
        ) +
          geom_histogram(aes(y = ..density..), bins = n_bins) +
          stat_function(
            fun = dnorm,
            args = list(
              mean = mean(.x),
              sd = sd(.x)
            ),
            col = "red",
            size = 0.5
          ) + 
          labs(x = NULL, y = NULL, title = .y) +
          theme(plot.title = element_text(hjust = 0.5))
    ) |>
      marrangeGrob(nrow = n_rows, ncol = n_cols, top = NULL)
}

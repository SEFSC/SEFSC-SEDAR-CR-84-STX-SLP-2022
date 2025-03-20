# Function to get quantiles per date
data_quant <- function(label, file_in, probs_vec) {
  # Read in file_in
  read.csv(here::here(file_in)) |>
    assign(label, value = _)

  # Prep file_in
  data_prep <- get(label) |>
    janitor::clean_names(sep_out = ".") |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("x"),
      names_to = "bin",
      names_prefix = "x",
      values_to = "prop",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      bin = as.numeric(bin),
      sex = as.numeric(ds4psy::is_wholenumber(bin)),
      bin = round(bin)
    )

  # Map quantile calculation
  data_quant <- purrr::map(
    probs_vec,
    ~ data_prep |>
      dplyr::group_by(dplyr::across(c(-bin, -prop))) |>
      dplyr::summarize(
        value = cNORM::weighted.quantile(x = bin, weights = prop, probs = .x),
        .groups = "drop"
      ) |>
      dplyr::mutate(quantile = .x)
  ) |>
    purrr::list_rbind()

  data_quant |>
    dplyr::mutate(quantile = paste0("q", quantile)) |>
    tidyr::pivot_wider(
      names_from = "quantile",
      values_from = "value"
    ) |>
    write.csv(here::here(paste0("lenght_quantiles_", label, ".csv")))

  data_quant |>
    dplyr::mutate(
      quantile = as.factor(quantile),
      sex = paste("sex", sex),
      part = 2,
      part = paste("part", part),
      fleet = paste("fleet", fleet)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = year, y = value, lty = quantile)) +
    ggplot2::facet_grid(fleet ~ sex + part) + 
    ggplot2::theme_classic() + 
    ggplot2::labs(
      x = "Year",
      y = "Length",
      title = label,
      subtitle = "Length quantiles by year")
  
  ggplot2::ggsave(
    here::here(paste0("lenght_quantiles_", label, ".png")),
    width = 5, 
    height = 5
  ) 
  
  data_quant
}

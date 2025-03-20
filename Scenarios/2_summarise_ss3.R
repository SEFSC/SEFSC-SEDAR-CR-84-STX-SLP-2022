# Specify pattern of runs (ex: "^84_stx_.*_m2$")
pattern <- "^84_stx_"

# Specify cutout string to match short names in scenarios.csv
cutout <- "84_stx_f3_5cm_010641_0041_"

# Specify parameters of interest
keep_parm <- c("SR_LN(R0)", "SR_sigmaR", "SR_BH_steep",
               "InitF_seas_1_flt_1Com_1", "L_at_Amax_Fem_GP_1",
               "VonBert_K_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1")

# Specify derived quantities of interest
keep_dq <- c("SSB_Virgin", "SSB_Initial", "Recr_Virgin", "Recr_Initial",
             "Dead_Catch_SPR", "SPR_MSY", "Dead_Catch_MSY", "Ret_Catch_MSY",
             "F_2020", "F_2021", "SSB_2022")

# Specify selectivity strata of interest
sizesel_factor <- c("Lsel")
sizesel_strata <- c("Fleet")

# Read in names ####
full_names <- list.files(
  here::here("scenarios"),
  pattern = pattern,
  full.names = TRUE
)

short_names <- list.files(here::here("Scenarios"), pattern = pattern) |>
  stringr::str_remove(cutout)

# Model notes
notes <- read.csv(here::here("Scenarios", "scenarios.csv")) |>
  dplyr::select(scenario, note)

# Read report files
full_data <- full_names |>
  purrr::map(r4ss::SS_output) |>
  rlang::set_names(short_names)

# EXTRACT PARAMETERS AND DERIVED QUANTITIES
parms <- purrr::map(full_data, "parameters") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

dq <- purrr::map(full_data, "derived_quants") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

likelihood <- purrr::map(full_data, "likelihoods_used") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

par_count <- purrr::map_dfr(full_data, "N_estimated_parameters") |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "scenario",
    values_to = "N_estimated_parameters"
  )

warnings <- purrr::map_dfr(full_data, "Nwarnings") |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "scenario",
    values_to = "warnings"
  )

sizeselex <- purrr::map(full_data, "sizeselex") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

# Reformat
short_parm <- parms |>
  dplyr::select(scenario, Label, Value) |>
  dplyr::filter(Label %in% keep_parm | stringr::str_detect(Label, "Size")) |>
  tidyr::pivot_wider(
    id_cols = "scenario",
    names_from = c("Label"),
    values_from = c("Value")
  )

short_dq <- dq |>
  dplyr::select(scenario, Label, Value) |>
  dplyr::filter(Label %in% keep_dq) |>
  tidyr::pivot_wider(
    id_cols = "scenario",
    names_from = c("Label"),
    values_from = c("Value")
  ) |>
  dplyr::mutate(
    SSB_Virgin_lbs =
      round(measurements::conv_unit(SSB_Virgin, "metric_ton", "lbs"), 0),
    SSB_Initial_lbs =
      round(measurements::conv_unit(SSB_Initial, "metric_ton", "lbs"), 0),
    Ret_Catch_MSY_lbs =
      round(measurements::conv_unit(Ret_Catch_MSY, "metric_ton", "lbs"), 0),
    SSB2022_SSB0 = round(SSB_2022 / SSB_Virgin, 4),
    Dead_Catch_SPR_lbs =
      round(measurements::conv_unit(Dead_Catch_SPR, "metric_ton", "lbs"), 0)
  )

short_likelihood <- likelihood |>
  dplyr::select(scenario, Label, values) |>
  tidyr::pivot_wider(
    id_cols = "scenario",
    names_from = c("Label"),
    values_from = c("values")
  )

bins <- names(sizeselex)[grepl("[^a-zA-Z]",  names(sizeselex), perl = TRUE)]

short_sizeselex <- sizeselex |>
  dplyr::select(dplyr::all_of(c(bins, sizesel_strata, "scenario", "Factor"))) |>
  dplyr::filter(Factor %in% sizesel_factor) |>
  dplyr::distinct() |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(bins),
    names_to = "size",
    values_to = "selex"
  ) |>
  dplyr::left_join(notes, by = "scenario") |>
  dplyr::relocate(note, scenario, Factor)

# Combine outputs
run_summary <- notes |>
  dplyr::left_join(short_dq, by = "scenario") |>
  dplyr::left_join(short_parm, by = "scenario") |>
  dplyr::left_join(par_count, by = "scenario") |>
  dplyr::left_join(warnings, by = "scenario") |>
  dplyr::left_join(short_likelihood, by = "scenario")

write.csv(run_summary,
  here::here("Scenarios", "scenarios_summary.csv"),
  row.names = FALSE
)

write.csv(short_sizeselex,
  here::here("Scenarios", "scenarios_size_selex.csv"),
  row.names = FALSE
)

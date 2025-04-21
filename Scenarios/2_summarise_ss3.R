# Specify pattern of runs (ex: "^84_stx_.*_m2$")
pattern <- ".*0041_.*"

# Specify cutout string to match short names in scenarios.csv
cutout <- "84_stx_f3_5cm_010641_0041_"

# Specify parameters of interest
keep_parm <- c(
  "SR_LN(R0)", "SR_sigmaR", "SR_BH_steep",
  "NatM_uniform_Fem_GP_1", "InitF_seas_1_flt_1Com_1",
  "L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1",
  "VonBert_K_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1",
  "Size_DblN_peak_Commercial(1)", "Size_DblN_ascend_se_Commercial(1)"
  )

# Specify derived quantities of interest
keep_dq <- c("SSB_Virgin", "SSB_Initial", "Recr_Virgin", "Recr_Initial",
             "Dead_Catch_SPR", "SPR_MSY", "Dead_Catch_MSY", "Ret_Catch_MSY",
             "annF_SPR", "F_2012", "F_2020", "F_2021", "F_2022", 
             "SSB_SPR", "SSB_2022")

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
catch <- purrr::map(full_data, "catch") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

short_catch <- catch |>
  dplyr::select("scenario", "Fleet_Name", "Yr", "Obs", "Exp", "se")

eq_catch <- short_catch |>
  dplyr::filter(Yr == min(Yr)) |>
  dplyr::select("Fleet_Name", "scenario", "Exp") |>
  dplyr::rename(
    Parameter = Fleet_Name,
    Scenario = scenario,
    Estimate = Exp
  ) |>
  dplyr::mutate(Parameter = paste0(Parameter, "\n Equilibrium Catch"))

parms <- purrr::map(full_data, "parameters") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

dq <- purrr::map(full_data, "derived_quants") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

likelihood <- purrr::map(full_data, "likelihoods_used") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

covar <- purrr::map(full_data, "CoVar") |>
  purrr::map(~ tibble::rownames_to_column(.x, "Label")) |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y))

short_covar <- covar |>
  dplyr::filter(Par..i == "Par",
         Par..j == "Par",
         abs(corr) > 0.9) |>
  dplyr::select(scenario, label.i, label.j, corr) |>
  dplyr::mutate(corr = round(corr, 3)) 

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

est_parm <- parms |>
  dplyr::select(scenario, Label, Status, Phase, Value, Parm_StDev) |>
  dplyr::filter(!is.na(Parm_StDev), 	
                Status != "act") |>
  dplyr::mutate(CV = round(Parm_StDev/Value, 2)) 

est_parm_short <- est_parm |>
  dplyr::select(-Status, -Phase) |>
  dplyr::rename(
    Parameter = Label,
    Scenario = scenario,
    Estimate = Value,
    SD = Parm_StDev,
  ) |>
  tidyr::pivot_longer(
    cols = c("Estimate", "SD", "CV"),
    names_to = "Type",
    values_to = "Value"
  ) |>
  dplyr::mutate(Value = round(Value, 2)) |>
  tidyr::pivot_wider(
    names_from = Type,
    values_from = Value
  )
  
est_dq <- dq |>
  dplyr::select(scenario, Label, Value, StdDev) |>
  dplyr::mutate(CV = round(StdDev/Value, 2))  |>
  dplyr::filter(Label %in% c(
    "SSB_unfished",
    "SSB_Initial",
    "Bratio_2012"
    )
  ) |>
  dplyr::rename(
    Parameter = Label,
    Scenario = scenario,
    Estimate = Value,
    SD = StdDev,
  ) |>
  tidyr::pivot_longer(
    cols = c("Estimate", "SD", "CV"),
    names_to = "Type",
    values_to = "Value"
  ) |>
  dplyr::mutate(Value = round(Value, 2)) |>
  tidyr::pivot_wider(
    names_from = Type,
    values_from = Value
  ) 

est_msy <- dq |>
  dplyr::select(scenario, Label, Value, StdDev) |>
  dplyr::mutate(CV = round(StdDev/Value, 2))  |>
  dplyr::filter(Label == "Dead_Catch_SPR") |>
  dplyr::select(-Label) |>
  dplyr::rename(
    Scenario = scenario,
    Estimate = Value,
    SD = StdDev,
  ) |>
  tidyr::pivot_longer(
    cols = c("Estimate", "SD", "CV"),
    names_to = "Type",
    values_to = "Value"
  ) |>
  dplyr::mutate(Value = round(Value, 2)) |>
  tidyr::pivot_wider(
    names_from = Type,
    values_from = Value
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

print_summary <- run_summary |>
  dplyr::rowwise() |>
  dplyr::mutate(
    SSB2022_SSBSPR = SSB_2022 / SSB_SPR,
    F2022_FSPR = F_2022/ annF_SPR,
    Fcurrent = psych::geometric.mean(c(F_2020, F_2021, F_2022)),
    Fcurrent_FSPR = Fcurrent / annF_SPR
  )

write.csv(print_summary,
  here::here("Scenarios", "scenarios_summary.csv"),
  row.names = FALSE
)

write.csv(short_sizeselex,
  here::here("Scenarios", "scenarios_size_selex.csv"),
  row.names = FALSE
)

write.csv(short_covar,
          here::here("Scenarios", "scenarios_covar.csv"),
          row.names = FALSE
)

write.csv(est_parm_short,
          here::here("Scenarios", "scenarios_parm.csv"),
          row.names = FALSE
)

write.csv(est_dq,
          here::here("Scenarios", "scenarios_dq.csv"),
          row.names = FALSE
)

write.csv(eq_catch, 
          here::here("Scenarios", "eq_catch.csv"),
          row.names = FALSE
)

write.csv(est_msy, 
          here::here("Scenarios", "est_msy.csv"),
          row.names = FALSE
          )

# tbl_dq ####

est_dq <- read.csv(here::here("Scenarios", "scenarios_dq.csv")) |>
  dplyr::mutate(
    Parameter = dplyr::case_when(
      Parameter == "SSB_Initial" ~ "SSB Initial (mt)",
      Parameter == "Bratio_2012" ~ "Ratio SSB Initial:Unfished",
      Parameter == "SSB_unfished" ~ "SSB Unfished (mt)"
    )
  ) |>
  dplyr::select(Parameter, Scenario, Estimate, SD, CV) |>
  dplyr::arrange(desc(Parameter), Scenario) |>
  dplyr::filter(stringr::str_detect(Scenario, "\\w*v7_m3$"))


tbl_dq <- est_dq |>
  dplyr::rename("Derived Quantity" = Parameter) |>
  flextable::flextable() |>
  flextable::autofit() |>
  flextable::theme_box() |>
  flextable::align(align = "center", part = "all") |>
  flextable::merge_v(j = "Derived Quantity") |>
  flextable::fontsize(size = 11, part = "all") |>
  flextable::font(fontname = "Times New Roman", part = "all")

flextable::save_as_image(tbl_dq, here::here("presentation","images","tbl_dq.png"))

# tbl_parm ####

est_parm_short <- read.csv(here::here("Scenarios", "scenarios_parm.csv")) |>
  dplyr::mutate(
    Parameter = dplyr::case_when(
      Parameter == "InitF_seas_1_flt_1Commercial" ~ "Initial F",
      Parameter == "Size_DblN_ascend_se_Commercial(1)" ~ "Commercial Selectivity Asend.",
      Parameter == "SR_LN(R0)" ~ "Unfished Recruitment (R0)",
      Parameter == "Size_DblN_peak_Commercial(1)" ~ "Commercial Selectivity Peak"
    )
  ) |>
  dplyr::select(Parameter, Scenario, Estimate, SD, CV, Gradient) |>
  dplyr::arrange(Parameter, Scenario) |>
  dplyr::filter(stringr::str_detect(Scenario, "\\w*v7_m3$")) |>
  dplyr::mutate(Gradient = format(Gradient, scientific = TRUE, digits = 2))


tbl_parm <- est_parm_short |>
  flextable::flextable() |>
  flextable::autofit() |>
  flextable::theme_box() |>
  flextable::align(align = "center", part = "all") |>
  flextable::merge_v(j = "Parameter") |>
  flextable::fontsize(size = 11, part = "all") |>
  flextable::font(fontname = "Times New Roman", part = "all")

flextable::save_as_image(tbl_parm, here::here("presentation","images","tbl_parm.png"))
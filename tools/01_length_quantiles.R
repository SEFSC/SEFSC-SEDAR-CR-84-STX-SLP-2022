# Source load length_quantile functions
source(here::here("tools", "00_length_quantiles.R"))

# Specify quantiles
probs_vec <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# STX SLP
quant_stx_slp <- data_quant(
  label = "stx_slp_1221_5cm_010641",
  file_in = here::here("data", "length_1221_f3_5cm_0141_fi.csv"),
  probs_vec = probs_vec
)


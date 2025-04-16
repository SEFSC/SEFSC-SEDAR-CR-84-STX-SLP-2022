# Specify pattern of runs (ex: "84_stx", "m2$")
pattern <- "b_m3"

# Specify cutout string to match short names in scenarios.csv
cutout <- "84_stx_f3_5cm_010641_0041_"

# Specify plot_notes comparison labels (e.g., "sensitivity" or "scenario")
note_label <- "scenario"

# Specify sizeselex plot settings
minbthresh <- -1
btarg <- -1
sprtarg <- -1

# Specify output folder name ####
output_dir <- here::here("scenarios", paste0("plots_", pattern))
output_dir_plain <- paste0(output_dir, "_plain")

# Read names 
full_names <- list.files(
  here::here("scenarios"),
  pattern = pattern,
  full.names = TRUE
) |>
  stringr::str_subset(pattern = "plots", negate = TRUE)

# Format short names
short_names <- list.files(here::here("scenarios"), pattern = pattern) |>
  stringr::str_remove(cutout) |>
  stringr::str_subset(pattern = "plots", negate = TRUE)

# Read notes
notes <- read.csv(here::here("scenarios", "scenarios.csv"))

plot_notes <- notes |>
  dplyr::filter(scenario %in% short_names)

# Read report files
full_data <- r4ss::SSgetoutput(dirvec = full_names) |>
  rlang::set_names(short_names) |>
  r4ss::SSsummarize()

# Prep location for plots
if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
dir.create(output_dir)

if (dir.exists(output_dir_plain)) unlink(output_dir, recursive = TRUE)
dir.create(output_dir_plain)

# Plot comparisons
r4ss::SSplotComparisons(
  full_data,
  png = TRUE,
  plotdir = output_dir,
  legendlabels = plot_notes[, note_label],
  sprtarg = sprtarg,
  btarg = btarg,
  minbthresh = minbthresh,
  pwidth = 4, pheight = 3
)

r4ss::SSplotComparisons(
  full_data,
  png = TRUE,
  plotdir = output_dir_plain,
  legend = FALSE,
  sprtarg = sprtarg,
  btarg = btarg,
  minbthresh = minbthresh,
  pwidth = 4, pheight = 3
)

# Read sizeselex
short_sizeselex <- read.csv(
  here::here("Scenarios", "scenarios_size_selex.csv")
) |>
  dplyr::filter(scenario %in% short_names)

# Plot selectivity
plot_sizeselex <- function(
  fleets = unique(short_sizeselex$Fleet),
  scenarios = unique(short_sizeselex$scenario),
  factors = c("Lsel"),
  sizesel_file = "",
  aes_color = "scenario",
  aes_lty = "Factor"
) {

  short_sizeselex |>
    dplyr::filter(
      Fleet %in% fleets,
      scenario %in% scenarios,
      Factor %in% factors,
      !is.na(selex)
    ) |>
    dplyr::mutate(
      size = as.numeric(size),
      selex = as.numeric(selex)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = size,
      y = selex,
      color = .data[[aes_color]],
      lty = .data[[aes_lty]]
    )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Fleet) +
    ggplot2::theme_minimal() 

  ggplot2::ggsave(
    here::here(output_dir, paste0("sizeselex", sizesel_file, ".png")),
    width = 6.5,
    height = 5,
    bg = "white"
  )
}

plot_sizeselex()

plot_sizeselex(
  fleets = c("1"),
  sizesel_file = "_fleet1"
)

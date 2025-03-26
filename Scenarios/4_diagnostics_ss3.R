# Load foreach and dplyr ####
library(foreach)

# Choose scenario for retro, jitter, and profiles  ####
scenario <- here::here("Scenarios", "84_stx_f3_5cm_010641_0041_v7_m2")

# Choose settings ####
overwrite_all <- TRUE
retro_years <- c(0:-5)

jitter_iterations <- 30
jitter_fraction <- 0.2

range_profile_k <- c(seq(.4, 0.95, by = 0.05), 0.99)
range_profile_eqcatch <- seq(0.5, 2, by = 0.1)
range_profile_r0 <- round(log(seq(700, 3000, length = 40)), 4)

# Prep for diagnostics ####

# List files to copy and rename
files_ss_new <- c(
  "control.ss_new",
  "data_echo.ss_new",
  "forecast.ss_new",
  "starter.ss_new",
  "ss3.exe"
)

files_ss <- c(
  "controlfile.ctl",
  "datafile.dat",
  "forecast.ss",
  "starter.ss",
  "ss3.exe"
)

# Name folder paths
dir_dx <- here::here(scenario, "diagnostics")

dir_retro <- here::here(dir_dx, "retro")
dir_retro_plots <- here::here(dir_retro, "plots")
dir_retro_runs <- here::here(dir_retro, "model_runs")

dir_jitter <- here::here(dir_dx, "jitter")
dir_jitter_plots <- here::here(dir_jitter, "plots")
dir_jitter_runs <- here::here(dir_jitter, "model_runs")

dir_profile_r0 <- here::here(dir_dx, "profile_r0")
dir_profile_r0_runs <- here::here(dir_profile_r0, "model_runs")
dir_profile_r0_plots <- here::here(dir_profile_r0, "plots")

dir_profile_eqcatch <- here::here(dir_dx, "profile_eqcatch")
dir_profile_eqcatch_runs <- here::here(dir_profile_eqcatch, "model_runs")
dir_profile_eqcatch_plots <- here::here(dir_profile_eqcatch, "plots")

dir_profile_k <- here::here(dir_dx, "profile_k")
dir_profile_k_runs <- here::here(dir_profile_k, "model_runs")
dir_profile_k_plots <- here::here(dir_profile_k, "plots")

dir_aspm <- here::here(dir_dx, "aspm")

setup_path <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
}

# Set up diagnostic folders
if (overwrite_all == TRUE) setup_path(dir_dx)

# Set up sub folders
sub_dir <- ls(pattern = "dir_")[ls(pattern = "dir_") != "dir_dx"]
if (overwrite_all == TRUE) purrr::map(mget(sub_dir), setup_path)

# Set up only some folders (specify which)
overwrite_some <- c(NA)
if (overwrite_all == FALSE) purrr::map(mget(overwrite_some), setup_path)

# Copy and rename files into diagnostics and jitter folders
file.copy(from = here::here(scenario, files_ss_new),
          to = here::here(dir_dx, files_ss_new))
file.rename(here::here(dir_dx, files_ss_new),
            here::here(dir_dx, files_ss))
file.copy(from = here::here(dir_dx, files_ss),
          to = here::here(dir_jitter_runs, files_ss))

# Set up R0 profile
foreach::foreach(i = seq_along(range_profile_r0)) %do% {
  profile_run <- paste0("r0_", range_profile_r0[i])
  profile_folder <- here::here(dir_profile_r0_runs, profile_run)
  if (dir.exists(profile_folder)) unlink(profile_folder, recursive = TRUE)
  dir.create(profile_folder)

  file.copy(from = here::here(dir_dx, files_ss),
            to = here::here(profile_folder, files_ss))

  start <- r4ss:::SS_readstarter(here::here(profile_folder, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(profile_folder, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(profile_folder, start$ctlfile),
                          datlist = dat, verbose = FALSE)

  ctl$SR_parms[1, 7] <- -1
  ctl$SR_parms[1, 3] <- range_profile_r0[i]

  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(profile_folder, start$ctlfile),
    overwrite = TRUE
  )

  return(NULL)
}

# Set up steepness profile
foreach::foreach(i = seq_along(range_profile_k)) %do% {
  profile_run <- paste0("steepness_", range_profile_k[i])
  profile_folder <- here::here(dir_profile_k_runs, profile_run)
  if (dir.exists(profile_folder)) unlink(profile_folder, recursive = TRUE)
  dir.create(profile_folder)

  file.copy(from = here::here(dir_dx, files_ss),
            to = here::here(profile_folder, files_ss))

  start <- r4ss:::SS_readstarter(here::here(profile_folder, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(profile_folder, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(profile_folder, start$ctlfile),
                          datlist = dat, verbose = FALSE)

  ctl$SR_parms[2, 7] <- -1
  ctl$SR_parms[2, 3] <- range_profile_k[i]

  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(profile_folder, start$ctlfile),
    overwrite = TRUE
  )

  return(NULL)
}

# Set up initial equilibrium catch profile
foreach::foreach(i = seq_along(range_profile_eqcatch)) %do% {
  profile_run <- paste0("eqcatch_", range_profile_eqcatch[i])
  profile_folder <- here::here(dir_profile_eqcatch_runs, profile_run)
  if (dir.exists(profile_folder)) unlink(profile_folder, recursive = TRUE)
  dir.create(profile_folder)

  file.copy(from = here::here(dir_dx, files_ss),
            to = here::here(profile_folder, files_ss))

  start <- r4ss:::SS_readstarter(here::here(profile_folder, "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(profile_folder, start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(profile_folder, start$ctlfile),
                          datlist = dat, verbose = FALSE)

  geomean_3yr <- psych::geometric.mean(dat$catch[2:4, 4])

  dat$catch[1, 4] <- range_profile_eqcatch[i] * geomean_3yr

  r4ss::SS_writedat(
    datlist = dat,
    outfile = here::here(profile_folder, start$datfile),
    overwrite = TRUE,
    verbose = FALSE
  )

  return(NULL)
}

run_folder <- function(run_path) {
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)
  full_names <- list.files(path = here::here(run_path),
                           full.names = TRUE)
  foreach::foreach(i = seq_along(full_names)) %dopar% {
    shell(paste("cd /d ", full_names[i], " && ss3 ", sep = ""))
    return(NULL)
  }
  parallel::stopCluster(cl)
  doParallel::stopImplicitCluster()
}

# Run models ####

# Run profiles
run_folder(dir_profile_k_runs)
run_folder(dir_profile_r0_runs)
run_folder(dir_profile_eqcatch_runs)

# Run retro
r4ss::retro(dir = dir_dx, newsubdir = "retro/model_runs", years = retro_years)

# Run Jitter
r4ss::jitter(
  dir = dir_jitter_runs,
  Njitter = jitter_iterations,
  jitter_fraction = jitter_fraction,
)

# Read reports ####

# Read steepness profile
profile_k_runs <- r4ss::SSgetoutput(
  dirvec = list.files(dir_profile_k_runs, full.names = TRUE)
)

profile_k_summary <- r4ss::SSsummarize(profile_k_runs)

# Read R0 profile
profile_r0_runs <- r4ss::SSgetoutput(
  dirvec = list.files(dir_profile_r0_runs, full.names = TRUE)
)

profile_r0_summary <- r4ss::SSsummarize(profile_r0_runs)

# Read initial equilibrium catch profile
profile_eqcatch_runs <- r4ss::SSgetoutput(
  dirvec = list.files(dir_profile_eqcatch_runs, full.names = TRUE)
)

profile_eqcatch_summary <- r4ss::SSsummarize(profile_eqcatch_runs)

# Read retro
retro_runs <- r4ss::SSgetoutput(
  dirvec = list.files(dir_retro_runs, full.names = TRUE)
)

retro_summary <- r4ss::SSsummarize(retro_runs)

# Read jitter
jitter_runs <- r4ss::SSgetoutput(
  dirvec = dir_jitter_runs,
  getcovar = FALSE,
  keyvec = 1:jitter_iterations
)

jitter_summary <- r4ss::SSsummarize(jitter_runs)



# Do comparisons ####

# Plot retro
r4ss::SSplotComparisons(
  retro_summary,
  plotdir = dir_retro_plots,
  minbthresh = -1,
  endyrvec = retro_summary$endyrs + retro_years,
  png = TRUE,
  legendlabels = paste("Data", retro_years, "years")
)

ss3diags::SSplotRetro(
  retro_summary, 
  subplots = "SSB", 
  print_plot = TRUE, 
  plotdir = dir_retro_plots
)

ss3diags::SSplotRetro(
  retro_summary, 
  subplots = "F", 
  print_plot  = TRUE, 
  plotdir = dir_retro_plots
)

ss3diags::SSplotHCxval(retro_summary, subplots = "cpue", add = TRUE)

retro_summary_comps <- ss3diags::SSretroComps(retro_runs)

ss3diags::SSplotHCxval(
  retro_summary_comps, 
  subplots = "len", 
  add = TRUE
)

# Plot jitter
r4ss::SSplotComparisons(
  jitter_summary,
  png = TRUE,
  plotdir = dir_jitter_plots,
  minbthresh = -1,
  legend = FALSE
)

# Plot steepness profile
r4ss::SSplotComparisons(
  profile_k_summary,
  png = TRUE,
  plotdir = dir_profile_k_plots,
  minbthresh = -1,
  legend = FALSE
)

# Plot R0 profile
r4ss::SSplotComparisons(
  profile_r0_summary,
  png = TRUE,
  plotdir = dir_profile_r0_plots,
  minbthresh = -1,
  legend = FALSE
)

# Plot initial equilibrium catch profile
r4ss::SSplotComparisons(
  profile_eqcatch_summary,
  png = TRUE,
  plotdir = dir_profile_eqcatch_plots,
  minbthresh = -1,
  legend = FALSE
)

# Export tables ####

save_summaries <- function(run_summary, run_dir) {

  likes <- run_summary$likelihoods #Likelihood across runs
  quants <- run_summary$quants #Derived quants across runs
  pars <- run_summary$pars #Estimated parameters across runs

  # Export tables
  write.csv(quants, here::here(run_dir, "summary_quants.csv"),
            row.names = FALSE)
  write.csv(pars, here::here(run_dir, "summary_par.csv"),
            row.names = FALSE)
  write.csv(likes, here::here(run_dir, "summary_likelihoods.csv"),
            row.names = FALSE)

}

save_summaries(jitter_summary, dir_jitter)
save_summaries(retro_summary, dir_retro)
save_summaries(profile_k_summary, dir_profile_k)
save_summaries(profile_r0_summary, dir_profile_r0)
save_summaries(profile_eqcatch_summary, dir_profile_eqcatch)

# Plot jitter likelihoods

# Traditional
jitter_likes <- as.data.frame(reshape::melt(jitter_summary$likelihoods))
jitter_total <- reshape::melt(jitter_summary$likelihoods[1, ])

png(file = here::here(dir_jitter, "Likelihood by Data Source.png"))
lattice::barchart(value ~ Label | Label, group = factor(variable),
                  data = jitter_likes, scales = "free")
dev.off()

png(file = here::here(dir_jitter, "Total Likelihood.png"))
lattice::barchart(value ~ Label | Label, group = factor(variable),
                  data = jitter_total, scales = "free")
dev.off()

# Relativized Jitter plot
limits <- jitter_likes |>
  dplyr::arrange(Label) |>
  dplyr::group_by(Label) |>
  dplyr::summarize(ymin = min(value),
            ymax = max(value),
            diff = ymax - ymin,
            lower = dplyr::case_when(diff < 1 ~ ymin - 0.4,
                              TRUE ~ ymin),
            upper = dplyr::case_when(diff < 1 ~ ymax + 0.4,
                              TRUE ~ ymax))

# Use purrr::map2() to turn two vectors into list of pairs
limit_pairs <- purrr::map2(limits$lower, limits$upper, c)

png(file = here::here(dir_jitter, "Likelihood by Data Source New.png"))
lattice::barchart(
  value ~ Label | Label, group = factor(variable),
  data = jitter_likes,
  scales = "free",
  ylim = limit_pairs,
  par.strip.text = list(cex = 0.8)
)
dev.off()


# Explore selectivity ####

plot_sizeselex <- function(ss_runs, run_dir) {
  sizeselex <- purrr::map(ss_runs, "sizeselex") |>
    purrr::imap_dfr(~ dplyr::mutate(.x, model = .y)) |>
    dplyr::filter(Factor == "Lsel") |>
    dplyr::select(-Yr, -Label, -Factor) |>
    dplyr::distinct()

  write.csv(sizeselex,
            here::here(run_dir, "sizeselex.csv"),
            row.names = FALSE)

  sizeselex_plot <- sizeselex |>
    dplyr::relocate(model, Fleet, Sex) |>
    tidyr::pivot_longer(
      cols = 4:ncol(sizeselex),
      names_to = "size",
      values_to = "selex"
    )

  fleets <- unique(sizeselex_plot$Fleet)
  models <- sizeselex_plot$model

  plotdata <- sizeselex_plot |>
    dplyr::filter(
      Fleet %in% fleets,
      model %in% models,
      !is.na(selex)
    ) |>
    dplyr::mutate(
      model = as.factor(model),
      Fleet = as.factor(Fleet),
      Sex = as.factor(Sex),
      size = as.numeric(size),
      selex = as.numeric(selex)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = size,
      y = selex,
      color = model
    )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Fleet) +
    ggplot2::guides(color = "none")

  ggplot2::ggsave(here::here(run_dir, "sizeselex.png"),
                  width = 6.5, height = 6.5)

  plotdata
}

plot_sizeselex(retro_runs, dir_retro)
plot_sizeselex(profile_r0_runs, dir_profile_r0)
plot_sizeselex(profile_k_runs, dir_profile_k)
plot_sizeselex(profile_eqcatch_runs, dir_profile_eqcatch)

# Jitter selectivity plot with likelihoods
sizeselex <- purrr::map(jitter_runs, "sizeselex") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, model = .y)) |>
  dplyr::filter(Factor == "Lsel") |>
  dplyr::select(-Yr, -Label, -Factor) |>
  dplyr::distinct()

write.csv(sizeselex, here::here(dir_jitter, "sizeselex.csv"), row.names = FALSE)

sizeselex_plot <- sizeselex |>
  dplyr::relocate(model, Fleet, Sex) |>
  tidyr::pivot_longer(
    cols = 4:ncol(sizeselex),
    names_to = "size",
    values_to = "selex"
  )

# Add in likelihood
sizeselex_plot_like <- jitter_likes |>
  dplyr::filter(Label == "TOTAL") |>
  dplyr::rename(model = variable, total = value) |>
  dplyr::right_join(sizeselex_plot, by = dplyr::join_by(model)) |>
  dplyr::mutate(total = as.factor(total))

fleets <- unique(sizeselex_plot_like$Fleet)
models <- sizeselex_plot_like$model

plotdata <- sizeselex_plot_like |>
  dplyr::filter(
    Fleet %in% fleets,
    model %in% models,
    !is.na(selex)
  ) |>
  dplyr::mutate(
    model = as.factor(model),
    Fleet = as.factor(Fleet),
    Sex = as.factor(Sex),
    size = as.numeric(size),
    selex = as.numeric(selex)
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = size,
    y = selex,
    color = model,
    linetype = total
  )) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~Fleet) +
  ggplot2::guides(color = "none")

ggplot2::ggsave(here::here(dir_jitter, "sizeselex.png"),
                width = 6.5, height = 6.5)

plotdata

# Plot likelihoods ####

png(file = here::here(dir_profile_r0, "profile_r0.png"),
    width = 500, height = 500)

r4ss::SSplotProfile(
  profile_r0_summary,
  plot = TRUE,
  profile.string = "R0",
  profile.label = "Virgin Recruitment R0 in log space"
)
dev.off()

png(file = here::here(dir_profile_k, "profile_k.png"),
    width = 500, height = 500)

r4ss::SSplotProfile(
  profile_k_summary,
  plot = TRUE,
  profile.string = "steep",
  profile.label = "Spawner-recruit steepness (h)"
)
dev.off()

png(file = here::here(dir_profile_eqcatch, "profile_eqcatch_f.png"),
    width = 500, height = 500)

r4ss::SSplotProfile(
  profile_eqcatch_summary,
  plot = TRUE,
  profile.string = "InitF_seas_1_flt_1Commercial",
  profile.label = "Initial F",
  legendloc = "topright"
)
dev.off()

eqcatch <- purrr::map(profile_eqcatch_runs, "catch") |>
  purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y)) |>
  dplyr::filter(Yr == min(Yr)) |>
  dplyr::group_by(scenario) |>
  dplyr::summarize(Exp = sum(Exp)) |>
  tidyr::pivot_wider(names_from = scenario, values_from = Exp) |>
  dplyr::mutate(Label = "eqcatch") |>
  data.frame()
  
profile_eqcatch_summary[["pars"]] <-
  eqcatch |>
  dplyr::bind_rows(profile_eqcatch_summary[["pars"]]) 

png(file = here::here(dir_profile_eqcatch, "profile_eqcatch.png"),
    width = 500, height = 500)

r4ss::SSplotProfile(
  profile_eqcatch_summary,
  plot = TRUE,
  profile.string = "eqcatch",
  profile.label = "eqcatch",
  legendloc = "topright"
)
dev.off()

# ASPM ####

aspm_files <- c(
  "data_echo.ss_new", 
  "control.ss_new", 
  "starter.ss", 
  "forecast.ss", 
  "ss3.par", 
  "ss3.exe"
)

file.copy(from = here::here(scenario, aspm_files), 
          to = here::here(dir_aspm, aspm_files))

# Set the recruitment devations in ss3.par to 0
par <- r4ss::SS_readpar_3.30(
  parfile = here::here(dir_aspm, "ss3.par"),
  datsource = here::here(dir_aspm, "data_echo.ss_new"),
  ctlsource = here::here(dir_aspm, "control.ss_new"),
  verbose = FALSE
)

par$recdev1
par$recdev_forecast

# par$recdev1[, "recdev"] <- 0
# par$recdev_forecast[,"recdev"] <- 0
# r4ss::SS_writepar_3.30(
#   parlist = par,
#   outfile = file.path(dir_aspm, "ss3.par"),
#   overwrite = T, verbose = FALSE
# )

# Change the starter file to read from ss3.par
starter <- r4ss::SS_readstarter(file = file.path(dir_aspm, "starter.ss"), verbose = FALSE)

starter$datfile <- "data_echo.ss_new"
starter$ctlfile <- "control.ss_new"

starter$init_values_src <- 1

r4ss::SS_writestarter(starter,
                dir = dir_aspm,
                overwrite = TRUE,
                verbose = FALSE
)

# Change estimation phases 
control <-  r4ss::SS_readctl_3.30(
  file = file.path(dir_aspm, "control.ss_new"),
  datlist = file.path(dir_aspm, "data_echo.ss_new"),
  verbose = FALSE
)

control$SR_parms
# control$SR_parms[c(2,3),"PHASE"] <- c(-4,-4)

control$size_selex_parms
control$size_selex_parms[, "PHASE"] <- abs(control$size_selex_parms[, "PHASE"]) * -1

control$recdev_early_phase
# control$recdev_early_phase <- -4

control$recdev_phase 
# control$recdev_phase <- -2

control$init_F
control$init_F[, "PHASE"] <- -1

# Turn off likelihood components
control$lambdas

# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 
# 7=sizeage; 8=catch; 9=init_equ_catch; # 10=recrdev; 11=parm_prior; 
# 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 
# 17=F_ballpark; 18=initEQregime 
new_lambdas <- data.frame(
  like_comp = c(4, 4, 9, 9),
  fleet = c(1, 2, 1, 2)
) |>
  dplyr::mutate(
    phase = 1,
    value = 1,
    sizefreq_method = 0
  )
new_lambdas

control$lambdas <- new_lambdas
control$N_lambdas <- nrow(new_lambdas)

r4ss::SS_writectl_3.30(control,
                 outfile = file.path(dir_aspm, "control.ss_new"),
                 overwrite = TRUE, verbose = FALSE
)

r4ss::run(dir = dir_aspm, exe = "ss3", skipfinished = FALSE, verbose = FALSE)

aspm_run <- r4ss::SS_output(dir = c(dir_aspm))
plots <- c(1:11, 13:26)
minbthresh <- -1
r4ss::SS_plots(aspm_run, plot = plots, minbthresh = minbthresh)

aspm_compare <- r4ss::SSgetoutput(dirvec = c(
  scenario,
  dir_aspm
), verbose = FALSE)

aspm_summary <- r4ss::SSsummarize(aspm_compare, verbose = FALSE)

r4ss::SSplotComparisons(
  aspm_summary,
  png = TRUE,
  plotdir = here::here(dir_aspm, "plots"),
  minbthresh = -1,
  legendlabels = c("Ref", "ASPM"),
  subplots = c(2, 8, 14), 
  new = F
)

# RUN TEST
report <- r4ss::SS_output(dir = scenario, verbose = FALSE, printstats = FALSE)
ss3diags::SSplotRunstest(report, add = TRUE)
ss3diags::SSrunstest(report)
ss3diags::SSplotRunstest(report, subplots = "len")
ss3diags::SSrunstest(report, quants = "len")

ss3diags::SSplotJABBAres(report)
ss3diags::SSplotJABBAres(report, subplots = "len")

rmse <- ss3diags::SSrmse(report, quants = "cpue")$RMSE
residuals <- ss3diags::SSrmse(report, quants = "cpue")$residuals

rmse
residuals


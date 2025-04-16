library(foreach)

# Specify pattern ####
pattern <- "84_stx_f3_5cm_010641_0041.*v7_m3$"
retro_years <- c(0:-5)
jitter_iterations <- 30

## Get folder names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

full_names

# Choose what to run ####
read_retro <- FALSE
read_jitter <- FALSE
read_profile_r0 <- FALSE
read_profile_eqcatch <- FALSE
read_profile_k <- TRUE
read_aspm <- FALSE
rsme <- FALSE

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
}

# START AUTOMATED RUN ####

foreach::foreach(i = seq_along(full_names)) %do% {
  
  # Name folder paths ####
  scenario <- full_names[i]
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
  dir_aspm_compare <- here::here(dir_aspm, "compare")
  
  # Read in and plot runs
  
  ## Read retro ####
  if(read_retro == TRUE){
    
    retro_runs <- r4ss::SSgetoutput(
      dirvec = list.files(dir_retro_runs, full.names = TRUE)
    )
    
    retro_summary <- r4ss::SSsummarize(retro_runs)
    
    # Plot retro
    r4ss::SSplotComparisons(
      retro_summary,
      plotdir = dir_retro_plots,
      minbthresh = -1,
      endyrvec = retro_summary$endyrs + retro_years,
      png = TRUE,
      legendlabels = paste("Data", retro_years, "years"),
      pwidth = 3.75, pheight = 3.75
    )
    
    ss3diags::SSplotRetro(
      retro_summary, 
      subplots = "SSB", 
      print_plot = TRUE, 
      plotdir = dir_retro_plots,
      pwidth = 3.75, pheight = 3.75
    )
    
    ss3diags::SSplotRetro(
      retro_summary, 
      subplots = "F", 
      print_plot  = TRUE, 
      plotdir = dir_retro_plots,
      pwidth = 3.75, pheight = 3.75
    )
    
    if("indices" %in% names(retro_summary)) {
      ss3diags::SSplotHCxval(
        retro_summary, subplots = "cpue", add = TRUE,
        print_plot = TRUE, 
        plotdir = dir_retro_plots,
        pwidth = 3.75, pheight = 3.75
      )
    }
    
    retro_summary_comps <- ss3diags::SSretroComps(retro_runs)
    
    ss3diags::SSplotHCxval(
      retro_summary_comps, 
      subplots = "len", 
      add = TRUE,
      print_plot = TRUE, 
      plotdir = dir_retro_plots,
      pwidth = 3.75, pheight = 3.75
    )
    
    save_summaries(retro_summary, dir_retro)
    
    plot_sizeselex(retro_runs, dir_retro)
    
  }
  
  ## Read jitter ####
  if(read_jitter == TRUE){
    
    jitter_runs <- r4ss::SSgetoutput(
      dirvec = dir_jitter_runs,
      getcovar = FALSE,
      keyvec = 1:jitter_iterations
    )
    
    jitter_summary <- r4ss::SSsummarize(jitter_runs)
    
    # Plot jitter
    r4ss::SSplotComparisons(
      jitter_summary,
      png = TRUE,
      plotdir = dir_jitter_plots,
      minbthresh = -1,
      legend = FALSE,
      pwidth = 3.25, pheight = 3
    )
    
    save_summaries(jitter_summary, dir_jitter)
    
    # Plot jitter likelihoods

    # Traditional
    jitter_likes <- as.data.frame(reshape::melt(jitter_summary$likelihoods))
    jitter_total <- reshape::melt(jitter_summary$likelihoods[1, ])
    
    png(file = here::here(dir_jitter, "Likelihood_by_Data_Source.png"))
    print(lattice::barchart(value ~ Label | Label, group = factor(variable),
                      data = jitter_likes, scales = "free"))
    dev.off()
    
    png(file = here::here(dir_jitter, "Total_Likelihood.png"))
    print(lattice::barchart(value ~ Label | Label, group = factor(variable),
                      data = jitter_total, scales = "free"))
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
    
    png(file = here::here(dir_jitter, "Likelihood_by_Data_Source_New.png"))
    print(lattice::barchart(
      value ~ Label | Label, group = factor(variable),
      data = jitter_likes,
      scales = "free",
      ylim = limit_pairs,
      par.strip.text = list(cex = 0.8))
    )
    dev.off()
    
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
  }
  
  ## Read r0 profile ####
  if(read_profile_r0 == TRUE){
    
    profile_r0_runs <- r4ss::SSgetoutput(
      dirvec = list.files(dir_profile_r0_runs, full.names = TRUE)
    )
    
    profile_r0_summary <- r4ss::SSsummarize(profile_r0_runs)
    
    # Plot R0 profile
    r4ss::SSplotComparisons(
      profile_r0_summary,
      png = TRUE,
      plotdir = dir_profile_r0_plots,
      minbthresh = -1,
      legend = FALSE,
      pwidth = 4, pheight = 3
    )
    
    save_summaries(profile_r0_summary, dir_profile_r0)
    
    plot_sizeselex(profile_r0_runs, dir_profile_r0)
    
    r4ss::SSplotProfile(
      profile_r0_summary,
      plot = TRUE,
      profile.string = "R0",
      profile.label = "Virgin Recruitment R0 in log space",
      print  = TRUE,
      legendloc = "top",  
      plotdir = dir_profile_r0,
      pwidth = 3.75, pheight = 3.75
    )

  }
  
  ## Read eqcatch profile ####
  if(read_profile_eqcatch == TRUE){
    
    profile_eqcatch_runs <- r4ss::SSgetoutput(
      dirvec = list.files(dir_profile_eqcatch_runs, full.names = TRUE)
    )
    
    profile_eqcatch_summary <- r4ss::SSsummarize(profile_eqcatch_runs)
    
    # Plot initial equilibrium catch profile
    r4ss::SSplotComparisons(
      profile_eqcatch_summary,
      png = TRUE,
      plotdir = dir_profile_eqcatch_plots,
      minbthresh = -1,
      legend = FALSE,
      pwidth = 4, pheight = 3
    )
    
    save_summaries(profile_eqcatch_summary, dir_profile_eqcatch)
    
    plot_sizeselex(profile_eqcatch_runs, dir_profile_eqcatch)
    
    # r4ss::SSplotProfile(
    #   profile_eqcatch_summary,
    #   plot = TRUE,
    #   profile.string = "InitF_seas_1_flt_1Commercial",
    #   profile.label = "Initial F",
    #   legendloc = "topright",
      # print  = TRUE,
      # plotdir = dir_profile_eqcatch,
      # pwidth = 2.5, pheight = 3.25
    # )
    # dev.off()
    
    eqcatch <- purrr::map(profile_eqcatch_runs, "catch") |>
      purrr::imap_dfr(~ dplyr::mutate(.x, scenario = .y)) |>
      dplyr::filter(Yr == min(Yr)) |>
      dplyr::group_by(scenario) |>
      dplyr::summarize(Exp = sum(Exp)) |>
      tidyr::pivot_wider(names_from = scenario, values_from = Exp) |>
      dplyr::mutate(Label = "eqcatch") |>
      data.frame()
    
    profile_eqcatch_summary_new <- profile_eqcatch_summary
    profile_eqcatch_summary_new[["pars"]] <-
      profile_eqcatch_summary[["pars"]] |>
      dplyr::bind_rows(eqcatch) 
    
    r4ss::SSplotProfile(
      profile_eqcatch_summary_new,
      plot = TRUE,
      profile.string = "eqcatch",
      profile.label = "eqcatch",
      legendloc = "topleft",      
      print  = TRUE,
      plotdir = dir_profile_eqcatch,
      pwidth = 3.75, pheight = 3.75
    )

  }
  
  
  ## Read steepness profile ####
  if(read_profile_k == TRUE){
    
    profile_k_runs <- r4ss::SSgetoutput(
      dirvec = list.files(dir_profile_k_runs, full.names = TRUE)
    )
    
    profile_k_summary <- r4ss::SSsummarize(profile_k_runs)
    
    r4ss::SSplotComparisons(
      profile_k_summary,
      png = TRUE,
      plotdir = dir_profile_k_plots,
      minbthresh = -1,
      legend = FALSE,
      pwidth = 4, pheight = 3
    )
    
    save_summaries(profile_k_summary, dir_profile_k)
    
    plot_sizeselex(profile_k_runs, dir_profile_k)
    
    r4ss::SSplotProfile(
      profile_k_summary,
      plot = TRUE,
      profile.string = "steep",
      profile.label = "Spawner-recruit steepness (h)",
      legendloc = "top",  
      print  = TRUE,
      plotdir = dir_profile_k,
      pwidth = 3.75, pheight = 3.75
    )

  }
  
  ## Read aspm ####
  if(read_aspm == TRUE){
    
    aspm_run <- r4ss::SS_output(dir = c(dir_aspm))
    plots <- c(1:11, 13:26)
    minbthresh <- -1
    r4ss::SS_plots(aspm_run, plot = plots, minbthresh = minbthresh,
                   pwidth = 4, pheight = 3, pheight_tall = 4)
    
    aspm_compare <- r4ss::SSgetoutput(dirvec = c(
      scenario,
      dir_aspm
    ), verbose = FALSE)
    
    aspm_summary <- r4ss::SSsummarize(aspm_compare, verbose = FALSE)
    
    minbthresh <- -1
    btarg <- -1
    sprtarg <- -1
    
    r4ss::SSplotComparisons(
      aspm_summary,
      png = TRUE,
      plotdir = dir_aspm_compare,
      legendlabels = c("Ref", "ASPM"),
      subplots= c(1, 4:13),
      sprtarg = sprtarg,
      btarg = btarg,
      minbthresh = minbthresh,
      pwidth = 4, pheight = 3,
      uncertainty=FALSE
    )
    
  }

  if(rsme == TRUE){
    # RMSE ####
    report <- r4ss::SS_output(dir = scenario, verbose = FALSE, printstats = FALSE)
    
    ss3diags::SSplotRunstest(
      report, subplots = "len", print_plot = TRUE, plotdir = dir_dx)
    
    ss3diags::SSrunstest(report, quants = "len")
    
    ss3diags::SSplotJABBAres(
      report, subplots = "len", print_plot = TRUE, plotdir = dir_dx)
    
    if("cpue" %in% names(report)) {
      ss3diags::SSplotRunstest(report, add = TRUE, , print_plot = TRUE, plotdir = dir_dx)
      
      ss3diags::SSrunstest(report)
      
      ss3diags::SSplotJABBAres(report, print_plot = TRUE, plotdir = dir_dx)
      
      rmse <- ss3diags::SSrmse(report, quants = "cpue")$RMSE
      
      residuals <- ss3diags::SSrmse(report, quants = "cpue")$residuals
      
      rmse
      residuals
    }
  }

}
library(foreach)

# Specify pattern ####
pattern <- "84_stx_f3_5cm_010641_0041.*m3$"

## Get folder names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

full_names

## Choose settings ####
retro_years <- c(0:-5)
jitter_iterations <- 30
jitter_fraction <- 0.2
range_profile_k <- c(seq(.4, 0.95, by = 0.05), 0.99)
range_profile_eqcatch <- seq(0.5, 4, by = 0.1)
range_profile_r0 <- round(log(seq(200, 600, length = 40)), 4)

# Choose what to run ####
run_retro <- TRUE
run_jitter <- TRUE
run_profile_r0 <- TRUE
run_profile_eqcatch <- TRUE
run_profile_k <- TRUE
run_aspm <- FALSE
reset_all <- FALSE

# START AUTOMATED RUN ####

foreach::foreach(i = seq_along(full_names)) %do% {
  
  ## List files to copy and rename ####
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
  
  setup_path <- function(dir) {
    if (dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
    dir.create(dir)
  }
  
  ## Set up diagnostic folder ####
  if (!dir.exists(dir_dx) == TRUE) {
    dir.create(dir_dx)
    file.copy(from = here::here(scenario, files_ss_new),
              to = here::here(dir_dx, files_ss_new))
    file.rename(here::here(dir_dx, files_ss_new),
                here::here(dir_dx, files_ss))
  }
  
  if (reset_all == TRUE) {
    setup_path(dir_dx)
    file.copy(from = here::here(scenario, files_ss_new),
              to = here::here(dir_dx, files_ss_new))
    file.rename(here::here(dir_dx, files_ss_new),
                here::here(dir_dx, files_ss))
  }
  
  ## Set up sub folders ####
  retro_dir <- ls(pattern = "dir_retro")
  jitter_dir <- ls(pattern = "dir_jitter")
  profile_r0_dir <- ls(pattern = "dir_profile_r0")
  profile_eqcatch_dir <- ls(pattern = "dir_profile_eqcatch")
  profile_k_dir <- ls(pattern = "dir_profile_k")
  aspm_dir <- ls(pattern = "dir_aspm")
  
  # Function to run all sub folders in a folder
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
  
  # Run Diagnostics ####
  
  ## Retro ####
  if (run_retro == TRUE) {
    
    # Create dir
    purrr::map(mget(retro_dir), setup_path)
    
    # Run
    r4ss::retro(dir = dir_dx, newsubdir = "retro/model_runs", years = retro_years)
  }
  
  ## Jitter ####
  if (run_jitter == TRUE) {
    
    # Create dir
    purrr::map(mget(jitter_dir), setup_path)
    
    # Set up files
    file.copy(from = here::here(dir_dx, files_ss),
              to = here::here(dir_jitter_runs, files_ss))
    
    # Run
    r4ss::jitter(
      dir = dir_jitter_runs,
      Njitter = jitter_iterations,
      jitter_fraction = jitter_fraction,
    )
  }
  
  ## R0 profile ####
  if (run_profile_r0 == TRUE) {
    
    # Create dir
    purrr::map(mget(profile_r0_dir), setup_path) 
    
    # Set up files
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
    
    # Run
    run_folder(dir_profile_r0_runs)
  }
  
  ## Initial equilibrium catch profile ####
  if (run_profile_eqcatch == TRUE) {
    
    # Create dir
    purrr::map(mget(profile_eqcatch_dir), setup_path) 
    
    # Set up files
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
    
    # Run 
    run_folder(dir_profile_eqcatch_runs)
  }
  
  ## Steepness profile ####
  if (run_profile_k == TRUE) {
    
    # Create dir
    purrr::map(mget(profile_k_dir), setup_path) 
    
    # Set up files
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
    
    # Run
    run_folder(dir_profile_k_runs)
  }
  
  if (run_aspm == TRUE) {
    purrr::map(mget(aspm_dir), setup_path) 
    
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
    
  }

}
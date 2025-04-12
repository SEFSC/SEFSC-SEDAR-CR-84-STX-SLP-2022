library(foreach)

# Specify pattern
pattern <- "84_stx_f3_5cm_010641_0041_"

# Get folder names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

foreach::foreach(i = seq_along(full_names)) %do% {

  start <- r4ss:::SS_readstarter(here::here(full_names[i], "starter.ss"),
                                 verbose = FALSE)
  dat <- r4ss:::SS_readdat(file = here::here(full_names[i], start$datfile),
                           version = 3.3, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file = here::here(full_names[i], start$ctlfile),
                          datlist = dat, verbose = FALSE)
  
  # Update age
  dat$Nages
  dat$Nages = 20
  
  # Update M
  # ctl$MG_parms
  # ctl$MG_parms[1,3]
  # ctl$MG_parms[1,3] = 0.27
  # ctl$MG_parms[13,3]
  # ctl$MG_parms[13,3] = 0.27
  
  # ctl$size_selex_parms[1, 1]
  # ctl$size_selex_parms[1, 1] = 15
  # ctl$size_selex_parms[1, 2]
  # ctl$size_selex_parms[1, 2] = 35
  # ctl$size_selex_parms[1, 3]
  # ctl$size_selex_parms[1, 3] = 25
  
  # ctl$size_selex_parms[3, 1]
  # ctl$size_selex_parms[3, 1] = -2
  # ctl$size_selex_parms[3, 2]
  # ctl$size_selex_parms[3, 2] = 6
  # ctl$size_selex_parms[3, 3]
  # ctl$size_selex_parms[3, 3] = 2
  
  # ctl$SR_parms[1, 1]
  # ctl$SR_parms[1, 1] = 4
  # ctl$SR_parms[1, 2] 
  # ctl$SR_parms[1, 2] = 7
  # ctl$SR_parms[1, 3]  
  # ctl$SR_parms[1, 3] = 5
  
  # ctl$init_F[1, 1]
  # ctl$init_F[1, 1] = 0
  # ctl$init_F[1, 2]
  # ctl$init_F[1, 2] = 4
  # ctl$init_F[1, 3]
  # ctl$init_F[1, 3] = 1.2
  
  r4ss::SS_writedat(
    datlist = dat,
    outfile = here::here(full_names[i], start$datfile),
    overwrite = TRUE,
    verbose = FALSE
  )

  
  r4ss::SS_writectl(
    ctllist = ctl,
    outfile = here::here(full_names[i], start$ctlfile),
    overwrite = TRUE
  )
  
  return(NULL)
}
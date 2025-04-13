# Setup ####

setup_path <- function(dir) {
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
}

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

setup_ss3 <- function(dir) {
file.copy(from = here::here(scenario, files_ss_new),
          to = here::here(dir, files_ss_new))
file.rename(here::here(dir, files_ss_new),
            here::here(dir, files_ss))
}

# Choose scenario  ####
scenario <- here::here("Scenarios", "84_stx_f3_5cm_010641_0041_v7_m2")

#_s1 higher CV on catch ####

dir_s1 <- paste0(scenario, "_s1")
setup_path(dir_s1)
setup_ss3(dir_s1)

start <- r4ss:::SS_readstarter(here::here(dir_s1, "starter.ss"),
                               verbose = FALSE)
dat <- r4ss:::SS_readdat(file = here::here(dir_s1, start$datfile),
                         version = 3.3, verbose = FALSE)

dat$catch[,5]
dat$catch[,5] = 0.3
dat$catch[,5]

r4ss::SS_writedat(
  datlist = dat,
  outfile = here::here(dir_s1, start$datfile),
  overwrite = TRUE,
  verbose = FALSE
)


#_s2 higher CV on growth young ####

dir_s2 <- paste0(scenario, "_s2")
setup_path(dir_s2)
setup_ss3(dir_s2)

start <- r4ss:::SS_readstarter(here::here(dir_s2, "starter.ss"),
                               verbose = FALSE)
dat <- r4ss:::SS_readdat(file = here::here(dir_s2, start$datfile),
                         version = 3.3, verbose = FALSE)
ctl <- r4ss::SS_readctl(file = here::here(dir_s2, start$ctlfile),
                        datlist = dat, verbose = FALSE)
ctl$MG_parms[5,3]
ctl$MG_parms[5,3] = 0.25
ctl$MG_parms[5,3]
ctl$MG_parms[17,3]
ctl$MG_parms[17,3] = 0.25
ctl$MG_parms[17,3]

r4ss::SS_writectl(
  ctllist = ctl,
  outfile = here::here(dir_s2, start$ctlfile),
  overwrite = TRUE
)

  
#_s3 higher CV on growth young and old ####

dir_s3 <- paste0(scenario, "_s3")
setup_path(dir_s3)
setup_ss3(dir_s3)

start <- r4ss:::SS_readstarter(here::here(dir_s3, "starter.ss"),
                               verbose = FALSE)
dat <- r4ss:::SS_readdat(file = here::here(dir_s3, start$datfile),
                         version = 3.3, verbose = FALSE)

ctl <- r4ss::SS_readctl(file = here::here(dir_s3, start$ctlfile),
                        datlist = dat, verbose = FALSE)

rownames(ctl$MG_parms)[5]
ctl$MG_parms[5,3]
ctl$MG_parms[5,3] = 0.25
ctl$MG_parms[5,3]

rownames(ctl$MG_parms)[17]
ctl$MG_parms[17,3]
ctl$MG_parms[17,3] = 0.25
ctl$MG_parms[17,3]

rownames(ctl$MG_parms)[6]
ctl$MG_parms[6,3] 
ctl$MG_parms[6,3] = 0.25
ctl$MG_parms[6,3] 

rownames(ctl$MG_parms)[18]
ctl$MG_parms[18,3] 
ctl$MG_parms[18,3] = 0.25
ctl$MG_parms[18,3] 

r4ss::SS_writectl(
  ctllist = ctl,
  outfile = here::here(dir_s3, start$ctlfile),
  overwrite = TRUE
)
  
  
#_s4 higher age and lower m ####

dir_s4 <- paste0(scenario, "_s4")
setup_path(dir_s4)
setup_ss3(dir_s4)

start <- r4ss:::SS_readstarter(here::here(dir_s4, "starter.ss"),
                               verbose = FALSE)
dat <- r4ss:::SS_readdat(file = here::here(dir_s4, start$datfile),
                         version = 3.3, verbose = FALSE)
ctl <- r4ss::SS_readctl(file = here::here(dir_s4, start$ctlfile),
                        datlist = dat, verbose = FALSE)

dat$Nages
dat$Nages = 30
dat$Nages

dat$N_agebins
dat$N_agebins = dat$Nages
dat$N_agebins

dat$agebin_vector
dat$agebin_vector = c(0:(dat$Nages-1))
dat$agebin_vector

dat$ageerror
dim(dat$ageerror)
dat$ageerror = cbind(
  dat$ageerror, dat$ageerror[,1:(dat$Nages-dim(dat$ageerror)[2]+1)] 
)
dat$ageerror
dim(dat$ageerror)

ctl$MG_parms[1,3] 
ctl$MG_parms[1,3] = 0.18
ctl$MG_parms[1,3] 
ctl$MG_parms[13,3] 
ctl$MG_parms[13,3] = 0.18
ctl$MG_parms[13,3]

r4ss::SS_writedat(
  datlist = dat,
  outfile = here::here(dir_s4, start$datfile),
  overwrite = TRUE,
  verbose = FALSE
)

r4ss::SS_writectl(
  ctllist = ctl,
  outfile = here::here(dir_s4, start$ctlfile),
  overwrite = TRUE
)  

#_s5 higher age and lower m ####

dir_s5 <- paste0(scenario, "_s5")
setup_path(dir_s5)
setup_ss3(dir_s5)

start <- r4ss:::SS_readstarter(here::here(dir_s5, "starter.ss"),
                               verbose = FALSE)
dat <- r4ss:::SS_readdat(file = here::here(dir_s5, start$datfile),
                         version = 3.3, verbose = FALSE)
ctl <- r4ss::SS_readctl(file = here::here(dir_s5, start$ctlfile),
                        datlist = dat, verbose = FALSE)

dat$Nages
dat$Nages = 12
dat$Nages

dat$N_agebins
dat$N_agebins = dat$Nages
dat$N_agebins

dat$agebin_vector
dat$agebin_vector = c(0:(dat$Nages-1))
dat$agebin_vector

dat$ageerror
dim(dat$ageerror)
dat$ageerror = dat$ageerror[,1:(dat$Nages+1)] 
dat$ageerror
dim(dat$ageerror)

ctl$MG_parms[1,3] 
ctl$MG_parms[1,3] = 0.45
ctl$MG_parms[1,3] 
ctl$MG_parms[13,3] 
ctl$MG_parms[13,3] = 0.45
ctl$MG_parms[13,3]

ctl$SR_parms[1,2]
ctl$SR_parms[1,2] = 9
ctl$SR_parms[1,2]

r4ss::SS_writedat(
  datlist = dat,
  outfile = here::here(dir_s5, start$datfile),
  overwrite = TRUE,
  verbose = FALSE
)

r4ss::SS_writectl(
  ctllist = ctl,
  outfile = here::here(dir_s5, start$ctlfile),
  overwrite = TRUE
)  

#_s6 set up for Hermaphroditism incorporated into fecundity  ####

dir_s6 <- paste0(scenario, "_s6")
setup_path(dir_s6)
setup_ss3(dir_s6)

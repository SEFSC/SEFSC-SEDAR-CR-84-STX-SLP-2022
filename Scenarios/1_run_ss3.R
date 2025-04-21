# Build a summary of file structure for awareness
# fs::dir_tree(
#   path = here::here(),
#   type = "directory"
# )

# Specify pattern and plots (plot 12 fails when using seasonal recruitment)
pattern <- "84_stx.*_s6"
plots <- c(1:11, 13:26)
minbthresh <- -1

# Get file names ####
full_names <- list.files(
  path = here::here("Scenarios"),
  pattern = pattern,
  full.names = TRUE
)

 # full_names <- c(here::here("Scenarios",
 #                            "archive",
 #                            "84_stx_f3_5cm_010641_0041_v1_m2_s1"))

# View file names
full_names

# Create function (line 15) then run
runplot(plots = plots, minbthresh = minbthresh)

# Function to run ss3 and r4ss::SS_plots
runplot <- function(plots, minbthresh) {

  # Specify ss3.exe
  ss3_file <- list.files(
    path = here::here("Scenarios"),
    pattern = "ss3.exe",
    full.names = TRUE
  )

  # Copy in excel executable
  file.copy(
    from = ss3_file,
    to = file.path(full_names, "ss3.exe"),
    overwrite = TRUE
  )

  library(foreach)

  # Run models and plots
  i <- NULL
  foreach(i = seq_along(full_names)) %do% {
    model_run <- full_names[i]
    shell(paste("cd /d ", model_run, " && ss3 ", sep = ""))
    myreplist <- r4ss::SS_output(dir = model_run)
    if (dir.exists(paste0(model_run, "/plots"))) {
      unlink(paste0(model_run, "/plots"), recursive = TRUE)
    }
    r4ss::SS_plots(replist = myreplist, plot = plots, minbthresh = minbthresh, pwidth = 4, pheight = 3, pheight_tall = 4)
  }
}

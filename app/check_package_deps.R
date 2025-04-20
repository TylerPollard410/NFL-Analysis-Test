#!/usr/bin/env Rscript

# -----------------------------------------------------------------------------
# check_deps.R
# Scans your Shiny app files and reports all packages actually used.
# -----------------------------------------------------------------------------

# 1) Install & load attachment if needed
# if (!requireNamespace("attachment", quietly = TRUE)) {
#   install.packages("attachment", repos = "https://cloud.r-project.org")
# }
library(attachment)

# 2) Define all the files you want to scan
setwd("app")
app_files <- c(
  "ui.R",
  "server.R",
  # all modules/helpers under R/
  list.files("R", pattern = "\\.R$", full.names = TRUE)
)

# 3) Run the dependency scanner
#    This returns a data.frame with columns: file, call, package, function
deps_df <- att_from_rscripts(
  app_files
  #which = c("imports", "suggests")  # only real imports by default
)

# 4) Extract the unique package names
pkgs_used <- sort(unique(deps_df$package))

# 5) Print to console
cat("Packages actually used in your app:\n")
cat(paste0(" - ", pkgs_used), sep = "\n")

# 6) Save to a text file for review
writeLines(pkgs_used, con = "pack_deps.txt")

cat("\nWrote pack_deps.txt — review this list and then remove/comment any "
    ,"unused library() calls in ui.R/server.R/global.R.\n")

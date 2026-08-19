############################################################
## build_site.R  – Build the entire website
## Author: Kate Schneider Lecy
##
## Usage (from R in the project root):
##   source("build_site.R")
##
## This script will:
##   1) (Optionally) install required packages  [ONE-TIME SETUP]
##   2) Copy the newest CV PDF from your Dropbox CV folder into cv/
##   3) Regenerate publications.qmd from data/publications.csv,
##      refreshing citation counts from Google Scholar (best-effort)
##   4) Render the Quarto website
##   5) Stage the changes in git (commit/push are left to you, or
##      set auto_commit / auto_push below)
############################################################

#### 0. ONE-TIME SETUP: install packages ----------------------------------

one_time_install <- FALSE  # set to TRUE the first time you run this

if (isTRUE(one_time_install)) {
  install.packages(c("scholar", "dplyr", "stringr", "glue", "readr"))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(glue)
  library(readr)
})

#### 1. Copy the newest CV PDF from your Dropbox CV folder ----------------
## Update this path if your CV project folder ever moves.

cv_source_dir <- file.path(
  Sys.getenv("USERPROFILE"),
  "Dropbox", "Professional Records", "CV"
)

cv_dest <- file.path("cv", "KSL_CV.pdf")

cv_candidates <- list.files(cv_source_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
cv_candidates <- cv_candidates[!grepl("^~\\$", basename(cv_candidates))]  # skip Office lock files

if (length(cv_candidates) == 0) {
  warning("No PDF found in ", cv_source_dir, " — keeping existing cv/KSL_CV.pdf.")
} else {
  newest_cv <- cv_candidates[which.max(file.info(cv_candidates)$mtime)]
  if (!dir.exists("cv")) dir.create("cv")
  file.copy(newest_cv, cv_dest, overwrite = TRUE)
  message("Copied CV from ", newest_cv, " -> ", cv_dest)
}

#### 2. Regenerate publications.qmd ----------------------------------------

source("R/generate_publications.R")

#### 3. Render the Quarto website -------------------------------------------
## Uses the Quarto CLI on your PATH; if you only have the copy bundled
## with RStudio, install the standalone CLI from https://quarto.org or
## point QUARTO_PATH at RStudio's bundled quarto.exe.

quarto_bin <- Sys.which("quarto")
if (quarto_bin == "") {
  rstudio_quarto <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
  if (file.exists(rstudio_quarto)) quarto_bin <- rstudio_quarto
}

if (quarto_bin == "") {
  stop("Quarto CLI not found. Install from https://quarto.org or run `quarto render` yourself.")
}

message("Rendering Quarto website ...")
status <- system(glue::glue('"{quarto_bin}" render'))

if (status != 0) {
  stop("quarto render failed. Check the console output above for details.")
}

message("Site build complete.")

#### 4. Stage changes in git (commit/push are opt-in) ------------------------

auto_commit <- FALSE  # set TRUE to auto-commit staged changes
auto_push   <- FALSE  # set TRUE to auto-push after auto_commit (only takes effect if auto_commit is TRUE)

system("git add -A")

if (isTRUE(auto_commit)) {
  commit_msg <- glue::glue("Rebuild site: {format(Sys.Date())}")
  system(glue::glue('git commit -m "{commit_msg}"'))
  message("Committed: ", commit_msg)
  if (isTRUE(auto_push)) {
    system("git push")
    message("Pushed to remote.")
  }
} else {
  message("Changes staged with `git add -A`. Review with `git status` / `git diff --staged`, ",
          "then commit and push when you're ready to publish.")
}

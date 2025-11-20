############################################################
## build_site.R  – Build the entire website
## Author: Kate Schneider Lecy
##
## Usage (from R in the project root):
##   source("build_site.R")
##
## This script will:
##   1) (Optionally) install required packages  [ONE-TIME SETUP]
##   2) Generate publications.qmd from Google Scholar
##   3) Render the Quarto website (quarto render)
############################################################

#### 0. ONE-TIME SETUP: install packages (run once, then comment out) ----
## After the first successful run, you can safely comment this section out.

one_time_install <- TRUE  # set to TRUE the first time you run this

if (isTRUE(one_time_install)) {
  install.packages(c(
    "scholar",
    "dplyr",
    "stringr",
    "glue",
    "readr"
  ))
  # If you use renv, you might also run renv::snapshot() afterward.
}

#### 1. Load required packages -------------------------------------------

suppressPackageStartupMessages({
  library(scholar)
  library(dplyr)
  library(stringr)
  library(glue)
  library(readr)
})
renv::snapshot()

# Quarto is usually called as a system command; if you want the R
# interface (optional), you can also:
# install.packages("quarto")  # ONE-TIME SETUP, then comment out
# library(quarto)


#### 2. ONE-TIME SETUP: set your Google Scholar ID -----------------------
## Find your ID at:
##   https://scholar.google.com/citations?user=lWGTfD4AAAAJ&hl=en
## The part after user= is your Scholar ID.
##
## Set it once here. You won't need to change this unless your profile changes.

scholar_id <- "lWGTfD4AAAAJ&hl"  # <-- ONE-TIME SETUP


#### 3. Function to build publications.qmd from Google Scholar -----------

build_publications_qmd <- function(scholar_id,
                                   outfile = "publications.qmd",
                                   name_for_header = "Kate Schneider Lecy") {
  
  message("Fetching publications from Google Scholar for ID: ", scholar_id)
  
  pubs_raw <- get_publications(id = scholar_id, sortby = "year")
  
  if (nrow(pubs_raw) == 0) {
    stop("No publications returned from Google Scholar. ",
         "Check your scholar_id or your internet connection.")
  }
  
  pubs <- pubs_raw %>%
    mutate(
      year    = suppressWarnings(as.integer(year)),
      journal = na_if(journal, "")
    ) %>%
    arrange(desc(year), desc(cites))
  
  message("Building ", outfile, " ...")
  
  lines <- c(
    "---",
    'title: "Publications"',
    "---",
    "",
    "# Publications",
    "",
    glue("Below is a list of publications for **{name_for_header}** "),
    "as indexed on Google Scholar.",
    "",
    "## Peer-reviewed articles",
    ""
  )
  
  for (i in seq_len(nrow(pubs))) {
    p <- pubs[i, ]
    
    authors <- p$author
    title   <- p$title
    journal <- p$journal
    year    <- p$year
    cites   <- p$cites
    pubid   <- p$pubid
    
    # Direct link to the article details on Google Scholar
    gs_url <- glue(
      "https://scholar.google.com/citations?view_op=view_citation&hl=en",
      "&user={scholar_id}&citation_for_view={scholar_id}:{pubid}"
    )
    
    entry <- glue(
      "- {authors} ({year}). *{title}*",
      "{if (!is.na(journal)) glue('. _{journal}_') else ''}. ",
      "Cited {cites} times on [Google Scholar]({gs_url})."
    )
    
    lines <- c(lines, entry)
  }
  
  write_lines(lines, outfile)
  
  message("Wrote ", outfile, " with ", nrow(pubs), " entries.")
}


#### 4. Generate publications.qmd ----------------------------------------

build_publications_qmd(
  scholar_id       = scholar_id,
  outfile          = "publications.qmd",
  name_for_header  = "Kate Schneider Lecy"
)


#### 5. Render the Quarto website ----------------------------------------
## Assumes:
##   - You have a _quarto.yml file in this directory.
##   - Your site is set up as a Quarto website project.
##
## If Quarto CLI is installed, this will call it directly.
## ONE-TIME SETUP: install Quarto from https://quarto.org

message("Rendering Quarto website ...")

# Option A: call Quarto via system command (most common)
status <- system("quarto render")

if (status != 0) {
  stop("quarto render failed. Check the console output above for details.")
}

message("Site build complete. Commit and push changes to deploy to GitHub Pages.")

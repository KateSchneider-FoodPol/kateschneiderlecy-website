############################################################
## generate_publications.R
## Builds publications.qmd from data/publications.csv, the
## hand-curated source of truth for titles, DOIs, data links,
## and research-theme tags.
##
## Optionally refreshes Google Scholar citation counts for
## journal articles (best-effort; a Scholar failure never
## breaks the build) and writes any Scholar entries that don't
## match an existing row to data/scholar_unmatched.csv so they
## can be triaged and added to publications.csv by hand.
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(glue)
  library(readr)
})

PROJECT_INFO <- list(
  "equitable-access" = list(
    label = "Equitable Access to Healthy & Sustainable Diets",
    anchor = "research-equitable-access.qmd"
  ),
  "food-systems-transformation" = list(
    label = "Food Systems Transformation",
    anchor = "research-food-systems-transformation.qmd"
  ),
  "economics-data-gaps" = list(
    label = "Economics of Agrifood Systems",
    anchor = "research-economics-data-gaps.qmd"
  )
)

normalize_title <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_squish()
}

#### 1. Load curated publications ----------------------------------------

pubs <- read_csv("data/publications.csv", col_types = cols(.default = "c")) %>%
  mutate(year_num = suppressWarnings(as.integer(year)))

#### 2. Best-effort Google Scholar citation refresh -----------------------
## Wrapped so any Scholar failure (rate limiting, network, API changes)
## never breaks the site build.

scholar_id <- "lWGTfD4AAAAJ"
cite_lookup <- NULL

try({
  suppressPackageStartupMessages(library(scholar))
  gs <- get_publications(id = scholar_id, sortby = "year")
  if (nrow(gs) > 0) {
    gs <- gs %>%
      mutate(norm_title = normalize_title(title))

    pubs <- pubs %>%
      mutate(norm_title = normalize_title(title)) %>%
      left_join(gs %>% select(norm_title, cites), by = "norm_title") %>%
      select(-norm_title)

    matched_titles <- unique(normalize_title(pubs$title))
    unmatched <- gs %>%
      filter(!norm_title %in% matched_titles) %>%
      select(title, author, journal, year, cites, pubid)

    if (nrow(unmatched) > 0) {
      write_csv(unmatched, "data/scholar_unmatched.csv")
      message(
        nrow(unmatched),
        " Google Scholar entries did not match data/publications.csv. ",
        "Review data/scholar_unmatched.csv and add any real new ",
        "publications to data/publications.csv by hand."
      )
    } else {
      if (file.exists("data/scholar_unmatched.csv")) file.remove("data/scholar_unmatched.csv")
    }
  }
}, silent = TRUE)

if (!"cites" %in% names(pubs)) pubs$cites <- NA_character_

#### 3. Formatting helpers -------------------------------------------------

fmt_links <- function(row) {
  links <- c()
  if (!is.na(row$doi_url) && nzchar(row$doi_url)) {
    links <- c(links, glue("[Article]({row$doi_url})"))
  }
  if (!is.na(row$data_url) && nzchar(row$data_url)) {
    label <- if (!is.na(row$data_label) && nzchar(row$data_label)) row$data_label else "Data & replication code"
    links <- c(links, glue("[{label}]({row$data_url})"))
  }
  if (!is.na(row$extra_url) && nzchar(row$extra_url)) {
    label <- if (!is.na(row$extra_label) && nzchar(row$extra_label)) row$extra_label else "Link"
    links <- c(links, glue("[{label}]({row$extra_url})"))
  }
  if (!is.na(row$extra2_url) && nzchar(row$extra2_url)) {
    label <- if (!is.na(row$extra2_label) && nzchar(row$extra2_label)) row$extra2_label else "Link"
    links <- c(links, glue("[{label}]({row$extra2_url})"))
  }
  if (length(links) == 0) return("")
  glue(" &mdash; {paste(links, collapse = ' &middot; ')}")
}

fmt_theme <- function(row) {
  proj <- row$project
  if (is.na(proj) || !nzchar(proj) || proj == "legacy" || !proj %in% names(PROJECT_INFO)) return("")
  info <- PROJECT_INFO[[proj]]
  glue(" &nbsp;·&nbsp; _Theme:_ [{info$label}]({info$anchor})")
}

fmt_cites <- function(row) {
  if (is.na(row$cites) || !nzchar(row$cites)) return("")
  glue(" (cited {row$cites}&times; on [Google Scholar](https://scholar.google.com/citations?hl=en&user={scholar_id}))")
}

fmt_entry <- function(row, show_theme = TRUE, number = NA) {
  year_txt <- if (!is.na(row$year) && nzchar(row$year)) glue("({row$year}) ") else ""
  num_txt <- if (!is.na(number)) glue("**{number}.** ") else ""
  glue(
    "- {num_txt}{row$authors} {year_txt}\"{row$title}.\" _{row$venue}_.",
    "{fmt_cites(row)}{fmt_links(row)}{if (show_theme) fmt_theme(row) else ''}"
  )
}

order_by_year <- function(df) {
  yr <- df$year_num
  yr[is.na(yr)] <- 0L
  df[order(-yr), ]
}

## Numbers descending from n (top / most recent) to 1 (bottom / oldest),
## so the top entry's number always shows the section's total count.
## Wrapped in a div so CSS can hide the bullet marker without affecting
## other bulleted lists on the site.
section <- function(df, heading, level = "##") {
  if (nrow(df) == 0) return(character(0))
  df <- order_by_year(df)
  n <- nrow(df)
  c(
    "", glue("{level} {heading}"), "",
    "::: {.numbered-pubs}", "",
    sapply(seq_len(n), function(i) fmt_entry(df[i, ], number = n - i + 1)),
    "", ":::"
  )
}

#### 4. Build publications.qmd ---------------------------------------------

lines <- c(
  "---",
  'title: "Publications"',
  "---",
  "",
  "Publications are grouped by type and tagged with the [research theme](research.qmd) they belong to.",
  "For the complete, continuously updated list see my [Google Scholar profile](https://scholar.google.com/citations?hl=en&user=lWGTfD4AAAAJ).",
  "Data repositories are also mirrored on [GitHub](https://github.com/KateSchneider-FoodPol) and",
  "[Harvard Dataverse](https://dataverse.harvard.edu/dataverse/Kate_Schneider).",
  section(pubs %>% filter(type == "journal-article"), "Journal Articles"),
  section(pubs %>% filter(type == "book-chapter"), "Book Chapters"),
  section(pubs %>% filter(type %in% c("working-paper", "report")), "Working Papers, Policy Briefs & Reports"),
  section(pubs %>% filter(type == "dissertation"), "Dissertation"),
  "",
  "## Earlier Policy Research (2009–2011)",
  "",
  "<details><summary>Policy briefs and technical reports from my pre-doctoral work with the <a href=\"https://epar.evans.uw.edu/\">Evans School Policy Analysis and Research Group (EPAR)</a> at the University of Washington</summary>",
  "",
  { legacy_df <- order_by_year(pubs %>% filter(type == "legacy"));
    sapply(seq_len(nrow(legacy_df)), function(i) fmt_entry(legacy_df[i, ])) },
  "",
  "</details>"
)

write_lines(lines, "publications.qmd")
message("Wrote publications.qmd with ", nrow(pubs), " entries.")

#### 5. Per-theme peer-reviewed publication lists for research.qmd --------
## These are included into research.qmd via {{< include >}} so the research
## page's "Peer-reviewed publications" lists stay in sync with
## data/publications.csv automatically, instead of being hand-duplicated.

if (!dir.exists("_generated")) dir.create("_generated")

peer_reviewed <- pubs %>% filter(type %in% c("journal-article", "book-chapter"))

for (proj in names(PROJECT_INFO)) {
  df <- order_by_year(peer_reviewed %>% filter(project == proj))
  frag_lines <- if (nrow(df) == 0) {
    "*No peer-reviewed publications tagged to this theme yet.*"
  } else {
    sapply(seq_len(nrow(df)), function(i) fmt_entry(df[i, ], show_theme = FALSE))
  }
  write_lines(frag_lines, glue("_generated/pubs-{proj}.md"))
}

message("Wrote per-theme publication fragments to _generated/.")

suppressPackageStartupMessages({
  library(ffanalytics)
  library(dplyr); library(readr); library(stringr); library(tidyr)
})

# Compute week & season (simple rolling; you can override later)
wk <- as.integer(format(Sys.Date(), "%U")) %% 18 + 1
yr <- as.integer(format(Sys.Date(), "%Y"))

# Sources (free/public)
src <- c("FantasyPros","ESPN","Yahoo","RotoWire","FFA")
positions <- c("QB","RB","WR","TE","K","DST")

get_pos <- function(pos){
  dat <- ffanalytics::scrape_projections(src, positions = pos, week = wk, season = yr)
  dat %>%
    transmute(
      player = stringr::str_squish(Player),
      pos    = toupper(Position),
      team   = toupper(Team),
      proj_std  = coalesce(`Std`, Points, Proj, `Fantasy Points`, NA_real_),
      proj_half = coalesce(`Half`, `Half PPR`, NA_real_, proj_std),
      proj_ppr  = coalesce(`PPR`, `Proj PPR`, Points, NA_real_, proj_std),
      source    = Source
    )
}

proj <- dplyr::bind_rows(lapply(positions, get_pos))

dir.create("exports", showWarnings = FALSE, recursive = TRUE)
for(s in unique(proj$source)){
  out <- dplyr::filter(proj, source == s)
  readr::write_csv(out, file.path("exports", paste0("proj_", tolower(s), "_week_", wk, ".csv")))
}
readr::write_csv(proj, file.path("exports", paste0("proj_all_week_", wk, ".csv")))

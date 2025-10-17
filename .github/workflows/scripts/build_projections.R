suppressPackageStartupMessages({
  library(ffanalytics)
  library(dplyr); library(readr); library(stringr); library(tidyr)
})

# Derive the NFL week (auto-detect based on the calendar)
wk <- as.integer(format(Sys.Date(), "%U")) %% 18 + 1
yr <- as.integer(format(Sys.Date(), "%Y"))

# List of free projection sources
src <- c("FantasyPros","ESPN","Yahoo","RotoWire","FFA")

# Positions to scrape
positions <- c("QB","RB","WR","TE","K","DST")

get_pos <- function(pos){
  # Scrape data for this position, all sources
  dat <- ffanalytics::scrape_projections(src, positions = pos, week = wk, season = yr)

  # Normalize the data columns so your Google Sheet can read them
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

# Combine all positions into one dataset
proj <- bind_rows(lapply(positions, get_pos))

# Create an /exports folder and write each CSV
dir.create("exports", showWarnings = FALSE, recursive = TRUE)

for(s in unique(proj$source)){
  out <- dplyr::filter(proj, source == s)
  readr::write_csv(out, file.path("exports", paste0("proj_", tolower(s), "_week_", wk, ".csv")))
}
readr::write_csv(proj, file.path("exports", paste0("proj_all_week_", wk, ".csv")))

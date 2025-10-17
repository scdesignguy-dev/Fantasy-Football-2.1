# scripts/build_projections.R
suppressPackageStartupMessages({
  library(ffanalytics)
  library(dplyr); library(readr); library(stringr); library(tidyr)
})

# --- Week & season (simple rolling week) ---
wk <- as.integer(format(Sys.Date(), "%U")) %% 18 + 1
yr <- as.integer(format(Sys.Date(), "%Y"))

# --- Sources & positions ---
src <- c("FantasyPros","ESPN","Yahoo","RotoWire","FFA")
positions <- c("QB","RB","WR","TE","K","DST")

# 1) SCRAPE raw projections from multiple sources/positions
scr <- ffanalytics::scrape_data(
  src = src,
  pos = positions,
  season = yr,
  week = wk
)  # returns a list of tibbles (one per position) with raw stats per source
# ref: scrape_data docs. :contentReference[oaicite:1]{index=1}

# 2) Define three scoring sets: STD (0 PPR), HALF (0.5), PPR (1.0)
# Start from package defaults, then modify receptions weight.
# ref: default scoring + how to change "rec" group. :contentReference[oaicite:2]{index=2}
sc_std  <- ffanalytics::scoring
sc_half <- ffanalytics::scoring
sc_ppr  <- ffanalytics::scoring

# ensure the rec group exists and set per-reception points
sc_std$rec$rec  <- 0.0
sc_half$rec$rec <- 0.5
sc_ppr$rec$rec  <- 1.0

# 3) Build projections tables for each scoring flavor
# projections_table returns a table of players with projected points (averaged across sources)
# ref: projections_table docs. :contentReference[oaicite:3]{index=3}
tab_std  <- ffanalytics::projections_table(scr, scoring_rules = sc_std)
tab_half <- ffanalytics::projections_table(scr, scoring_rules = sc_half)
tab_ppr  <- ffanalytics::projections_table(scr, scoring_rules = sc_ppr)

# 4) Select common columns and rename points
pick_cols <- function(tb){
  tb %>%
    transmute(
      player = stringr::str_squish(Player),
      pos    = toupper(Position),
      team   = toupper(Team),
      points = Points  # this column name is provided by projections_table
    )
}

std  <- pick_cols(tab_std)  %>% rename(proj_std  = points)
half <- pick_cols(tab_half) %>% rename(proj_half = points)
ppr  <- pick_cols(tab_ppr)  %>% rename(proj_ppr  = points)

# 5) Join into one table (consensus per player/pos/team)
proj <- std %>%
  full_join(half, by = c("player","pos","team")) %>%
  full_join(ppr,  by = c("player","pos","team")) %>%
  mutate(
    proj_std  = round(proj_std,  3),
    proj_half = round(coalesce(proj_half, proj_ppr, proj_std), 3),
    proj_ppr  = round(coalesce(proj_ppr,  proj_half, proj_std), 3)
  )

# 6) Write a single combined CSV (your Google Sheet will read this)
dir.create("exports", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(proj, file.path("exports", paste0("proj_all_week_", wk, ".csv")))

# scripts/build_projections.R
suppressPackageStartupMessages({
  library(ffanalytics)
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr)
})

# --- Week & season (simple rolling week) ---
wk <- as.integer(format(Sys.Date(), "%U")) %% 18 + 1
yr <- as.integer(format(Sys.Date(), "%Y"))

# --- Sources & positions ---
src_all   <- c("FantasyPros","ESPN","Yahoo","RotoWire","FFA")
pos_skill <- c("QB","RB","WR","TE")
pos_other <- c("K","DST")  # we'll skip these in projections_table to avoid VOR errors

# 1) SCRAPE raw projections for ALL positions
scr_all <- ffanalytics::scrape_data(
  src = src_all,
  pos = c(pos_skill, pos_other),
  season = yr,
  week = wk
)
# scrape_data returns a named list of tibbles by position

# 2) Define three scoring sets: STD/HALF/PPR (modify receptions weight)
sc_std  <- ffanalytics::scoring
sc_half <- ffanalytics::scoring
sc_ppr  <- ffanalytics::scoring
sc_std$rec$rec  <- 0.0
sc_half$rec$rec <- 0.5
sc_ppr$rec$rec  <- 1.0

# Helper: run projections_table safely on a subset of positions
safe_proj_table <- function(scr_list, scoring_rules){
  # Keep only skill positions to avoid DST baseline/VOR errors
  scr_skill <- scr_list[names(scr_list) %in% pos_skill]
  if (length(scr_skill) == 0) return(tibble())
  ffanalytics::projections_table(scr_skill, scoring_rules = scoring_rules)
}

tab_std  <- safe_proj_table(scr_all, sc_std)
tab_half <- safe_proj_table(scr_all, sc_half)
tab_ppr  <- safe_proj_table(scr_all, sc_ppr)

# 3) Select common columns and rename points
pick_cols <- function(tb){
  tb %>%
    transmute(
      player = stringr::str_squish(Player),
      pos    = toupper(Position),
      team   = toupper(Team),
      points = Points
    )
}

std  <- pick_cols(tab_std)  %>% rename(proj_std  = points)
half <- pick_cols(tab_half) %>% rename(proj_half = points)
ppr  <- pick_cols(tab_ppr)  %>% rename(proj_ppr  = points)

# 4) Join into one table (consensus per player/pos/team) — skill positions only
proj_skill <- std %>%
  full_join(half, by = c("player","pos","team")) %>%
  full_join(ppr,  by = c("player","pos","team")) %>%
  mutate(
    proj_std  = round(proj_std,  3),
    proj_half = round(coalesce(proj_half, proj_ppr, proj_std), 3),
    proj_ppr  = round(coalesce(proj_ppr,  proj_half, proj_std), 3)
  )

# (Optional later) We can add a fallback to compute K/DST from a single source.
# For now, leave them out to keep the build robust.

# 5) Write the combined CSV
dir.create("exports", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(proj_skill, file.path("exports", paste0("proj_all_week_", wk, ".csv")))

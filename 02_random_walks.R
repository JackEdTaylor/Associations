library(tidyverse)

# import data
swow <- read_csv("00_swow_edited.csv")

take_walks <- function(cue_node, n_walks = 5, alpha = .95, .swow = swow, assoc_weight = "norm_ppmi", direction = "forwards", cues_only = TRUE) {
  
  # NOTE: assoc_weight should be the column that reflects association strength (i.e. "ppmi", "norm_ppmi", or "R123.Strength")
  
  # if direction is "forwards", cue to response, if "backwards", response to cue
  
  library(purrr)
  
  # if cues_only is TRUE, responses are removed if they are never used as a cue
  # setting cues_only to FALSE will cause walks to terminate at non-cue responses, such that all subsequent steps in that walk will return NA
  if (cues_only) .swow <- dplyr::filter(.swow, response %in% cue)
  
  # alpha = probability at any step on a walk taking another step
  cat(sprintf("Calculating walk lengths for alpha of %s...", alpha))
  walk_lengths <- lapply(1:n_walks, function(walk_nr) {
    decayed <- FALSE
    walk_length <- 0
    while(!decayed) {
      decayed <- base::sample(c(TRUE, FALSE), 1, prob = c(1 - alpha, alpha))
      walk_length <- walk_length + 1
    }
    walk_length
  })
  cat(sprintf("  -done\n"))
  
  # function to take one step in a random walk
  take_step <- function(node_i, step_nr) {
    if (is.na(node_i)) {
      NA
    } else {
      # get all responses to this cue
      cue_dat <- filter(.swow, cue == node_i)
      # get the next step
      if (nrow(cue_dat) > 0) {
        cue_dat %>%
          dplyr::sample_n(1, weight = !!dplyr::sym(assoc_weight)) %>%
          dplyr::pull(response)
      } else {
        NA
      }
    }
  }
  
  cat(sprintf("Taking %i walks...", n_walks))
  out <- purrr::map(walk_lengths, ~ purrr::accumulate(1:., take_step, .init = cue_node))
  cat(sprintf("  -done\n"))
  
  out
  
}

take_walks("cake", n_walks = 100)

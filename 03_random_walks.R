library(tidyverse)

# import data
swow <- read_csv("00_swow_edited.csv")

# function for taking random walks
take_walks <- function(cue_node, n_walks = 5, alpha = .95, .swow = swow, assoc_weight = "norm_ppmi", direction = "forwards", known_cues_only = TRUE) {
  
  # NOTE: assoc_weight should be the column that reflects association strength (i.e. "ppmi", "norm_ppmi", or "R123.Strength")
  
  # if direction is "forwards", cue to response, if "backwards", response to cue
  if (direction == "forwards") {
    start_node <- "cue"
    next_node <- "response"
  } else if (direction == "backwards") {
    start_node <- "response"
    next_node <- "cue"
  } else {
    stop("unknown walk direction (expected \"forwards\" or \"backwards\")")
  }
  
  library(purrr)
  
  # if known_cues_only is TRUE, responses are removed if they are never used as a cue
  # setting known_cues_only to FALSE will cause walks to terminate at non-cue responses, such that all subsequent steps in that walk will return NA
  
  # if the direction is backwards, only `response %in% cue` becomes `cue %in% response`
  
  if (known_cues_only) .swow <- dplyr::filter(.swow, !!dplyr::sym(next_node) %in% !!dplyr::sym(start_node))
  
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
      cue_dat <- filter(.swow, !!dplyr::sym(start_node) == node_i)
      # get the next step
      if (nrow(cue_dat) > 0) {
        cue_dat %>%
          dplyr::sample_n(1, weight = !!dplyr::sym(assoc_weight)) %>%
          dplyr::pull(!!dplyr::sym(next_node))
      } else {
        NA
      }
    }
  }
  
  cat(sprintf("Taking %i walks...", n_walks))
  out <- purrr::map(walk_lengths, ~ purrr::accumulate(1:., take_step, .init = cue_node))
  cat(sprintf("  -done\n"))
  
  # store the direction
  attr(out, "direction") <- direction
  
  out
  
}

# function for making result of `take_walks()` tidier
tidy_walks <- function(walks, .swow = swow) {
  # get the direction
  direction <- attr(walks, "direction")
  # if direction is "forwards", cue to response, if "backwards", response to cue
  if (direction == "backwards") {
    start_node <- "response"
    next_node <- "cue"
    # rename the .swow columns to trick the left joins
    .swow <- rename(.swow, response = "cue", cue = "response")
  }
  # store the cue
  cue_node <- walks %>%
    lapply(dplyr::first) %>%
    unlist() %>%
    unique()
  # check there is only one cue
  if (length(cue_node) > 1) stop("Multiple cues detected")
  # get the cue to each step (step cue)
  step_cues <- lapply(walks, function(w) w[1:length(w)-1] )
  # remove the cues from the walks
  walks <- lapply(walks, function(w) w[2:length(w)] )
  # get the number of steps taken to reach any node, in each walk
  step_nrs <- lapply(walks, function(w) 1:length(w) )
  # sensible names for later left joins
  .swow_cue <- .swow %>%
    select(cue, response, ppmi, norm_ppmi) %>%
    rename("start_ppmi" = ppmi, "start_norm_ppmi" = norm_ppmi)
  .swow_step <- .swow %>%
    select(cue, response, ppmi, norm_ppmi) %>%
    rename("step_ppmi" = ppmi, "step_norm_ppmi" = norm_ppmi)
  # tidy into a tibble
  walks_df <- 1:length(walks) %>%
    purrr::map_df(~ tibble(start = cue_node, from = step_cues[[.x]], node = walks[[.x]], step_nr = step_nrs[[.x]]) ) %>%
    # get the stats relative to the cue
    dplyr::left_join(.swow_cue, by = c("start" = "cue", "node" = "response")) %>%
    # get the stats relative to the last step
    dplyr::left_join(.swow_step, by = c("from" = "cue", "node" = "response"))
  
  walks_df
}

cos.sim <- function(ix) {
  A = X[ix[1],]
  B = X[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}  

cake_walks <- take_walks("cake", n_walks = 10) %>%
  tidy_walks()

food_walks <- take_walks("cake", n_walks = 10) %>%
  tidy_walks()

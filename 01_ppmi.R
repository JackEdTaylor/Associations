library(tidyverse)
library(plotly)

# import data
swow <- read_tsv("00_strength.SWOW-EN.R123.tsv")

# function to calculate positive pointwise mutual information (PPMI)
calc_ppmi <- function(this_rc, all_rc, n_cues = length(unique(swow$cue))) {
  # this_rc should be p(r|c) for the present response-cue combination
  # all_rc should be a vector of all p(r|c) for all cues, in which the present response was given
  # n_cues should be the number of cues
  max( c(0, log2( this_rc * n_cues / sum(all_rc) )) )
}

# function to calculate a cue-response combination's PPMI, given the cue, response, and data
calc_ppmi_cr <- function(c, r, df = swow) {
  
  if (length(r) != length(c)) stop("Expected equal number of responses and cues (c_i should combine with r_i)")
  
  n_combs <- length(c)
  
  n_cues <- df %>%
    pull(cue) %>%
    unique() %>%
    length()
  
  lapply(1:n_combs, function(i) {
    
    # priont progress every x% if more than 1000 calculations
    if (n_combs > 1000) {
      perc_done <- (i / n_combs) * 100
      print_every_x_perc <- 1
      if (round(perc_done) %% print_every_x_perc == 0) {
        cat(sprintf("%i/%i (%i%%)\n", i, n_combs, round(perc_done)))
      }
    }
    
    c_i <- c[[i]]
    r_i <- r[[i]]
    
    this_rc <- df %>%
      filter(cue == c_i, response == r_i) %>%
      pull(R123.Strength)
    
    all_rc <- df %>%
      filter(response == r_i) %>%
      pull(R123.Strength)
    
    calc_ppmi(this_rc, all_rc, n_cues)
    
  }) %>%
    unlist()
  
}

# test function
calc_ppmi_cr("sill", "window")
calc_ppmi_cr("old", "age")
calc_ppmi_cr("sea", "blood")
calc_ppmi_cr(c("sill", "old", "sea"), c("window", "age", "blood"))

# calculate ppmi for all words
swow <- swow %>%
  mutate(ppmi = calc_ppmi_cr(cue, response, .))

write_csv(swow, "00_swow_edited.csv")

source("theme_black.R", local=T)

swow %>%
  ggplot(aes(x = R123.Strength, y = ppmi)) +
  geom_bin2d(bins = 1000) +
  scale_fill_continuous(type = "viridis", trans = "log2") +
  theme_black()

library(tidyverse)

# import data
swow <- read_tsv("00_strength.SWOW-EN.R123.tsv")

# function to calculate cosine similarity
calc_cos <- function(a, b) {
  if (length(a) != length(b)) stop("Inconsistent vector lengths")
  sum(a * b) / sqrt(sum(a^2)*sum(b^2))
}

# function to calculate associative strength
calc_assoc_str <- function(cues, .swow = swow) {
  
  if (identical(cues[[1]], cues[[2]])) {
    if (cues[[1]] %in% .swow$cue) return(1) else return(NA)
  }
  
  cues_neighbours <- .swow %>%
    dplyr::filter(cue %in% cues) %>%
    dplyr::mutate(cue = dplyr::case_when(
      cue == cues[[1]] ~ "cue_a",
      cue == cues[[2]] ~ "cue_b"
    )) %>%
    select(cue, response, R123.Strength) %>%
    tidyr::pivot_wider(names_from = cue, values_from = R123.Strength, names_prefix = "p_") %>%
    #dplyr::filter(!is.na(p_cue_a), !is.na(p_cue_b))
    tidyr::replace_na(list(p_cue_a = 0, p_cue_b = 0))
  
  calc_cos(cues_neighbours$p_cue_a, cues_neighbours$p_cue_b)
  
}

calc_assoc_str(c("jungle", "leopard"))

calc_assoc_str(c("jungle", "cat"))

calc_assoc_str(c("cat", "leopard"))

calc_assoc_str(c("cat", "cat"))

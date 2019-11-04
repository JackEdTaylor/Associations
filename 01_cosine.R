library(tidyverse)

# import data
swow <- read_tsv("00_strength.SWOW-EN.R123.tsv")

# function to calculate cosine similarity
calc_cos <- function(a, b) {
  if (length(a) != length(b)) stop("Inconsistent vector lengths")
  sum(a * b) / sqrt(sum(a^2)*sum(b^2))
}

# function to calculate associative strength
assoc_str <- function(cues_a, cues_b, .swow = swow, print = FALSE) {
  
  if (length(cues_a) != length(cues_b)) stop("Inconsistent vector lengths")
  
  sapply(1:length(cues_a), function(i) {
    cues_a_i <- as.character(cues_a[[i]])
    cues_b_i <- as.character(cues_b[[i]])
    
    if (identical(cues_a_i, cues_b_i)) {
      cos_i <- if (cues_a_i %in% .swow$cue) 1 else NA
    } else {
      cues_neighbours <- .swow %>%
        dplyr::filter(cue %in% c(cues_a_i, cues_b_i)) %>%
        dplyr::mutate(cue = dplyr::case_when(
          cue == cues_a_i ~ "cue_a",
          cue == cues_b_i ~ "cue_b"
        )) %>%
        select(cue, response, R123.Strength) %>%
        tidyr::pivot_wider(names_from = cue, values_from = R123.Strength, names_prefix = "p_") %>%
        #dplyr::filter(!is.na(p_cue_a), !is.na(p_cue_b))
        tidyr::replace_na(list(p_cue_a = 0, p_cue_b = 0))
      
      cos_i <- calc_cos(cues_neighbours$p_cue_a, cues_neighbours$p_cue_b)
    }
    
    if (print) cat(sprintf("\"%s\" ~ \"%s\" = %s\n", cues_a_i, cues_b_i, cos_i))
    
    cos_i
  })
}

assoc_str("jungle", "leopard")
assoc_str("jungle", "cat")
assoc_str("cat", "leopard", print = TRUE)
assoc_str("cat", "cat")
assoc_str(c("cat", "cat", "cat"), c("cat", "teacher", "jungle"), print = TRUE)

swow_cues <- expand.grid(unique(swow$cue), unique(swow$cue)) %>%
  magrittr::set_colnames(c("cue_a", "cue_b")) %>%
  dplyr::arrange(cue_a, cue_b) %>%
  dplyr::mutate(cos_R123 = assoc_str(cue_a, cue_b))

write_csv(swow_cues, "00_cos.csv")

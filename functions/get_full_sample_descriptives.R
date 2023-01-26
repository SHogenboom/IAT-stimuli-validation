################################################################################
################################################################################
### LICENSE:                                                                 ###
### This code is available under a CC BY-NC-SA 4.0 license                   ###
### https://creativecommons.org/licenses/by-nc-sa/4.0/                       ###
###                                                                          ###
### BY  – Credit must be given to the creator                                ###
### NC  – Only noncommercial uses of the work are permitted                  ###
### SA  – Adaptations must be shared under the same terms                    ###
################################################################################
################################################################################

#' GOAL
#' Get summary statistics for the full sample (N, M, SD, Min, Max)
#' @param year_of_interest numeric
#'
#' @return list of descriptives

get_full_sample_descriptives <- function(year_of_interest) {
  # DETERMINE the number of included participants per IAT
  # based on the compressed in-/exclusion statistics across IATs
  full_sample_stats <-
    get_full_sample_stats(year_of_interest) |>
    # Keep only the statistics from the included participants
    dplyr::filter(Criteria == "Total",
                  Type == "Included")

  # initialize
  stats <- list()
  #### SAVE IN-TEXT STATISTICS ####
  stats$total_included <- sum(full_sample_stats$N)
  # round to whole participants
  stats$mean_pp_per_IAT <- round(mean(full_sample_stats$N), 0)
  stats$sd_pp_per_IAT <- round(sd(full_sample_stats$N), 0)
  stats$max_pp_per_IAT <- round(max(full_sample_stats$N), 0)
  stats$min_pp_per_IAT <- round(min(full_sample_stats$N), 0)

  # Find IAT with the most participants
  stats$max_IAT <- full_sample_stats$IAT[full_sample_stats$N == max(full_sample_stats$N)]
  # Find IAT with the least participants
  stats$min_IAT <- full_sample_stats$IAT[full_sample_stats$N == min(full_sample_stats$N)]

  return(stats)
}

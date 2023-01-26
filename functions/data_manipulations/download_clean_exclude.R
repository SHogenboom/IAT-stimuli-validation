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
#'
#' GOAL
#' Combine all steps required to get useable data
#' 1. Download the data from OSF from URLs > .zip
#' 2. Preprocess the data (e.g., cleaning, compute age etc.) > _prepared.csv
#' 3. Exclude Participants/Trials > _cleaned.csv / _exclusion_stats.csv
#'
#' See 'workflow_data.html' for explanations and examples of each step.
#' An overview of changes (compared to Stage 1) is available in
#' ... 'explanation_of_changes.html'
#'
#' @param IAT_name name of the IAT's folder in `datasets`
#' @param year_of_interest numeric
#' @param months_of_interest numeric vector of months to retain.
#' With 1 = January - 12 = December.
#'
#' @return list of `nrow_compressed` & `nrow_raw` (after exclusion)

download_prep_exclude_data <- function(IAT_name,
                                       year_of_interest,
                                       months_of_interest) {
  # DOWNLOAD: Compressed and Raw data from OSF
  # NOTE: we retrieve data based on URLs stored in `datasets/OSF/OSF-urls.csv`.
  # The urls are added manually - which is why they only contain those for 2019 (pilot) and 2020 (all IATs).
  retrieve_IAT_data(
    IAT_name,
    year_of_interest,
    # Folder where the IAT data is stored (subfolder per IAT)
    output_path = here::here(
      "datasets",
      "OSF"
    )
  )

  # PREPARE: Compressed data (e.g., compute participant age)
  prepare_compressed(
    IAT_name,
    year_of_interest,
    months_of_interest # filters from a year to the months of interest
  )

  # PREPARE: Raw data (e.g., determine trial_congruency)
  prepare_raw(
    IAT_name,
    year_of_interest
  )

  # EXCLUDE: Compressed data (e.g., remove participants with missing D-score)
  # Outputs:
  # (1) The cleaned compressed data
  # (2) The exclusion statistics
  nrow_compressed <-
    exclusion_compressed(
      IAT_name,
      year_of_interest
    )

  # EXCLUDE: Raw data (e.g., RT > 10,000 ms)
  # Outputs:
  # (1) The cleaned raw data
  # (2) The exclusion statistics
  nrow_raw <-
    exclusion_raw(
      IAT_name,
      year_of_interest
    )

  return(list(
    "nrow_compressed" = nrow_compressed,
    "nrow_raw" = nrow_raw
  ))
}

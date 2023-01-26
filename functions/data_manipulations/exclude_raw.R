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
#' Apply the exclusion criteria to a PREPARED data set
#' @param IAT_name name of the IAT's folder in `datasets`
#' @param year_of_interest numeric
#'
#' @return nrow of included data
#' saves prepared raw data minus the excluded users
#' saves exclusion statistics; the summary statistics of the applied exclusion criteria

exclusion_raw <- function(IAT_name,
                          year_of_interest) {
  # SET data_path
  data_path <-
    here::here(
      "datasets",
      "OSF",
      IAT_name
    )

  # CHECK if file exists
  if (
    file.exists(
      here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}_cleaned.csv")
      )
    ) &
      file.exists(
        here::here(
          data_path,
          glue::glue("Raw_{year_of_interest}_exclusion_stats.csv")
        )
      )
  ) {
    # Data already excluded - return number of included rows of data
    return(
      nrow(
        data.table::fread(
          here::here(
            data_path,
            glue::glue("Raw_{year_of_interest}_cleaned.csv")
          )
        )
      )
    )
  } else {
    # initialize
    exclusion_data <- data.frame()

    #### LOAD prepared raw data ####
    dat <-
      data.table::fread(
        here::here(
          data_path,
          glue::glue("Raw_{year_of_interest}_prepared.csv")
        )
      )

    #### DELETE: trials from excluded participants ####
    # Load cleaned compressed data = included participants
    dat_compressed_cleaned <-
      data.table::fread(
        here::here(
          data_path,
          glue::glue("Compressed_{year_of_interest}_cleaned.csv")
        )
      )

    # Keep only trials from included session_ids
    dat <-
      dat[session_id %in% dat_compressed_cleaned$session_id, ]

    #### TRIAL LATENCY ####
    # Remove trials that were slower than 10,000 ms
    # Remove trials that were 0 or negative
    dat <-
      dat[, "excl_latency" :=
        data.table::fcase(
          is.na(trial_latency), "Missing",
          trial_latency < 1, "Excluded",
          trial_latency > 10000, "Excluded",
          # All other cases
          default = "Included"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "Latency",
          "Type" = names(table(dat$excl_latency)),
          "N" = table(dat$excl_latency),
          "index" = 1
        )
      )

    #### ERROR RATES ####
    # We want to exclude users that have error rates higher than chance (50%)
    # As it is unlikely that these participants understood the task demands
    dat <-
      dat[, `:=`(
        # Total number of trials per participant
        n_trials = .N,
        # Total number of errors per participant
        # trial_error = 0 (correct) or 1 (incorrect)
        n_errors = sum(trial_error),
        # Percentage of errors per participant
        perc_errors = sum(trial_error) / .N * 100
      ),
      by = session_id
      ]

    dat <-
      dat[, "excl_error" :=
        data.table::fcase(
          is.na(perc_errors), "Missing",
          perc_errors > 50, "Excluded",
          # All other cases
          default = "Included"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "% Errors",
          "Type" = names(table(dat$excl_error)),
          "N" = table(dat$excl_error),
          "index" = max(exclusion_data$index) + 1
        )
      )

    #### TOTAL ####
    # Both excluded and missing data will be removed
    dat <-
      dat[, "excl_total" :=
        data.table::fcase(
          excl_latency == "Included" &
            excl_error == "Included", "Included",
          # All other cases
          default = "Excluded"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "Total",
          "Type" = names(table(dat$excl_total)),
          "N" = table(dat$excl_total),
          "index" = max(exclusion_data$index + 1)
        )
      )

    #### CLEAN DATA ####
    # Exclude both when information is missing, and/or when explicitly excluded
    dat <-
      dat[excl_total == "Included", ]

    # Add IAT information
    exclusion_data <-
      exclusion_data |>
      dplyr::mutate("IAT" = IAT_name)

    #### SAVE: data as CSV ####
    # Cleaned data set
    data.table::fwrite(
      x = dat,
      file = here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}_cleaned.csv")
      )
    )

    # Exclusion Statistics
    data.table::fwrite(
      x = exclusion_data,
      file = here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}_exclusion_stats.csv")
      )
    )

    #### RETURN: number of rows of data ####
    return(nrow(dat))
  } # END IF file.exists
} # END function

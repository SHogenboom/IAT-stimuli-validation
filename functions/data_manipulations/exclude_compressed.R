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
#' Applying the exclusion criteria to a PREPARED compressed data set
#' @param IAT_name name of the IAT's folder in `datasets`
#' @param year_of_interest numeric
#'
#' @return nrow included data
#' saves prepared compressed data minus the excluded users
#' saves exclusion statistics; the summary statistics of the applied exclusion criteria

exclusion_compressed <- function(IAT_name,
                                 year_of_interest) {
  # SET data_path
  data_path <-
    here::here(
      "datasets",
      "OSF",
      IAT_name
    )

  # CHECK if file.exists
  if (
    file.exists(
      here::here(
        data_path,
        glue::glue("Compressed_{year_of_interest}_cleaned.csv")
      )
    ) &
      file.exists(
        here::here(
          data_path,
          glue::glue("Compressed_{year_of_interest}_exclusion_stats.csv")
        )
      )
  ) {
    return(
      nrow(
        data.table::fread(
          here::here(
            data_path,
            glue::glue("Compressed_{year_of_interest}_cleaned.csv")
          )
        )
      )
    )
  } else {
    # initialize
    exclusion_data <- data.frame()

    #### LOAD data ####
    dat <-
      data.table::fread(
        here::here(
          data_path,
          glue::glue("Compressed_{year_of_interest}_prepared.csv")
        )
      )

    #### IAT: Completion ####
    #' We want to include only the results of participants that completed the IAT
    dat <-
      dat[, "excl_completion" :=
        data.table::fcase(
          session_status_f == "Complete", "Included",
          session_status_f == "Incomplete", "Excluded",
          default = "Missing"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "Completion",
          "Type" = names(table(dat$excl_completion)),
          "N" = table(dat$excl_completion),
          "index" = 1
        )
      )

    #### IAT: MISSING D-SCORE ####
    # It is also possible that the status lists "Complete", but the D-score is missing
    dat <-
      dat[, "excl_d_score" :=
        data.table::fcase(
          is.na(IAT_D_Score_all), "Missing",
          default = "Included"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "D-score",
          "Type" = names(table(dat$excl_d_score)),
          "N" = table(dat$excl_d_score),
          "index" = max(exclusion_data$index + 1)
        )
      )

    #### IAT: Inattentive error rates ####
    #' Greenwald et al. (2003) indicated that participants whom have too many
    #' ... responses which could be considered inattentively fast (< 300ms)
    #' ... should be removed from subsequent analyses.
    dat <-
      dat[, "excl_pct_300" :=
        data.table::fcase(
          is.na(pct_300), "Missing",
          pct_300 > 10, "Excluded",
          default = "Included"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "PCT 300",
          "Type" = names(table(dat$excl_pct_300)),
          "N" = table(dat$excl_pct_300),
          "index" = max(exclusion_data$index + 1)
        )
      )

    #### DEMOGRAPHICS: Nationality ####
    #' In order to ensure that participants have the best chances of having the
    #' ... same association with words, names, and images, we want to include
    #' users that were born, and are currently living, in the United States.
    #' The US Citizens (code 1) are by far the largest representation of participants in
    #' ... the entire data set.

    dat <-
      dat[, "excl_nationality" :=
        data.table::fcase(
          is.na(countrycit_num) | is.na(countryres_num), "Missing",
          countrycit_num == 1 & countryres_num == 1, "Included",
          # All other cases
          default = "Excluded"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "Nationality",
          "Type" = names(table(dat$excl_nationality)),
          "N" = table(dat$excl_nationality),
          "index" = max(exclusion_data$index + 1)
        )
      )

    #### DEMOGRAPHICS: Age ####
    #' Age; participants should be old enough to give informed consent (>= 18)
    #' ... but should also be 'young adults' (<= 25)
    #' We establish the age from the moment the IAT was completed which is computed
    #' ... during the data preperation
    dat <-
      dat[, "excl_age" :=
        data.table::fcase(
          is.na(age), "Missing",
          age < 18 | age > 26, "Excluded",
          # All other cases
          default = "Included"
        )]

    # ADD TO OVERVIEW
    exclusion_data <-
      rbind(
        exclusion_data,
        dplyr::tibble(
          "Criteria" = "Age",
          "Type" = names(table(dat$excl_age)),
          "N" = table(dat$excl_age),
          "index" = max(exclusion_data$index + 1)
        )
      )

    #### TOTAL ####
    # Both excluded and missing data will be removed
    dat <-
      dat[, "excl_total" :=
        data.table::fcase(
          excl_completion == "Included" &
            excl_d_score == "Included" &
            excl_pct_300 == "Included" &
            excl_nationality == "Included" &
            excl_age == "Included", "Included",
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
        glue::glue("Compressed_{year_of_interest}_cleaned.csv")
      )
    )

    # Exclusion Statistics
    data.table::fwrite(
      x = exclusion_data,
      file = here::here(
        data_path,
        glue::glue("Compressed_{year_of_interest}_exclusion_stats.csv")
      )
    )

    #### RETURN: number of rows of data - after exclusion ####
    return(nrow(dat))
  } # END IF file.exists
} # END function

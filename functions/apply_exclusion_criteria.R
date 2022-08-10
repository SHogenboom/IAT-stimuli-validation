################################################################################
################################################################################
### LISCENSE:                                                                ###
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
#' @param raw_dat prepared raw data
#' @return cleaned_dat; cleaned raw_dat minus the excluded data
#' @return exclusion_data; the summary statistics of the applied exclusion criteria

exclusion_raw <- function(raw_dat) {

    # initialize
    exclusion_data <- data.frame()

    #### TRIAL LATENCY ####
    # Remove trials that were slower than 10,000 ms
    # Remove trials that were 0 or negative
    raw_dat <-
        raw_dat %>%
        mutate("excl_latency" =
                   case_when(is.na(trial_latency) ~ "Missing",
                             trial_latency < 1 ~ "Excluded",
                             trial_latency > 10000 ~ "Excluded",
                             # All other cases
                             TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              raw_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_latency) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "Latency",
                            "N" = n(),
                            "index" = 1) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_latency))

    #### ERROR RATES ####
    # We want to exclude users that have error rates higher than chance (50%)
    # As it is unlikely that these participants understood the task demands
    pp_error_rates <-
        raw_dat %>%
        # Compute per participant / session
        group_by(session_id) %>%
        # Determine percentage error
        summarize("n_trials" = n(),
                  # trial_error = 0 (correct) or 1 (incorrect)
                  "n_errors" = sum(trial_error),
                  # computer percentage
                  "perc_errors" = n_errors / n_trials * 100) %>%
        ungroup()

    # Determine which participants to exclude
    raw_dat <-
        raw_dat %>%
        # Add the percentage incorrect
        left_join(pp_error_rates[, c("session_id", "perc_errors")],
                  by = "session_id") %>%
        # Determine whether the participant (and thus the trial) should be excluded
        mutate("excl_error" = case_when(is.na(perc_errors) ~ "Missing",
                                        perc_errors > 50 ~ "Excluded",
                                        # All other cases
                                        TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              raw_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_error) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "% Errors",
                            "N" = n(),
                            "index" = max(exclusion_data$index) + 1) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_error))

    #### CLEAN DATA ####
    # Exclude both when information is missing, and/or when explicitly excluded
    cleaned_data <-
        raw_dat %>%
        filter(excl_latency == "Included",
               excl_error == "Included")

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              tibble("Criteria" = rep("Total", 2),
                     "Type" = c("Included", "Excluded"),
                     "N" = c(nrow(cleaned_data),
                             nrow(raw_dat) - nrow(cleaned_data)),
                     "index" = max(exclusion_data$index + 1)))

    #### RETURN ####
    return(list("cleaned_dat" = cleaned_data,
                "exclusion_data" = exclusion_data))
}

#' GOAL
#' Applying the exclusion criteria to a PREPARED compressed data set
#' @param compressed_dat prepared compressed data
#' @param countries users to retain from which countries. Defaults to the USA
#' ... though may be changed if one wants to explore different poppulations
#' @return cleaned_dat; the compressed_dat minus the excluded users
#' @retun exclusion_data; the summary statistics of the applied exclusion criteria

exclusion_compressed <- function(compressed_dat,
                                 countries = c("U.S.A. ", "U.S.A.")) {

    # initialize
    exclusion_data <- data.frame()

    #### IAT: Completion ####
    #' We want to include only the results of participants that completed the IAT
    compressed_dat <-
        compressed_dat %>%
        mutate("excl_completion" =
                   case_when(session_status == "Complete" ~ "Included",
                             session_status == "Incomplete" ~ "Excluded",
                             # All other cases
                             TRUE ~ "Missing"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_completion) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "Completion",
                            "N" = n(),
                            "index" = 1) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_completion))

    #### IAT: MISSING D-SCORE ####
    # It is also possible that the status lists "Complete", but the D-score is missing
    compressed_dat <-
        compressed_dat %>%
        mutate("excl_d_score" =
                   case_when(is.na(IAT_D_Score_all) ~ "Missing",
                             # All other cases
                             TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_d_score) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "D-score",
                            "N" = n(),
                            "index" = max(exclusion_data$index + 1)) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_d_score))

    #### IAT: Inattentive error rates ####
    #' Greenwald et al. (2003) indicated that participants whom have too many
    #' ... responses which could be considered inattentively fast (< 300ms)
    #' ... should be removed from subsequent analyses.

    compressed_dat <-
        compressed_dat %>%
        mutate("excl_pct_300" =
                   case_when(is.na(pct_300) ~ "Missing",
                             pct_300 > 10 ~ "Excluded",
                             # All other cases
                             TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_pct_300) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "PCT 300",
                            "N" = n(),
                            "index" = max(exclusion_data$index + 1)) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_pct_300))

    #### DEMOGRAPHICS: Nationality ####
    #' In order to ensure that participants have the best chances of having the
    #' ... same association with words, names, and images, we want to include
    #' users that were born, and are currently living, in the United States.
    #' The US Citizens are by far the largest representation of participants in
    #' ... the entire data set.

    compressed_dat <-
        compressed_dat %>%
        mutate("excl_nationality" =
                   case_when(is.na(countrycit_num) |
                                 is.na(countryres_num) ~ "Missing",
                             !as.character(countrycit_num) %in%
                                 countries |
                                 !as.character(countryres_num) %in%
                                 countries ~ "Excluded",
                             # All other cases
                             TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_nationality) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "Nationality",
                            "N" = n(),
                            "index" = max(exclusion_data$index + 1)) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_nationality))

    #### DEMOGRAPHICS: Age ####
    #' Age; participants should be old enough to give informed consent (>= 18)
    #' ... but should also be 'young adults' (<= 25)
    #' We establish the age from the moment the IAT was completed which is computed
    #' ... during the data preperation

    compressed_dat <-
        compressed_dat %>%
        mutate("excl_age" =
                   case_when(is.na(age) ~ "Missing",
                             age < 18 | age > 26 ~ "Excluded",
                             # All other cases should be excluded
                             TRUE ~ "Included"))

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_age) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "Age",
                            "N" = n(),
                            "index" = max(exclusion_data$index + 1)) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_age))

    #### TOTAL ####
    # Both excluded and missing data will be removed
    compressed_dat <-
        compressed_dat %>%
        rowwise() %>%
        mutate("excl_total" =
                   # Only include participants who met all the criteria
                   case_when(all(c(excl_completion,
                                   excl_d_score,
                                   excl_pct_300,
                                   excl_nationality,
                                   excl_age) == "Included") ~ "Included",
                             # All other cases
                             # Some data missing or excluded
                             TRUE ~ "Excluded")) %>%
        ungroup()

    # ADD TO OVERVIEW
    exclusion_data <-
        rbind(exclusion_data,
              # add new statistics
              compressed_dat %>%
                  # group by values in the exclusion column (to change per criteria)
                  group_by(excl_total) %>%
                  # summarize the number of participants per exclusion type
                  summarize("Criteria" = "Total",
                            "N" = n(),
                            "index" = max(exclusion_data$index + 1)) %>% # set for plotting
                  ungroup() %>%
                  # rename for plotting (to change per criteria)
                  rename("Type" = excl_total))

    #### CLEAN DATA ####
    # Exclude both when information is missing, and/or when explicitly excluded
    cleaned_data <-
        compressed_dat %>%
        filter(compressed_dat$excl_total == "Included")

    #### RETURN ####
    return(list("cleaned_dat" = cleaned_data,
                "exclusion_data" = exclusion_data))

}

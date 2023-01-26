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
#' Generate automatic text that can be included in the results section
#' Number of Participants
#' Number of Trials
#' Number of Trials in Block 1/2
#' Descriptives Age (M; SD; 95% CI)
#' Descriptives Validity (RT, ERROR, TOTAL)
#' @param IAT_name the name of the IAT from which to retrieve the data
#' @param year_of_interest
#' @return data.frame which contains the relevant text in each column

results_text <- function(IAT_name,
                         year_of_interest) {

  # Initialize
  results <- list()

  #### LOAD DATA ####
  if (IAT_name != "ALL") {
    # Participants
    dat_compressed <-
      data.table::fread(
        here::here("datasets",
                   "OSF",
                   IAT_name,
                   glue::glue("Compressed_{year_of_interest}_cleaned.csv")))

    # Raw
    dat_raw <-
      data.table::fread(
        here::here("datasets",
                   "OSF",
                   IAT_name,
                   glue::glue("Raw_{year_of_interest}_cleaned.csv")))

    # Validity Estimates
    exemplar_stats <-
      data.table::fread(
        here::here("datasets",
                   "Analyses",
                   glue::glue("validity_estimates_{IAT_name}_{year_of_interest}.csv")))
  } else {
    # Validity Estimates
    exemplar_stats <-
      get_all_validity_estimates(year_of_interest)
  }


  #### RESULTS: Participants & Responses ####
  if (IAT_name != "ALL") {
  n_participants <- nrow(dat_compressed)
  n_trials <- nrow(dat_raw)
  n_block_12 <- nrow(dat_raw |> dplyr::filter(block_number %in% c(1, 2)))

  results$participants_and_responses <-
    glue::glue("The {IAT_name}-IAT included {n_participants} participants who provided",
    " {n_trials} responses overall and {n_block_12} responses in Block 1 and 2. ")

 #### RESULTS: Age ####
  mean_age <- round(mean(dat_compressed$age), 2)
  sd_age <- round(sd(dat_compressed$age), 2)
  age_ci <- round(Rmisc::CI(dat_compressed$age), 2)

  results$age <-
    glue::glue("The included participants were on average {mean_age} years old ",
               "($SD$ = {sd_age}; 95% CI = {age_ci[1]}, {age_ci[3]}). ")
  }

  #### RESULTS: Validity ####
  # Generate from results so that text is updated automatically
  validity_text <-
    exemplar_stats |>
    dplyr::summarize(
      # N VALID
      "N_valid_RT" = sum(Rel_validity_RT == "valid"),
      "N_valid_ERROR" = sum(Rel_validity_ERROR == "valid"),
      "N_valid_TOTAL" = sum(Rel_validity_TOTAL == "valid"),
      # N INVALID
      "N_invalid_RT" = sum(Rel_validity_RT == "invalid"),
      "N_invalid_ERROR" = sum(Rel_validity_ERROR == "invalid"),
      "N_invalid_TOTAL" = sum(Rel_validity_TOTAL == "invalid"),
      # N UNRELIABLE
      "N_unreliable_RT" = dplyr::n() - N_valid_RT - N_invalid_RT,
      "N_unreliable_ERROR" = dplyr::n() - N_valid_ERROR - N_invalid_ERROR,
      "N_unreliable_TOTAL" = dplyr::n() - N_valid_TOTAL - N_invalid_TOTAL,
      # PERC
      "P_valid_RT" = round(N_valid_RT / dplyr::n() * 100, 2),
      "P_invalid_RT" = round(N_invalid_RT / dplyr::n() * 100, 2),
      "P_unreliable_RT" = round(N_unreliable_RT / dplyr::n() * 100, 2),
      "P_valid_ERROR" = round(N_valid_ERROR / dplyr::n() * 100, 2),
      "P_invalid_ERROR" = round(N_invalid_ERROR / dplyr::n() * 100, 2),
      "P_unreliable_ERROR" = round(N_unreliable_ERROR / dplyr::n() * 100, 2),
      "P_valid_TOTAL" = round(N_valid_TOTAL / dplyr::n() * 100, 2),
      "P_invalid_TOTAL" = round(N_invalid_TOTAL / dplyr::n() * 100, 2),
      "P_unreliable_TOTAL" = round(N_unreliable_TOTAL / dplyr::n() * 100, 2),
      # TEXT
      "RT" =
        glue::glue('Based on average RT, ',
                   # VALID
                   '{N_valid_RT} ',
                   'of the stimuli used in the {IAT_name}-IAT ',
                   '{ifelse(N_valid_RT == 1, "was ", "were ")}',
                   'reliably valid ({P_valid_RT}%), ',
                   # INVALID
                   '{N_invalid_RT} ',
                   'reliably invalid ({P_invalid_RT}%), ',
                   # UNRELIABLE
                   'and {N_unreliable_RT} ',
                   'could not be reliably estimated ({P_unreliable_RT}%). '),
      "ERROR" =
        glue::glue('With respect to the percentage of errors, ',
                   # VALID
                   '{N_valid_ERROR} ',
                   'of the stimuli used in the {IAT_name}-IAT ',
                   '{ifelse(N_valid_ERROR == 1, "was ", "were ")}',
                   'reliably valid ({P_valid_ERROR}%), ',
                   # INVALID
                   '{N_invalid_ERROR} ',
                   'reliably invalid ({P_invalid_ERROR}%), ',
                   # UNRELIABLE
                   'and {N_unreliable_ERROR} ',
                   'could not be reliably estimated ({P_unreliable_ERROR}%). '),
      "TOTAL" =
        glue::glue('Taken together, based on RT *and* error percentages, ',
                   # VALID
                   '{N_valid_TOTAL} ',
                   'of the stimuli used in the {IAT_name}-IAT ',
                   '{ifelse(N_valid_TOTAL == 1, "was ", "were ")}',
                   'reliably valid ({P_valid_TOTAL}%), ',
                   # INVALID
                   '{N_invalid_TOTAL} ',
                   'reliably invalid ({P_invalid_TOTAL}%), ',
                   # UNRELIABLE
                   'and {N_unreliable_TOTAL} ',
                   'could not be reliably estimated ({P_unreliable_TOTAL}%). ')
    ) |>
    dplyr::ungroup()

  # Store results
  results$RT <- validity_text$RT
  results$ERROR <- validity_text$ERROR
  results$TOTAL <- validity_text$TOTAL

  if (IAT_name == "ALL") {
    results <-
      lapply(X = results,
             FUN = function(str) {
               gsub(x = str,
                    pattern = "in the ALL-IAT",
                    replacement = "in all IATs")
             })

  }

  results$stats <- validity_text

  return(results)
} # END FUNCTION

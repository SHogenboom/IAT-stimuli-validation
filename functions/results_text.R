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
#' Generate automatic text that can be included in the results section
#' ... which details the number of valid, invalid, and reliable
#' ... stimuli (and the percentages) per criteria.
#' @param exemplar_stats output from functions/validity_estimates.R
#' @param IAT name of the IAT to be displayed in the text.
#' @return data.frame which contains the relevant text in each column

results_text <- function(exemplar_stats, IAT) {

    #### RESULTS TEXT ####
    # Generate from results so that text is updates automatically
    validity_text <-
        exemplar_stats %>%
        summarize(
            # N VALID
            "N_valid_RT" = sum(Rel_validity_RT == "valid"),
            "N_valid_ERROR" = sum(Rel_validity_ERROR == "valid"),
            "N_valid_TOTAL" = sum(Rel_validity_TOTAL == "valid"),
            # N INVALID
            "N_invalid_RT" = sum(Rel_validity_RT == "invalid"),
            "N_invalid_ERROR" = sum(Rel_validity_ERROR == "invalid"),
            "N_invalid_TOTAL" = sum(Rel_validity_TOTAL == "invalid"),
            # N UNRELIABLE
            "N_unreliable_RT" = n() - N_valid_RT - N_invalid_RT,
            "N_unreliable_ERROR" = n() - N_valid_ERROR - N_invalid_ERROR,
            "N_unreliable_TOTAL" = n() - N_valid_TOTAL - N_invalid_TOTAL,
            # TEXT
            "RT" =
                glue::glue('Based on average RT ',
                           # VALID
                           '{N_valid_RT} ',
                           'of the ',
                           '{ifelse(N_valid_RT == 1, "stimulus used in the {IAT} was ", "stimuli used in the {IAT} were ")}',
                           'reliably valid ({round(N_valid_RT / n() * 100, 2)}%), ',
                           # INVALID
                           '{N_invalid_RT} ',
                           'reliably invalid ({round(N_invalid_RT / n() * 100, 2)}%), ',
                           # UNRELIABLE
                           'and {N_unreliable_RT} ',
                           'could not be reliably estimated ({round(N_unreliable_RT / n() * 100, 2)}%). '),
            "ERROR" =
                glue::glue('With respect to the percentage of errors ',
                           # VALID
                           '{N_valid_ERROR} ',
                           'of the ',
                           '{ifelse(N_valid_ERROR == 1, "stimulus used in the {IAT} was ", "stimuli used in the {IAT} were ")}',
                           'reliably valid ({round(N_valid_ERROR / n() * 100, 2)}%), ',
                           # INVALID
                           '{N_invalid_ERROR} ',
                           'reliably invalid ({round(N_invalid_ERROR / n() * 100, 2)}%), ',
                           # UNRELIABLE
                           'and {N_unreliable_ERROR} ',
                           'could not be reliably estimated ({round(N_unreliable_ERROR / n() * 100, 2)}%). '),
            "TOTAL" =
                glue::glue('Taken together, based on RT *and* error percentages, ',
                           # VALID
                           '{N_valid_TOTAL} ',
                           'of the ',
                           '{ifelse(N_valid_TOTAL == 1, "stimulus used in the {IAT} was ", "stimuli used in the {IAT} were ")}',
                           'reliably valid ({round(N_valid_TOTAL / n() * 100, 2)}%), ',
                           # INVALID
                           '{N_invalid_TOTAL} ',
                           'reliably invalid ({round(N_invalid_TOTAL / n() * 100, 2)}%), ',
                           # UNRELIABLE
                           'and {N_unreliable_TOTAL} ',
                           'could not be reliably estimated ({round(N_unreliable_TOTAL / n() * 100, 2)}%). ')
        ) %>%
        ungroup()

    # Add the correct IAT name
    validity_text$RT <- gsub(x = validity_text$RT,
                             pattern = "{IAT}",
                             replacement = IAT,
                             fixed = TRUE)
    validity_text$ERROR <- gsub(x = validity_text$ERROR,
                             pattern = "{IAT}",
                             replacement = IAT,
                             fixed = TRUE)
    validity_text$TOTAL <- gsub(x = validity_text$TOTAL,
                                pattern = "{IAT}",
                                replacement = IAT,
                                fixed = TRUE)

    return(validity_text)
} # END FUNCTION

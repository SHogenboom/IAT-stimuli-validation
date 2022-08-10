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
#' Convert the sample statistics to estimates of validity dependent on a cut-off
#' Add the colors that will be used for subsequent plotting.
#' @param samples the output from functions/bootstrapped_parameters.R
#' @param perc_limit the upper percentage that is required to class an exemplar
#' ... as reliably VALID 100% - perc_limit is automatically classified as
#' ... reliably INVALID. Everything in between is classed as unreliable
#' @param col_valid the color given to VALID exemplars, defaults to green
#' @param col_invalid the color given to INVALID exemplars, defaults to red
#' @param col_unreliable the color given to UNRELIABLE exemplars, defaults to black

exemplar_validity_estimates <- function(samples,
                                        perc_limit,
                                        col_valid = "#009E73", # green
                                        col_invalid = "#D55E00", # red
                                        col_unreliable = "#000000" # black
                                        ) {

    samples %>%
        # Compute statistic per exemplar
        group_by(trial_name) %>%
        summarise(
            # RESPONSE TIME
            # percentage of VALID judgments
            "Perc_validity_RT" = sum(validity_RT == "valid") / n() * 100,
            # Classification of reliability of (in)valid judgments
            "Rel_validity_RT" =
                case_when(Perc_validity_RT > perc_limit ~ "valid",
                          Perc_validity_RT < (100 - perc_limit) ~ "invalid",
                          TRUE ~ "unreliable"),
            # Color coded reliability for plotting
            "Col_validity_RT" =
                case_when(Rel_validity_RT == "valid" ~ col_valid, # green
                          Rel_validity_RT == "invalid" ~ col_invalid,
                          Rel_validity_RT == "unreliable" ~ col_unreliable
                ),
            # ERROR RATE
            # percentage of VALID judgments
            "Perc_validity_ERROR" = sum(validity_ERROR == "valid") / n() * 100,
            # Classification of reliability of (in)valid judgments
            "Rel_validity_ERROR" =
                case_when(Perc_validity_ERROR > perc_limit ~ "valid",
                          Perc_validity_ERROR < (100 - perc_limit) ~ "invalid",
                          TRUE ~ "unreliable"),
            # Color coded reliability for plotting
            "Col_validity_ERROR" =
                case_when(Rel_validity_ERROR == "valid" ~ col_valid, # green
                          Rel_validity_ERROR == "invalid" ~ col_invalid,
                          Rel_validity_ERROR == "unreliable" ~ col_unreliable
                ),
            # TOTAL
            # Both RT and Error Rate reliably (in)valid
            # percentage of VALID judgments
            "Perc_validity_TOTAL" = sum(validity_TOTAL == "valid") / n() * 100,
            # Classification of reliability of (in)valid judgments
            "Rel_validity_TOTAL" =
                case_when(Perc_validity_TOTAL > perc_limit ~ "valid",
                          Perc_validity_TOTAL < (100 - perc_limit) ~ "invalid",
                          TRUE ~ "unreliable"),
            # Color coded reliability for plotting
            "Col_validity_TOTAL" =
                case_when(Rel_validity_TOTAL == "valid" ~ col_valid, # green
                          Rel_validity_TOTAL == "invalid" ~ col_invalid,
                          Rel_validity_TOTAL == "unreliable" ~ col_unreliable
                )
        )

}

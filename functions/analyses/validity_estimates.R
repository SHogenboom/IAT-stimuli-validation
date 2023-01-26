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
#' @param IAT_name the name of the IAT
#' @param year_of_interest year from which the data was derived.
#' @param perc_limit the upper percentage that is required to class an exemplar
#' ... as reliably VALID 100% - perc_limit is automatically classified as
#' ... reliably INVALID. Everything in between is classed as unreliable
#' @param col_valid the color given to VALID exemplars, defaults to green
#' @param col_invalid the color given to INVALID exemplars, defaults to red
#' @param col_unreliable the color given to UNRELIABLE exemplars, defaults to black

exemplar_validity_estimates <- function(IAT_name = IAT,
                                        year_of_interest,
                                        perc_limit,
                                        col_valid = "#009E73", # green
                                        col_invalid = "#D55E00", # red
                                        col_unreliable = "#000000" # black
) {

  if (!file.exists(here::here("datasets",
                              "Analyses",
                              glue::glue("validity_estimates_{IAT_name}_{year_of_interest}.csv")))) {

    # LOAD DATA
    samples <-
      data.table::fread(
        file = here::here("datasets",
                          "Analyses",
                          glue::glue("bootstraps_{IAT_name}_{year_of_interest}.csv")))

    #### SUMMARY STATISTICS ####
    # Percentages of VALID judgments
    validity_estimates <-
      samples[,
              .(
                # Response Times
                "Perc_validity_RT" = round(sum(validity_RT == "valid") / .N * 100, 2),
                # Error Rates
                "Perc_validity_ERROR" = round(sum(validity_ERROR == "valid") / .N * 100, 2),
                # TOTAL
                # Both RT and Error Rate reliably (in)valid
                "Perc_validity_TOTAL" = round(sum(validity_TOTAL == "valid") / .N * 100, 2)
              ),
              by = trial_name]

    # Classification of Reliability
    validity_estimates <-
      validity_estimates[,
                         `:=`("Rel_validity_RT" =
                                data.table::fcase(
                                  Perc_validity_RT > perc_limit, "valid",
                                  Perc_validity_RT < (100 - perc_limit), "invalid",
                                  default = "unreliable"),
                              "Rel_validity_ERROR" =
                                data.table::fcase(
                                  Perc_validity_ERROR > perc_limit, "valid",
                                  Perc_validity_ERROR < (100 - perc_limit), "invalid",
                                  default = "unreliable"),
                              "Rel_validity_TOTAL" =
                                data.table::fcase(
                                  Perc_validity_TOTAL > perc_limit, "valid",
                                  Perc_validity_TOTAL < (100 - perc_limit), "invalid",
                                  default = "unreliable")
                         ),
                         by = trial_name]

    # Color coded reliability for plotting
    validity_estimates <-
      validity_estimates[,
                         `:=`(
                           "Col_validity_RT" =
                             data.table::fcase(
                               Rel_validity_RT == "valid", col_valid, # green
                               Rel_validity_RT == "invalid", col_invalid,
                               Rel_validity_RT == "unreliable", col_unreliable),
                           "Col_validity_ERROR" =
                             data.table::fcase(
                               Rel_validity_ERROR == "valid", col_valid, # green
                               Rel_validity_ERROR == "invalid", col_invalid,
                               Rel_validity_ERROR == "unreliable", col_unreliable),
                           "Col_validity_TOTAL" =
                             data.table::fcase(
                               Rel_validity_TOTAL == "valid", col_valid, # green
                               Rel_validity_TOTAL == "invalid", col_invalid,
                               Rel_validity_TOTAL == "unreliable", col_unreliable)
                         ),
                         by = trial_name]

    ### ADD INFO ###
    # IAT information
    validity_estimates <-
      cbind(data.table::data.table("IAT" = IAT_name),
            validity_estimates)

    # Categories
    categories <-
      samples |>
      dplyr::group_by(trial_name) |>
      # Some IATs (e.g., the Race-IAT) have changed the category labels
      dplyr::summarise("block_name" =
                         glue::glue_collapse(unique(block_name),
                                             sep = "; "))

    validity_estimates <-
      dplyr::left_join(x = validity_estimates,
                       y = categories,
                       by = "trial_name")

    # Clean categories
    validity_estimates$block_name <-
      gsub(x = validity_estimates$block_name,
           pattern = '\\[|"|\\]',
           replacement = "")

    # Change the order of the columns for ease of inspection
    data.table::setcolorder(validity_estimates,
                            c("IAT", "trial_name", "block_name"))

    # SAVE DATA
    data.table::fwrite(validity_estimates,
                       file = here::here("datasets",
                                         "Analyses",
                                         glue::glue("validity_estimates_{IAT_name}_{year_of_interest}.csv")))

  } # END IF summary stats exist
}

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
#' Run m out of n bootstraps where the validity of an exemplar is estimated
#' ... in each of the re-samples (NB).
#' m: n_subjects_per_sample
#' n: dependent on the analyzed IAT
#' NB: n_samples
#' @param raw_dat_list raw data PREPARED and EXCLUDED in the form of a list with
#' ... the raw data per participants. This is important because it allows
#' ... independent resampling where a participant can be included in a sample
#' ... multiple times
#' @param n_samples the Number of Bootstraps (i.e., repeated experiments) to conduct
#' @param n_subjects_per_sample the number of participants to select in each re-sample.
#' @return the data summarized per bootstrapped sample

bootstrapped_parameters <- function(raw_dat_list,
                                    n_samples,
                                    n_subjects_per_sample) {

    # Compute statistics per sample
    samples <-
        parallel::mclapply(X = 1:n_samples,
                           # run custom function
                           FUN = function(x) {

                               # SAMPLE
                               # Sample n_subjects from which to evaluate
                               # ... the raw data.
                               # Sample with replacement to achieve independent
                               # ... samples.
                               sample_ids <- sample(x = 1:length(raw_dat_list),
                                                    size = n_subjects_per_sample,
                                                    replace = TRUE)

                               # RECREATE RAW DATA
                               # Index from the input list so that repeated participants
                               # ... are actually included, this does not happen
                               # ... when filtering by session_id.
                               new_raw_dat <-
                                   raw_dat_list[sample_ids] %>%
                                   # Unlist and bind the rows of the tibbles
                                   # ... to create a single data.frame
                                   bind_rows()

                               # COMPUTE PARAMETERS
                               exemplar_stats <-
                                   new_raw_dat %>%
                                   # Compute statistics per exemplar
                                   group_by(trial_name) %>%
                                   # Summarize participant responses
                                   summarize("M_RT" = mean(trial_latency),
                                             "P_ERROR" = (sum(trial_error,
                                                              na.rm = TRUE) /
                                                              n()) * 100) %>%
                                   ungroup() %>%
                                   # DEFINE VALIDITY
                                   # Greenwald2020: "Pilot subjects should be
                                   # ... able to categorize all stimuli in these
                                   # ... two blocks rapidly (600–800 ms for most
                                   # ... young adult subjects) AND with low
                                   # ... error rates (less than 10%).
                                   mutate("validity_RT" =
                                              case_when(M_RT <= 800 ~ "valid",
                                                        M_RT > 800 ~ "invalid"),
                                          "validity_ERROR" =
                                              case_when(P_ERROR <= 10 ~ "valid",
                                                        P_ERROR > 10 ~ "invalid"),
                                          "validity_TOTAL" =
                                              case_when(validity_RT == "valid" &
                                                            validity_ERROR == "valid" ~
                                                            "valid",
                                                        validity_RT == "invalid" |
                                                            validity_ERROR == "invalid" ~
                                                            "invalid"))
                           }) %>% # END SAMPLING
        # Unlist and bind the rows of the tibbles
        bind_rows()

    return(samples)
} # END FUNCTION

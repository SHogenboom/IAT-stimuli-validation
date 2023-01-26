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
#' Run m out of n bootstraps where the validity of an exemplar is estimated
#' ... in each of the re-samples (NB).
#' m: n_subjects_per_sample
#' n: dependent on the analyzed IAT
#' NB: n_samples
#' @param IAT_name name of the IAT in the datasets folder
#' @param year_of_interest
#' @param n_samples the Number of Bootstraps (i.e., repeated experiments) to conduct
#' @param n_subjects_per_sample the number of participants to select in each re-sample.
#' @return the data summarized per bootstrapped sample

bootstrapped_parameters <- function(IAT_name,
                                    year_of_interest,
                                    n_samples,
                                    n_subjects_per_sample) {

  if (!file.exists(here::here("datasets",
                              "Analyses",
                              glue::glue("bootstraps_{IAT_name}_{year_of_interest}.csv")))) {
    #### LOAD DATA ####
    dat_raw <-
      data.table::fread(
        here::here("datasets",
                   "OSF",
                   IAT_name,
                   glue::glue("Raw_{year_of_interest}_cleaned.csv")))

    # Greenwald et al., 2020: "The useful data will come from Blocks 1 and 2
    # ... of the standard procedure"
    dat_raw_12 <-
      dat_raw[block_number %in% c(1, 2), ]

    # UNIQUE PARTICIPANTS
    session_ids <- unique(dat_raw_12$session_id)

    #### BOOTSTRAP STATISTICS ####
    # Compute statistics per sample
    samples <-
      data.table::rbindlist(
        # operating system agnostic parallel implementation of lapply
        # future.apply::future_lapply(
        #  future.seed = TRUE,
        parallel::mclapply(
          mc.cores = parallel::detectCores() - 1,
          X = 1:n_samples,
          # run custom function
          FUN = function(i) {

            # SAMPLE
            # Sample n_subjects from which to evaluate
            # ... the raw data.
            # Sample with replacement to achieve independent
            # ... samples.
            sample_ids <- sample(x = session_ids,
                                 size = n_subjects_per_sample,
                                 replace = TRUE)

            # RECREATE RAW DATA
            # Lapply so that repeated participants
            # ... are actually included, this does not happen
            # ... when simply filtering by session_id.
            new_raw_dat <-
              data.table::rbindlist(
                lapply(X = 1:length(sample_ids),
                       FUN = function(j) {
                         # Select data from the session
                         dat_raw_12[session_id %in% sample_ids[j], ]
                       }
                )
              )

            # COMPUTE PARAMETERS
            exemplar_stats <-
              new_raw_dat[,
                          .("M_RT" = mean(trial_latency),
                            "P_ERROR" = sum(trial_error,
                                            na.rm = TRUE) / .N * 100,
                            "N" = .N,
                            "block_name" = unique(block_name)),
                          by = trial_name]

            # DEFINE VALIDITY
            # Greenwald2020: "Pilot subjects should be
            # ... able to categorize all stimuli in these
            # ... two blocks rapidly (600–800 ms for most
            # ... young adult subjects) AND with low
            # ... error rates (less than 10%).
            exemplar_stats <-
              exemplar_stats[,
                             `:=`(
                               "validity_RT" =
                                 data.table::fcase(
                                   M_RT <= 800, "valid",
                                   M_RT > 800, "invalid"
                                 ),
                               "validity_ERROR" =
                                 data.table::fcase(
                                   P_ERROR <= 10, "valid",
                                   P_ERROR > 10, "invalid"
                                 )
                             )
              ]

            exemplar_stats <-
              exemplar_stats[,
                             "validity_TOTAL" :=
                               data.table::fcase(
                                 validity_RT == "valid" &
                                   validity_ERROR == "valid",
                                 "valid",
                                 validity_RT == "invalid" |
                                   validity_ERROR == "invalid",
                                 "invalid")
              ]
          } # END function
        ) # END lapply
      ) # END rbindlist

    # SAVE DATA
    data.table::fwrite(x = samples,
                       file = here::here("datasets",
                                         "Analyses",
                                         glue::glue("bootstraps_{IAT_name}_{year_of_interest}.csv")))
  } # END IF file.exists
} # END FUNCTION

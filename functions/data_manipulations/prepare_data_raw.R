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

#' Prepare the Raw OSF data
#'
#' @param IAT_name name of the IAT's folder in `datasets` folder
#' @param year_of_interest numeric
#' months_of_interest is derived from the compressed data because test dates are
#' not available in the raw data.
#'
#' OUTPUT: prepared_CSV in `data_path`

prepare_raw <- function(IAT_name,
                        year_of_interest) {
  # SET data_path
  data_path <-
    here::here(
      "datasets",
      "OSF",
      IAT_name
    )

  # CHECK if data already exists
  if (
    !file.exists(
      here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}_prepared.csv")
      )
    )
  ) {
    #### LOAD: compressed data into R memory ####
    # It is not possible to filter `months_of_interest` based on raw data. Instead,
    # ... we include only the raw data from the session_ids which are in the
    # ... prepared compressed data.
    dat_compressed <-
      data.table::fread(
        file = here::here(
          data_path,
          glue::glue("Compressed_{year_of_interest}_prepared.csv")
        ),
        fill = TRUE # deals with missing data.
      )

    #### EXTRACT: the unique session ids ####
    # Proxy for months_of_interest
    session_ids <- dat_compressed[, unique(session_id)]
    rm(dat_compressed) # data no longer needed.

    #### UNZIP: data into temporary folder ####
    # Create a temporary data folder
    tmp_data_path <-
      here::here(
        data_path,
        "tmp_data"
      )
    dir.create(
      path = tmp_data_path,
      showWarnings = FALSE
    )

    # Unzip the contents to the temporary data folder
    utils::unzip(
      zipfile = here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}.zip")
      ),
      exdir = tmp_data_path
    )

    #### LAPPLY PROCEDURES TO EACH FILE ####
    dat_raw <-
      # rbindlist combines all the fread data.table's to a single data.table
      data.table::rbindlist(
        # It is likely that parallel:mclapply only works on Mac
        parallel::mclapply(
          mc.cores = parallel::detectCores() - 1,
          X = list.files(here::here(tmp_data_path),
            full.names = TRUE,
            recursive = TRUE, # nested folders
            pattern = ".txt" # only the text files
          ),
          FUN = function(raw_file) {
            #### LOAD: data into R memory ####
            # The `data.table` format is the fastest way to work with large datasets in R
            # see https://okanbulut.github.io/bigdata/wrangling-big-data.html
            # see https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
            # see https://mgimond.github.io/rug_2019_12/Index.html
            dat <- data.table::fread(
              file = raw_file,
              sep = "\t"
            )

            #### FILTER: session_ids ####
            # Continue with the raw data that belongs to sessions completed during the
            # ... months_of_interest (from compressed data)
            dat <-
              dat[session_id %in% session_ids, ]

            # Some files will not hold any valuable data
            # Return = local to the function
            if (nrow(dat) != 0) {

            #### RESTRUCTURE: session_id and trial_number ####
            # For ease of interpretation get the correct order of trials per pp (session_id).
            dat <-
              dat[order(session_id, trial_number), ]

            #### CLEAN: block- and trial_name's ####
            # For ease of interpretation remove the | from the block and trial names
            dat <-
              dat[, `:=`(
                block_name = gsub(block_name,
                  pattern = "|",
                  replacement = "",
                  fixed = TRUE
                ),
                trial_name = gsub(trial_name,
                  pattern = "|",
                  replacement = "",
                  fixed = TRUE
                )
              )]

            #### CLEAN: remove instruction trials ####
            # instruction trials don't offer us any information
            dat <-
              dat[block_number != 0, ]

            ##### CREATE: new variable with category-exemplar combinations
            # Save the combination of block_name (Category) and trial_name (Exemplar)
            dat <-
              dat[, "Category-Exemplar" :=
                glue::glue_data(
                  .SD,
                  "{block_name} - {trial_name}"
                )]
            } # END nrow_dat
          } # END FUNCTION
        ) # END lapply
      ) # END rbindlist

    #### CLEAN: non-primary task data entries ####
    # From the pilot data it became apparent that some data has ended up
    # ... "in the wrong place". For example, we see trials for Ronald Reagan /
    # ... Donald Trump in the Gender-Career IAT.
    # ... These have received "task_name" -1, but further cleaning may be necessary

    # Determine which task name is "correct" by selecting the task name
    # ... that features most often in the data.
    primary_task_name <-
      dplyr::as_tibble(dat_raw) |>
      # Count the number of times each task name is mentioned
      dplyr::count(task_name) |>
      # Select the 1 task name which has been counted the most (n)
      dplyr::top_n(
        wt = n, # weights = n counts
        n = 1
      ) |> # select top 1
      # Keep only the task name
      dplyr::pull(task_name)

    # Keep only the entries with the primary_task_name
    dat_raw <- dat_raw[task_name == primary_task_name, ]

    # TRIAL CONGRUENCY
    # Note: the block_pairing_definitions - which need to be used to determine
    # ... whether the block was (In)congruent are specified per IAT.
    # In the file `datasets/IAT_trial_congruency.csv`

    dat_trial_congruency <-
      # LOAD trial congruency data
      readr::read_csv(file = here::here(
        "datasets",
        "IAT_trial_congruency.csv"
      ),
      show_col_types = FALSE) |>
      dplyr::filter(task_name == primary_task_name)

    # CHECK: congruency data exists
    if (nrow(dat_trial_congruency) == 0) {
      stop(glue::glue(
        "The (In)congruent block_pairing_definitions have not ",
        "been defined for the '{primary_task_name}': \n",
        glue::glue_collapse(unique(dat_raw$block_pairing_definition),
          sep = "; "
        )
      ))
    }

    dat_raw <-
      dat_raw[, `:=`(
        trail_congruency =
          dplyr::case_when(
            block_pairing_definition %in%
              dat_trial_congruency$congruent ~ "Congruent",
            block_pairing_definition %in%
              dat_trial_congruency$incongruent ~ "Incongruent",
            # All other cases
            TRUE ~ "Other"
          )
      )]

    #### SAVE: data as CSV ####
    data.table::fwrite(
      x = dat_raw,
      file = here::here(
        data_path,
        glue::glue("Raw_{year_of_interest}_prepared.csv")
      )
    )

    #### REMOVE: temporary data ####
    unlink(
      here::here(
        data_path,
        "tmp_data"
      ),
      recursive = TRUE
    )
  } # END IF file exists.
} # END function

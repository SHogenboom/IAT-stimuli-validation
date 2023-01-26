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

#' Prepare the Compressed OSF data
#'
#' @param IAT_name name of the IAT (folder name in `datasets`)
#' @param year_of_interest numeric
#' @param months_of_interest numeric vector of months to retain.
#' With 1 = January - 12 = December.
#'
#' OUTPUT: prepared_CSV in `data_path`

prepare_compressed <- function(IAT_name,
                               year_of_interest,
                               months_of_interest) {
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
        glue::glue("Compressed_{year_of_interest}_prepared.csv")
      )
    )
  ) {
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
        glue::glue("Compressed_{year_of_interest}.zip")
      ),
      exdir = tmp_data_path
    )

    #### LOAD: data into R memory ####
    # The `data.table` format is the fastest way to work with large datasets in R
    # see https://okanbulut.github.io/bigdata/wrangling-big-data.html
    # see https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
    # see https://mgimond.github.io/rug_2019_12/Index.html
    dat <-
      # rbindlist combines all the fread data.table's (for the individual files)
      # ... to a single data.table
      data.table::rbindlist(
        lapply(
          X = list.files(here::here(tmp_data_path),
            full.names = TRUE,
            recursive = TRUE,
            pattern = ".csv"
          ), # only the csv files
          FUN = function(filepath) {
            data.table::fread(filepath)
          }
        )
      )

    #### FILTER: months_of_interest ####
    # Continue only with the months_of_interest where 1 = January and 12 = December
    dat <-
      dat[month %in% months_of_interest, ]

    #### REFACTOR: session_status ####
    # Convert session_status to an easy to interpret factor
    dat <-
      dat[, `:=`(
        session_status = trimws(session_status), # incomplete sessions are empty
        session_status_f = factor(session_status,
          levels = c("", "C"),
          labels = c("Incomplete", "Complete")
        )
      )]

    #### RENAME: number of previous IATS ####
    # Rename for ease of interpretation and consistency across differently
    # ... coded IATs.
    # ASSUMPTION: only one of these 'old' exists in each data set.

    if (IAT_name == "Race") {
      # The race IAT has two columns num_002 and num002 with duplicate data
      # ... to bypass issues later on we remove one of the columns
      dat <- dat[, "num002" := NULL]
    }

    data.table::setnames(
      x = dat,
      old = c("num_002", "num002", "num"), # only 1 in each dataset
      new = rep("n_previous_iats", 3), # repeat three times for the old variables.
      skip_absent = TRUE
    )

    #### SET: Date of IAT Completion ####
    # TEST DATE
    # In order to compute the age of a participant when the test was taken
    # ... the test date needs to be established.
    # Computations are only possible in the correct date format.
    dat <-
      dat[, `:=`(
        # Day the test was completed
        day = ifelse(day < 10,
          glue::glue_data(
            .SD,
            "0{day}"
          ),
          day
        ),
        # Month the test was completed
        month = ifelse(month < 10,
          glue::glue_data(
            .SD,
            "0{month}"
          ),
          month
        ),
        # Store the full test date in the appropriate date format
        test_date = as.Date(
          glue::glue_data(
            .SD,
            "{day}-{month}-{year}"
          ),
          format = "%d-%m-%Y"
        )
      )]

    #### COMPUTE: Age ####
    # BIRTH DATE
    # In order to compute ages, we need to add days to the available
    # ... birth computations - otherwise age computations are not possible
    # see following issue why normal glue approach does not work:
    # https://github.com/Rdatatable/data.table/issues/4879
    dat <-
      dat[, birth_date := glue::glue_data(
        .SD,
        "01-{birthmonth}-{birthyear}"
      )]

    # Remove any invalid birth dates because of missing data
    # ... otherwise errors occur in the computation of age
    dat <-
      dat[, birth_date := ifelse(grepl(
        x = birth_date,
        pattern = "-NA"
      ),
      # Leave empty
      "",
      # Keep birth_data
      birth_date
      )]

    # AGE
    # From 2016 onward participants no longer reported their age,
    # ... but rather their birth date information. This needs to be
    # ... recomputed.
    # Compute the age (in a second / new variable)
    dat <-
      dat[
        ,
        age := lubridate::as.period(
          lubridate::interval(
            as.Date(birth_date, "%d-%m-%Y"), # birth date
            # vs day at which the IAT was completed
            test_date
          ),
          units = "year"
        )@year
      ]

    #### CLEAN: Nationality ####
    # Sometimes the countries have trailing white spaces which have not yet
    # ... been removed upon import.
    dat <-
      dat[, `:=`(
        countrycit_num = trimws(countrycit_num), # remove trailing white spaces
        countryres_num = trimws(countryres_num)
      )]

    #### REDUCE: Unnecessary Columns ####
    # Due to the explicit questions, some data sets have a lot of columns
    # ... which we will not use. These can be removed to reduce memory.
    # We convert back to the Tidyverse for the next two stages because the syntax
    # ... is easiest to understand.
    # print(object.size(dat), unit = "MB") shows no impact on memory

    dat <-
      dplyr::as_tibble(dat) |>
      # Select the wanted columns
      dplyr::select(
        study_name, session_id, session_status,
        day, month, year, test_date, # date information
        # DEMOGRAPHICS
        n_previous_iats,
        dplyr::contains("country"), # all column with "country" in the name
        age,
        # IAT SCORES
        # All columns will follow the same patterns, but the middle name
        # ... changes depending on the content of the IAT
        dplyr::matches("^D_.*_all"), # overall D-scores
        dplyr::matches("^D_.*_36"), # D-scores block 6 - 3
        dplyr::matches("^D_.*_47"), # D-scores blocks 7 - 4
        # IAT statistics
        # Reaction times
        dplyr::matches("Mn_R*_all_"),
        dplyr::matches("SD_R*_all_"),
        dplyr::matches("Mn_R*_correct_"),
        # N Trials
        dplyr::contains("N_"),
        dplyr::contains("N_ERROR_"),
        # Other
        dplyr::contains("Order_"), # order of blocks
        dplyr::contains("pct_"), # percentage of too fast responses with different cut-offs
      )

    #### UNITE: duplicate columns ####
    # Some IATs have multiple forms (e.g., Religion) which
    # ... results in the same value being spread over separate columns
    # Tidyverse vs. Data.table benchmark comparisson:
    # ... https://stackoverflow.com/questions/37545051/data-table-version-of-tidyrunite
    dat <-
      dat |>
      # IAT D-score
      tidyr::unite("IAT_D_Score_all", # name of new column
        dplyr::matches("^D_.*_all"), # input columns
        remove = TRUE, # remove old columns
        na.rm = TRUE
      ) |>
      tidyr::unite("IAT_D_Score_36",
        dplyr::matches("^D_.*_36"), # input columns
        remove = TRUE, # remove old columns
        na.rm = TRUE
      ) |>
      tidyr::unite("IAT_D_Score_47",
        dplyr::matches("^D_.*_47"), # input columns
        remove = TRUE, # remove old columns
        na.rm = TRUE
      ) |>
      # Convert back to numeric
      dplyr::mutate(
        "IAT_D_Score_all" = as.numeric(IAT_D_Score_all),
        "IAT_D_Score_36" = as.numeric(IAT_D_Score_36),
        "IAT_D_Score_47" = as.numeric(IAT_D_Score_36)
      )

    #### SAVE: data as CSV ####
    data.table::fwrite(
      x = dat,
      file = here::here(
        data_path,
        glue::glue("Compressed_{year_of_interest}_prepared.csv")
      )
    )

    #### REMOVE: temporary data ####
    rm(dat)
    unlink(
      here::here(
        data_path,
        "tmp_data"
      ),
      recursive = TRUE
    )
  } # END IF file exists.
} # END function

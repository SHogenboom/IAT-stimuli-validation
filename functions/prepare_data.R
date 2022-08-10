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
#' Prepare raw data sets so that they all follow the same format
#' For example, restructuring, defining trial congruency etc.
#' @param raw_dat raw data as attained from Project Implicit - without any changes
#' @return data.frame with prepared raw data

prepare_raw <- function(raw_dat) {

    prepared_raw <-
        raw_dat %>%
        # RESTRUCTURE
        # For ease of interpretation get the correct order of trials per pp (session_id).
        arrange(session_id, trial_number) %>%
        # CLEANING
        # For ease of interpretation remove the | from the block and trial names
        mutate("block_name" = gsub(block_name,
                                   pattern = "|",
                                   replacement = "",
                                   fixed = TRUE),
               "trial_name" = gsub(trial_name,
                                   pattern = "|",
                                   replacement = "",
                                   fixed = TRUE)) %>%
        # REMOVE instruction trials - these don't offer us any information
        filter(block_number != 0) %>%
        # NEW VARIABLES
        # Save the combination of block_name (Category) and trial_name (Exemplar)
        mutate("Category-Exemplar" = glue::glue("{block_name} - {trial_name}"))

    # PRIMARY TASK
    # From the pilot data it became apparent that some data has ended up
    # ... "in the wrong place". For example, we see trials for Ronald Reagan /
    # ... Donald Trump in the Gender-Career IAT.
    # ... These have received "task_name" -1, but further cleaning may be necessary

    # Determine which task name is "correct" by selecting the task name
    # ... that features most often in the data.
    primary_task_name <-
        prepared_raw %>%
        # Count the number of times each task name is mentioned
        count(task_name) %>%
        # Select the 1 task name which has been counted the most (n)
        top_n(wt = n, # weights = n counts
              n = 1) %>% # select top 1
        # Keep only the task name
        pull(task_name)

    # REMOVE any trials that do not belong to the primary IAT
    prepared_raw <-
        prepared_raw %>%
        filter(task_name == primary_task_name)

    # TRIAL CONGRUENCY
    # Note: the block_pairing_definitions - which need to be used to determine
    # ... whether the block was (In)congruent are specified per IAT.
    # Consequently, this code has been prepared, but needs to be checked and
    # ... likely altered once the full data has been attained.
    if (primary_task_name == "careeriat") {

        prepared_raw <-
            prepared_raw %>%
            # Determine whether the trial was part of a (in)congruently paired block
            mutate("trial_congruency" =
                       case_when(block_pairing_definition ==
                                     "Career/Male,Family/Female" ~ "Congruent",
                                 block_pairing_definition ==
                                     "Family/Male,Career/Female" ~ "Incongruent",
                                 # all other cases (e.g., instructions)
                                 TRUE ~ "Other"))
    } else {
        stop(glue::glue("The (In)congruent block_pairing_definitions have not ",
                        "been defined for the '{primary_task_name}' IAT"))
    } # END IF primary_task_name

return(prepared_raw)

} # END prepare_raw FUNCTION

#' GOAL
#' Prepare the compressed data sets so that they all follow the same format
#' For example, rename the D-score and recode session completion
#' @param compressed_dat unprepared compressed data as downloaded from
#' ... Project Implicits' OSF.
#' @return data.frame with prepared compressed data
prepare_compressed <- function(compressed_dat) {

    prepared_compressed <-
        compressed_dat %>%
        # CLEANING
        # Whether or not the full IAT was completed
        # Convert for ease of interpretation
        mutate("session_status" = trimws(session_status), # remove trailing white spaces
               "session_status" =
                   factor(session_status,
                          levels = c("", "C"),
                          labels = c("Incomplete", "Complete")))

    # PRIMARY TASK
    # From the pilot data it became apparent that some data has ended up
    # ... "in the wrong place". For example, we see trials for Ronald Reagan /
    # ... Donald Trump in the Gender-Career IAT.
    #' To ensure that this did not occur in the compressed data we determine
    #' ...  the most prominent IAT, and exclude the data from participants
    #' ... of other IATs.

    # Determine which task name is "correct" by selecting the task name
    # ... that features most often in the data.
    primary_study_name <-
        prepared_compressed %>%
        count(study_name) %>%
        # Select the 1 task name which has been counted the most (n)
        top_n(wt = n, # weights = n counts
              n = 1) %>% # select top 1
        # Keep only the task name
        pull(study_name)

    # Number of previous IATs
    # Rename for ease of interpretation and consistency across differently
    # ... coded IATs.
    if (any(colnames(compressed_dat) == "num_002")) {
        prepared_compressed <-
            prepared_compressed %>%
            rename("n_previous_iats" = num_002)
    } else if (any(colnames(compressed_dat) == "num002")) {
        prepared_compressed <-
            prepared_compressed %>%
            rename("n_previous_iats" = num002)
    } else if (any(colnames(compressed_dat) == "num")) {
        prepared_compressed <-
            prepared_compressed %>%
            rename("n_previous_iats" = num)
    } else {
        stop("Number of Previous IATs could not be coded")
    } # END IF

    # NEW VARIABLES
    prepared_compressed <-
        prepared_compressed %>%
        mutate(
            # BIRTH DATE
            # In order to compute ages, we need to add days to the available
            # ... birth computations - otherwise age computations are not possible
            "birth_date" = glue::glue("01-{birthmonth}-{birthyear}"),
            # Remove any invalid birth dates because of missing data
            # ... otherwise errors occur in the computation of new_age
            "birth_date" = ifelse(grepl(x = birth_date,
                                        pattern = "-NA"),
                                  "",
                                  birth_date),
            # TEST DATE
            # In order to compute the age of a participant when the test was taken
            # ... the test date needs to be established.
            # Computations are only possible in the correct date format.
            "day" = ifelse(day < 10,
                           glue::glue("0{day}"),
                           day),
            "month" = ifelse(month < 10,
                             glue::glue("0{month}"),
                             month),
            # Store the full test date in the appropriate date format
            "test_date" = as.Date(glue::glue("{day}-{month}-{year}"),
                                  format = "%d-%m-%Y"),
            # AGE
            # From 2016 onward participants no longer reported their age,
            # ... but rather their birth date information. This needs to be
            # ... recomputed.
            # Compute the age (in a second / new variable)
            "age" = lubridate::as.period(lubridate::interval(
                as.Date(birth_date, "%d-%m-%Y"), # birth date
                # vs day at which the IAT was completed
                test_date),
                units = "year")@year
        )

    # CLEAN NATIONALITY
    # Sometimes the countries have trailing white spaces which have not yet
    # ... been removed upon import.
    prepared_compressed <-
        prepared_compressed %>%
        mutate("countrycit_num" = trimws(countrycit_num), # remove trailing white spaces
               "countryres_num" = trimws(countryres_num)) # remove trailing white spaces

    # REDUCE DATA
    # Due to the explicit questions, some data sets have a lot of columns
    # ... which we will not use. These can be removed to reduce memory.
    prepared_compressed <-
        prepared_compressed %>%
        select(study_name, session_id, session_status,
               day, month, year, test_date, # date information
               # DEMOGRAPHICS
               n_previous_iats,
               contains("country"), # all column with "country" in the name
               age,
               # IAT SCORES
               # All columns will follow the same patterns, but the middle name
               # ... changes depending on the content of the IAT
               matches("^D_.*_all"), # overall D-scores
               matches("^D_.*_36"), # D-scores block 6 - 3
               matches("^D_.*_47"), # D-scores blocks 7 - 4
               # IAT statistics
               # Reaction times
               matches("Mn_R*_all_"),
               matches("SD_R*_all_"),
               matches("Mn_R*_correct_"),
               # N Trials
               contains("N_"),
               contains("N_ERROR_"),
               # Other
               contains("Order_"), # order of blocks
               contains("pct_"), # percentage of too fast responses with different cut-offs
               )

    # In addition, some IATs have multiple forms (e.g., Religion) which
    # ... results in the same value being spread over separate columns
    prepared_compressed <-
        prepared_compressed %>%
        # IAT D-score
        unite("IAT_D_Score_all", # name of new column
              matches("^D_.*_all"), # input columns
              remove = TRUE, # remove old columns
              na.rm = TRUE) %>%
        unite("IAT_D_Score_36",
              matches("^D_.*_36"), # input columns
              remove = TRUE, # remove old columns
              na.rm = TRUE) %>%
        unite("IAT_D_Score_47",
              matches("^D_.*_47"), # input columns
              remove = TRUE, # remove old columns
              na.rm = TRUE) %>%
        # Convert back to numeric
        mutate("IAT_D_Score_all" = as.numeric(IAT_D_Score_all),
               "IAT_D_Score_36" = as.numeric(IAT_D_Score_36),
               "IAT_D_Score_47" = as.numeric(IAT_D_Score_36))

return(prepared_compressed)

} # END prepare_compressed FUNCTION

#' GET VALIDITY ESTIMATES
#'
#' Combine the validity estimates computed for each IAT
#'
#' @param year_of_interest

get_all_validity_estimates <- function(year_of_interest) {

  # GET FILE PATHS
  # Of validityestimates per IAT for the `year_of_interest`
  validity_estimate_paths <-
    list.files(
      # Loop over Analyses data folder
      here::here(
        "datasets",
        "Analyses"
      ),
      # Use the data from the year_of_interest
      pattern = "validity_estimates",
      full.names = TRUE
    )

  # COMBINE DATA
  # rbindlist combines all the fread data.table's to a single data.table
  all_validity_estimates <-
    data.table::rbindlist(
      lapply(
        X = validity_estimate_paths,
        FUN = function(filepath) {
          # Only load the data from the correct year_of_interest.
          if (grepl(x = filepath,
                    pattern = year_of_interest)) {
            # Load the data as data.table
            data.table::fread(filepath,
                              verbose = FALSE)
          }

        }
      )
    ) |>
    # Convert to tibble for easier processing
    dplyr::as_tibble()

  return(all_validity_estimates)

}

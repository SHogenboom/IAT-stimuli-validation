#' GET FULL SAMPLE STATS
#'
#' Combine the in-/exclusion statistics of all IATs to a single data.table
#' @param year_of_interest
#' @param type "Compressed" or "Raw"; defaults to "Compressed".
#' The type of in-/exclusion data to retrieve

get_full_sample_stats <- function(year_of_interest,
                                  type = "Compressed") {

  # GET FILE PATHS
  # Of in-/exclusion statistics per IAT for the `year_of_interest`
  exclusion_files <-
    list.files(
      # Loop over OSF data folder
      here::here(
      "datasets",
      "OSF"
    ),
    # Look in each IAT subfolder
    recursive = TRUE,
    # Use the compressed data from the year_of_interest
    pattern =
      glue::glue("{type}_{year_of_interest}_exclusion_stats.csv"),
    full.names = TRUE
    )


  # COMBINE DATA
  # rbindlist combines all the fread data.table's to a single data.table
  exclusion_stats <-
    data.table::rbindlist(
    lapply(
      X = exclusion_files,
      FUN = function(filepath) {
        # Load the data as data.table
        data.table::fread(filepath,
                          verbose = FALSE)
      }
    )
  ) |>
    # Convert to tibble for easier multi-column filtering
    dplyr::as_tibble()

  return(exclusion_stats)
}

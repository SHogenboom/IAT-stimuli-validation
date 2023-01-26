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
#' Download Raw and Compressed Project Implicit data
#' ... directly from the Open Science Framework (as .zip).
#'
#' The relevant URLs have been manually looked up and saved in
#' a csv file: datasets/OSF-urls.csv.
#' When adding new URLs please make sure to include the URL to the
#' ... Compressed CSV rather than SPSS data.
#'
#' @param IAT_name character; most basic indicator of a unique IAT (e.g., 'Skin' for Skin Tone IAT)
#' @param year_of_interest number; the year from which the data should be downloaded
#' @param output_path file path to where the data should be downloaded.
#' ... A subfolder for the corresponding IAT is automatically created.
#'
retrieve_IAT_data <- function(IAT_name,
                              year_of_interest,
                              output_path) {
  #### CREATE: OUTPUT DIRECTORY ####
  # Create an output path that is specific for the retrieved IAT
  output_path <- here::here(
    output_path,
    IAT_name
  )

  # Existing folders do not get overwritten.
  dir.create(
    path = output_path,
    showWarnings = FALSE # does not cause an error if folder already exists
  )

  #### GET: OSF URLs ####
  if (!file.exists(here::here(
    "datasets",
    "OSF-urls.csv"
  ))) {
    stop("You need to have a file with download links in `datasets` called `OSF-urls.csv`")
  }

  osf_urls <-
    # DOWNLOAD all urls
    readr::read_csv(
      here::here(
        "datasets",
        "OSF-urls.csv"
      ),
      show_col_types = FALSE
    ) |>
    # CONVERT to download links
    dplyr::mutate(
      "Compressed" = gsub(
        x = Compressed,
        pattern = "https://osf.io/",
        replacement = "https://osf.io/download/"
      ),
      "Raw" = gsub(
        x = Raw,
        pattern = "https://osf.io/",
        replacement = "https://osf.io/download/"
      )
    ) |>
    # KEEP those of the relevant IAT
    dplyr::filter(
      IAT == IAT_name,
      Year == year_of_interest
    )

  if (nrow(osf_urls) != 1) {
    stop(glue::glue("There are {nrow(osf_urls)} download urls for {IAT_name} ({year_of_interest})"))
  }

  #### DOWNLOAD: COMPRESSED DATA ####
  # Only download when file hasn't yet been downloaded
  if (!file.exists(
    here::here(
      output_path,
      glue::glue("Compressed_{year_of_interest}.zip")
    )
  )) {
    download.file(
      url = osf_urls$Compressed,
      destfile = here::here(
        output_path,
        glue::glue("Compressed_{year_of_interest}.zip")
      )
    )
  } # END if file.exists Compressed

  #### DOWNLOAD: RAW DATA ####
  # Only download when file hasn't yet been downloaded
  if (!file.exists(
    here::here(
      output_path,
      glue::glue("Raw_{year_of_interest}.zip")
    )
  )) {
    download.file(
      url = osf_urls$Raw,
      destfile = here::here(
        output_path,
        glue::glue("Raw_{year_of_interest}.zip")
      )
    )
  } # END if file.exists Raw
} # END retrieve_data

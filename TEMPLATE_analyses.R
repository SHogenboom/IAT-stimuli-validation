#' GOAL
#' This template serves as a guide for researchers who wish to analyze their
#' ... own IAT data, or a different data set with Project Implicit data.
#'
#' NOTE: This template works with our project file structure. If you wish to
#' ... analyse your own data changes need to be made to the filepaths and/or
#' ... names of the files.
#'
#' NOTE: these functions were prepared so that they worked with
#' ... the data we received from Project Implicit (Nov' 2020).
#' It is possible that changes in variable names (e.g., prior to 2017) may
#' ... cause the analyses to break.

#### LOAD DATA ####
# RAW DATA
# Trial-by-trial data which includes response times and accuracy information
# This data was received after submitting a data request with Project Implicit
raw_dat <-
    readr::read_delim(file = here::here("datasets",
                                        "original",
                                        "RAW_iat_NovDec2019.txt"),
                      delim = "\t", # tab separated file
                      trim_ws = TRUE, # remove extra white spaces from input
                      progress = TRUE, # set to false for RMarkdown knitting
                      skip_empty_rows = TRUE)  # prevent errors when empty rows occur

# COMPRESSED DATA
# Aggregated data per participant including demographic information and
# ... explicit attitudes
# This data was downloaded from Project Implicits' OSF (https://osf.io/y9hiq/)
compressed_dat <-
    # LOAD DATA (from SPSS file)
    suppressWarnings(
        foreign::read.spss(file =
                               here::here("datasets",
                                          "original",
                                          "Gender-Career IAT.public.2019.sav"),
                           to.data.frame = TRUE))

#### PREPARE DATA ####
#' We apply some preprocessing steps to the data such as computing the age
#' ... of participants when they took the IAT
#' see functions/prepare_data.R

raw_dat <- prepare_raw(raw_dat = raw_dat)
compressed_dat <- prepare_compressed(compressed_dat = compressed_dat)

#### EXCLUSION CRITERIA ####
#' We apply exclusion criteria to both data sets separately
#' ... and remove the data vice versa.
#' see functions/apply_exclusion_criteria.R
#'
#' NOTE: this output generates a list with two elements
#' ... (1) A table with the exclusion statistics that can be used for plotting
#' ... (2) The cleaned data

# Apply the exclusion criteria
raw_list <-
    raw_dat_list <- exclusion_raw(raw_dat = raw_dat)
compressed_list <-
    exclusion_compressed(compressed_dat = compressed_dat)

# Save the cleaned data
raw_dat <- raw_list$cleaned_dat
compressed_dat <- compressed_list$cleaned_dat

# Remove participants vice versa
raw_dat <- raw_dat[raw_dat$session_id %in% compressed_dat$session_id, ]
compressed_dat <- compressed_dat[compressed_dat$session_id %in% raw_dat$session_id, ]

#### EXCLUSION PLOTS ####
#' If you want to visualize the impact of the exclusion criteria you may execute
#' ... the following code segments

# Visualize the exclusion criteria impact
raw_exclusion_plot <-
    plot_exclusion_criteria(exclusion_data = raw_list$exclusion_data,
                            datType = "raw")

compressed_exclusion_plot <-
    plot_exclusion_criteria(exclusion_data = compressed_list$exclusion_data,
                            datType = "compressed")

#### M OUT OF N BOOTSTRAP ####
#' We use the prepared and cleaned data to conduct our m out of n bootstraps.
#' In these analyze we re-sample m participants from the n totally available
#' ... participants. For each of these samples we compute the average response
#' ... time and error percentage per exemplar. We then judge whether - within -
#' ... the sample the exemplar was valid (RT <= 800; Error <= 10%) or invalid.
#'
#' see functions/bootstrapped_parameters.R
#'
#' NOTE: depending on the number of re-samples this analyses takes extremely long
#' ... we therefore set the variable to a small number in case you accidentally
#' ... run the entire script.
#'
#' NOTE: the m out of n bootstrap should be applied to data sets where the total
#' ... number of participants (n) is larger than the sampled number of participants
#' ... (m). You will however NOT receive an error message if this is not the case
#' ... as technically it is still possible to execute.

# Prepare data
list_raw_12 <-
    raw_dat %>%
    # Greenwald et al., 2020: "The useful data will come from Blocks 1 and 2
    # ... of the standard procedure"
    filter(block_number %in% c(1, 2)) %>%
    # Convert to list to allow for repeated selection of the same participant
    # It ultimately more efficient when applying bootstrap m out of n resamples
    group_by(session_id) %>%
    group_split()

# M out of N Bootstraps
resamples_dat <-
    bootstrapped_parameters(raw_dat_list = list_raw_12,
                            # Number of times to re-sample
                            n_samples = 100,
                            # Number of participants to sample with replacement
                            # ... to attain independent samples
                            n_subjects_per_sample = n_subjects_per_sample)

#### RELIABLE VALIDITY ####
#' In the previous analyses we computed whether an exemplar was valid within
#' ... each of the samples. We now determine the reliability of this decision.
#' In these analyses we class exemplars are reliably VALID if the exemplar is
#' ... classed valid in 95% or more of the samples. An exemplar is reliably
#' ... INVALID if the exemplar is classed valid in 5% or less of the samples.
#' ... In all other cases an exemplar is classed as UNRELIABLE
#'
#' see functions/validity_estimates.R

exemplar_validity <-
    exemplar_validity_estimates(samples = resamples_dat,
                                perc_limit = 95)

#' Validity texts. A summary of the results in a form that could be included
#' ... in a manuscript
#' see functions/results_text.R
validity_text <- results_text(exemplar_stats = exemplar_validity)

#### EXEMPLAR VALIDITY PLOT ####
#' Visualize the outcome of the analyses in a combined plot.

# Get the category information for clustered plotting
categories <-
    raw_dat %>%
    group_by(trial_name) %>%
    summarize("category" = unique(block_name)) %>%
    ungroup()

# Add the data to the exemplar stats
exemplar_validity <-
    left_join(exemplar_validity,
              categories,
              by = "trial_name") %>%
    # Restructure for ease of interpretation
    arrange(category, trial_name)

# Generate the plot
# NOTE: plot is saved as PDF to ensure correct proportions
results_plot(exemplar_samples = resamples_dat,
             exemplar_stats = exemplar_validity,
             filename = here::here("plots",
                                   "pilot-results.pdf"))

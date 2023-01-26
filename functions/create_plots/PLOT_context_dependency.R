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

#' PLOT the context dependency for a subset of stimuli. Only show the RT & ERROR
#' ... plots to limit space.
#' Plots show the raw data from 9/10 IATs as well as the 95% CI of the Mean
#'
#' @param multi_IAT_stimuli df of stimuli which are used in multiple IATs
#' @param validity_estimates df of the validity estimates - only a subset is used.
#' @param stimulus_gap the number of stimuli to skip in between stimuli.
#' This improves the legibility of the plot while still showing the general pattern
#' @param plot_heigth the output height of the plot - for the generated png.
#' Needs to be adjusted dependent on the stimulus gap.
all_context_dependency_plots <- function(multi_IAT_stimuli,
                                         validity_estimates,
                                         stimulus_gap,
                                         plot_heigth) {

  #### CREATE PLOTS ####
  context_dependency_RT <- plot_context_dependency(multi_IAT_stimuli,
                                                   validity_estimates,
                                                   DV = "RT",
                                                   stimulus_gap)
  context_dependency_ERROR <- plot_context_dependency(multi_IAT_stimuli,
                                                      validity_estimates,
                                                      DV = "ERROR",
                                                      stimulus_gap)

  #### COMBINE PLOTS ####
  # Extract common legend
  shared_legend <-
    cowplot::get_legend(context_dependency_RT)

  # Combine plots
  combined_plot <-
    cowplot::plot_grid(
      shared_legend,
      context_dependency_RT +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = ggplot2::element_blank()),
      context_dependency_ERROR + ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = ggplot2::element_blank()),
      # Three rows
      nrow = 3,
      # Assign heights of the plots, legend should take up less space
      rel_heights = c(0.1, 1, 1),
      # Align the plots left
      axis = "l")

  dir.create(here::here("plots",
                        "context_dependency"),
             showWarnings = FALSE)

  # Save plot for later use.
  cowplot::ggsave2(combined_plot,
                   filename = here::here("plots",
                                         "context_dependency",
                                         "context_dependency_small.png"),
                   height = plot_heigth,
                   units = "cm",
                   bg = "white")
}

#' Create a single context dependency plot
#'
#' @param DV the dependent variable ("RT"; "ERROR"; "TOTAL")
#'
#' @return ggplot

plot_context_dependency <- function(multi_IAT_stimuli,
                                    validity_estimates,
                                    DV,
                                    stimulus_gap) {

  #### VALIDITY ESTIMATES ####
  # Extract the relevant stimuli, compute the M/SD percentage valid
  multi_IAT_validity_estimates <-
    validity_estimates |>
    # Keep only the data from stimuli in multiple IATs
    dplyr::filter(trial_name %in% multi_IAT_stimuli$trial_name) |>
    # Compute the mean/SD percentage valid per stimulus
    dplyr::group_by(trial_name) |>
    dplyr::mutate(
      "M" = mean(get(glue::glue("Perc_validity_{DV}"))),
      "SD" = sd(get(glue::glue("Perc_validity_{DV}"))),
      "perc_valid" = get(glue::glue("Perc_validity_{DV}"))
    ) |>
    dplyr::ungroup()

  # PLOT SUBSET
  # In total there are 70 stimuli to visualize - this doesn't fit nicely on
  # ... any screen. We therefore plotted every third stimulus so as to still
  # ... show the general trends.
  stimuli <-
    multi_IAT_validity_estimates |>
    dplyr::arrange(desc(M)) |>
    dplyr::select(trial_name) |>
    unique() |>
    dplyr::pull()

  stimuli_selection <-
    c(stimuli[1],
      stimuli[seq(from = 2,
                  to = length(stimuli) - 1,
                  by = stimulus_gap)],
      stimuli[length(stimuli)])

  multi_IAT_validity_estimates <-
    multi_IAT_validity_estimates[multi_IAT_validity_estimates$trial_name %in%
                                   stimuli_selection, ]

  # ORDER BY percentage_valid
  # dense rank functions provide a rank without 'gaps'
  # using data.table::frank instead of dplyr::dense_rank allows us to enter
  # ... multiple sorting columns. This is relevant because in the ERROR
  # ... criterion, two stimuli have the exact same M_ERROR.
  # With dplyr::dense_rank these would receive the same rank and be plotted
  # ... on top of eachother.
  multi_IAT_validity_estimates$PosY <-
    data.table::frank(multi_IAT_validity_estimates,
                      M,
                      trial_name,
                      ties.method = "dense"
    )

  # Recalculate PosY for plotting the rainclouds
  # Each raincloud plot ranges from 0.5 - 2.5
  # The first element can stay where it is, only the others need to be replaced
  multi_IAT_validity_estimates$PosY <-
    multi_IAT_validity_estimates$PosY + 3

  # SUMMARY STATISTICS
  summary_stats <-
    Rmisc::summarySE(data = multi_IAT_validity_estimates,
                     measurevar = glue::glue("Perc_validity_{DV}"),
                     groupvars = "trial_name") |>
    dplyr::left_join(multi_IAT_validity_estimates |>
                       dplyr::select("trial_name",
                                     "PosY") |>
                       unique(),
                     by = "trial_name")

    # INITIALIZE
    ggplot2::ggplot(data = multi_IAT_validity_estimates,
                    ggplot2::aes(x = get(glue::glue("Perc_validity_{DV}")))) +
      # RELIABILITY LABELS
      # Vertical lines plotted before boxplots to get clearer image.
      # < 5% = reliably invalid, > 95% is reliably valid
      ggplot2::geom_vline(
        xintercept = c(5, 95),
        col = "#0072B2" # dark-blue
      ) +
    ggplot2::geom_errorbar(data = summary_stats,
                           # Mean - 95% CI // Mean + 95% CI
                           ggplot2::aes(xmin = get(glue::glue("Perc_validity_{DV}")) - ci,
                                        xmax = get(glue::glue("Perc_validity_{DV}")) + ci,
                                        # Adjust position to be on boxplot
                                        y = PosY + 0.3),
                           width = .2,
                           color = "grey40") +
      # ERRORBAR (95% CI of MEAN)
      ggplot2::geom_point(data = summary_stats,
                          ggplot2::aes(x = get(glue::glue("Perc_validity_{DV}")),
                                       y = PosY + 0.3),
                          size = 1.5,
                          color = "grey30") +
    # RAW DATA
  ggplot2::geom_jitter(
    # Adjust relative position (with 1 being center)
    ggplot2::aes(y = PosY - 0.3),
    height = 0.2,
    size = 0.5,
  ) +
    # STYLING
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      # Add a axis tick for each 10%
      breaks = seq(from = 0,
                   to = 100,
                   by = 10),
      # Do not plot 5% breaks
      minor_breaks = FALSE,
      # Force consistent axis - even if only 90-100% is used
      limits = c(0, 100),
      # Prevent out-of-bounds jitters
      oob = scales::squish,
      # Add percentage sign to axis labels
      labels = function(x) paste0(x, "%"),
      # Reduce spacing around the axes
      expand = c(0, 2),
      # ADD validity indications above plot
      sec.axis =
        ggplot2::sec_axis(
          trans = ~.,
          breaks = c(2, 50, 96),
          labels = c("Invalid", "Unreliable", "Valid")
        )) +
    ggplot2::scale_y_continuous(
      # Add axis ticks for each stimulus
      breaks = unique(multi_IAT_validity_estimates$PosY),
      labels = unique(multi_IAT_validity_estimates$trial_name),
      minor_breaks = FALSE
    ) +
    ggplot2::labs(x = "Percentage Valid",
                  y = ggplot2::element_blank(),
                  title = DV) +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(size = 10))
}

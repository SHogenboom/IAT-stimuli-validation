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

#' COMBINED RAINCLOUD PLOT
#'
#' Combine three plots showing the validity distributions for RT, ERROR, and TOTAL
plot_distributions_RT_ERROR_TOTAL <- function(validity_estimates,
                                              IAT_name = NA) {

  #### DETERMINE SUBSET ####
  # Apply IAT filter if an IAT was provided
  if (is.na(IAT_name)) {
    sub_validity_estimates <- validity_estimates
  } else {
    sub_validity_estimates <-
      validity_estimates |>
      dplyr::filter(IAT == IAT_name)
  }

  #### CREATE INDIVIDUAL PLOTS ####
  # Create raincloud plots of the percentage valid per criterion
  plot_RT_full <-
    plot_one_distribution(sub_validity_estimates,
                          DV = "RT")

  plot_ERROR_full <- plot_one_distribution(sub_validity_estimates,
                                           DV = "ERROR")

  plot_TOTAL_full <- plot_one_distribution(sub_validity_estimates,
                                           DV = "TOTAL")

  #### COMBINE PLOTS ####
  # Extract common legend
  shared_legend <-
    cowplot::get_legend(plot_RT_full)

  # Combine plots
  combined_raincloud_plot <-
    cowplot::plot_grid(
      shared_legend,
      plot_RT_full +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = ggplot2::element_blank()),
      plot_ERROR_full + ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = ggplot2::element_blank()),
      plot_TOTAL_full + ggplot2::theme(legend.position = "none"),
      # Three rows
      nrow = 4,
      # Assign heights of the plots, legend should take up less space
      rel_heights = c(0.2, 1, 1, 1),
      # Align the plots left
      axis = "l")

  dir.create(here::here("plots",
                        "validity_estimates"),
             showWarnings = FALSE)

  # Save plot for later use.
  cowplot::ggsave2(combined_raincloud_plot,
                   filename = here::here("plots",
                                         "validity_estimates",
                                         glue::glue("{ifelse(is.na(IAT_name), 'All', IAT_name)}.png")),
                   height = 15,
                   units = "cm",
                   bg = "white")

}

#' RAINCLOUD PLOT
#'
#' GOAL: Visualize the validity_estimates as
#' 1. Violin/Density cures
#' 2. Boxplots with summary validity_estimates
#' 3. Raw (jittered) validity_estimates.
#'
#' @param sub_validity_estimates the validity_estimates you want to plot.
#' Note plot is optimized for percentages (0 - 100)
#' @param DV the dependent variable - plotted on the x-axis
#'
plot_one_distribution <- function(sub_validity_estimates,
                                  DV) {

  #### COMPUTE SUMMARY STATISTICS ####
  summary_stats <- Rmisc::summarySE(data = sub_validity_estimates,
                                    measurevar = glue::glue("Perc_validity_{DV}"))

  #### CREATE PLOT ####
 raincloud_plot <-
    # INITIALIZE
    ggplot2::ggplot(data = sub_validity_estimates,
                    ggplot2::aes(x = get(glue::glue("Perc_validity_{DV}")))) +
    # DENSITY
    ggdist::stat_halfeye(
      # Adjust relative position (with 1 being center)
      ggplot2::aes(y = 1.4),
      # place on top
      side = "top",
      # adjust position
      # justification = -0.2,
      # remove automatically added boxplot
      .width = 0,
      point_color = NA,
      # flatten
      # scale = 0.5,
      alpha = 0.5
    ) +
    # BOXPLOT
    ggplot2::geom_boxplot(
      # Adjust relative position (with 1 being center)
      ggplot2::aes(y = 1),
      # reduce size
      width = .2,
      # remove outliers
      outlier.colour = NA,
      # make slightly transparant
      alpha = 0
    ) +
    # ERRORBAR (95% CI of MEAN)
    ggplot2::geom_point(data = summary_stats,
                        ggplot2::aes(x = get(glue::glue("Perc_validity_{DV}")),
                                     y = 1.25),
                        size = 2) +
    ggplot2::geom_errorbar(data = summary_stats,
                           # Mean - 95% CI // Mean + 95% CI
                           ggplot2::aes(xmin = get(glue::glue("Perc_validity_{DV}")) - ci,
                                        xmax = get(glue::glue("Perc_validity_{DV}")) + ci,
                                        # Adjust position to be on boxplot
                                        y = 1.25),
                           width = .2) +
    # DATA POINTS
    # ggdist::stat_dots(
    #   # place on bottom
    #   side = "bottom",
    #   # Adjust position
    #   justification = 1.1,
    #   # group observations
    #   binwidth = .25,
    #   # Give the corresponding color
    #   ggplot2::aes(fill = get(glue::glue("Rel_validity_{DV}"))),
    #   dotsize = 10,
    #   size = 0.1
    # ) +
    ggplot2::geom_jitter(
      # Adjust relative position (with 1 being center)
      ggplot2::aes(y = 0.6,
                   # Give the reliability color
                   col = get(glue::glue("Rel_validity_{DV}"))),
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
      expand = c(0, 2)) +
    ggplot2::labs(x = "Percentage Valid",
                  y = ggplot2::element_blank(),
                  title = DV,
                  col = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_blank(),  #remove y axis labels
                   axis.ticks.y = ggplot2::element_blank()  #remove y axis ticks
                   ) +
    # Custom color profile
    ggplot2::scale_color_manual(values = c("valid" = "#009E73", # green
                                  "invalid" = "#D55E00", # red
                                  "unreliable" = "#000000"), # black
                       breaks = c("valid", "invalid", "unreliable")) +
   ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 5)))

  return(raincloud_plot)
}


plot_IAT_distributions <- function(validity_estimates,
                                   included_IATs) {
  # Initialize plot list
  IAT_plots <- list()

  # Create a 3-raincloud plot for each IAT
  for (IAT_name in included_IATs) {
    IAT_plots[[IAT_name]] <-
      plot_distributions_RT_ERROR_TOTAL(validity_estimates,
                             IAT_name)
  }

}

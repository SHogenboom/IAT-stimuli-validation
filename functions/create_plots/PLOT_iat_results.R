#' GOAL
#' Visualize the distribution of the response parameters in the different samples
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
#' ... and visualize the overall reliability of the (in)validity judgment
#' All plot a final comparison which allows one to see the final verdict on
#' ... the validity of exemplars per category
#' @param IAT the name of the IAT for which the results should be plotted
#' @param year_of_interest the year of interest from which to plot the data
#' @param plotHeight should be dependent on the number of stimuli to visualize in
#' ... a single plot. (0.8 x nrow(exemplar_stats) for GC-IAT)
#' @return saves a png in 'plots/IATs/...'

results_plot <- function(IAT,
                         year_of_interest,
                         plotHeight) {

    #### LOAD DATA ####
    # Summary Statistics
    exemplar_stats <-
      data.table::fread(
        here::here("datasets",
                   "Analyses",
                   glue::glue("validity_estimates_{IAT}_{year_of_interest}.csv"))) |>
      # Restructure for consistent plotting
      dplyr::arrange(block_name, desc(trial_name)) |>
      # Add index for plotting
      dplyr::mutate(
        # Give each category a unique value (for plotting)
        "category_id" = as.numeric(factor(block_name)) - 1,
        # Give each exemplar a unique plotting position within that category
        "posY" = 1:dplyr::n() + category_id)

    # Raw Sample Data
    exemplar_samples <-
      data.table::fread(
        here::here("datasets",
                   "Analyses",
                   glue::glue("bootstraps_{IAT}_{year_of_interest}.csv")))

    # Combine data for plotting purposes
    exemplar_samples <-
      dplyr::left_join(exemplar_samples,
                       exemplar_stats[, c("trial_name", "posY")],
                       by = "trial_name")

    # CATEGORIES
    categories <-
      exemplar_stats |>
      dplyr::group_by(block_name) |>
      dplyr::summarize("posY" = mean(posY)) |>
      dplyr::ungroup() |>
      dplyr::mutate("block_name" =
                      gsub(x = block_name,
                           pattern = " |;",
                           replacement = "\n"))

    #### PLOT: RESPONSE TIME ####
    plot_RT <-
      ggplot2::ggplot() +
      # CRITERION
      # @Greenwald2020: RT < 800 ms
      ggplot2::geom_vline(xintercept = 800,
                          col = "#0072B2") + # dark-blue
      # SAMPLE DISTRIBUTION
      ggplot2::geom_boxplot(data = exemplar_samples,
                            ggplot2::aes(x = M_RT,
                                         y = posY,
                                         group = posY)) +
      # STYLING
      ggplot2::theme_minimal() +
      # Change y-axis labels
      ggplot2::scale_y_continuous(breaks = exemplar_stats$posY,
                                  labels = exemplar_stats$trial_name,
                                  # do not show minor gridlines
                                  minor_breaks = FALSE,
                                  # Add informative secondary axis
                                  sec.axis = ggplot2::sec_axis(trans = ~.,
                                                               breaks = exemplar_stats$posY,
                                                               labels = glue::glue("{round(exemplar_stats$Perc_validity_RT, 2)}%"))
      ) +
      # Visualize the (in)validity with colored labels
      ggplot2::theme(# Color code exemplars
        axis.text.y.left =
          ggtext::element_markdown(color = exemplar_stats$Col_validity_RT),
        # Color code percentages
        axis.text.y.right =
          ggtext::element_markdown(color = exemplar_stats$Col_validity_RT),
        plot.title = ggplot2::element_text(size = 10)) +
      # Add axis labels
      ggplot2::labs(x = "Mean Response Time (in ms)",
                    y = ggplot2::element_blank(),
                    title = "RT") +
      # Increase the number of breaks on the x-axis
      ggplot2::scale_x_continuous(n.breaks = 10)

    #### ERROR RATE ####
    plot_ERROR <-
      ggplot2::ggplot() +
      # CRITERION
      # @Greenwald2020: Error < 10 %
      ggplot2::geom_vline(xintercept = 10,
                          col = "#0072B2") + # dark-blue
      # SAMPLE DISTRIBUTION
      ggplot2::geom_boxplot(data = exemplar_samples,
                            ggplot2::aes(x = P_ERROR,
                                         y = posY,
                                         group = posY)) +
      # STYLING
      ggplot2::theme_minimal() +
      # Change y-axis labels
      ggplot2::scale_y_continuous(breaks = exemplar_stats$posY,
                                  labels = exemplar_stats$trial_name,
                                  # do not show minor gridlines
                                  minor_breaks = FALSE,
                                  # Add informative secondary axis
                                  sec.axis = ggplot2::sec_axis(trans = ~.,
                                                               breaks = exemplar_stats$posY,
                                                               labels = glue::glue("{round(exemplar_stats$Perc_validity_ERROR, 2)}%"))
      ) +
      # Visualize the (in)validity with colored labels
      ggplot2::theme(# Color code exemplars
        axis.text.y.left =
          ggtext::element_markdown(color = exemplar_stats$Col_validity_ERROR),
        # Color code percentages
        axis.text.y.right =
          ggtext::element_markdown(color = exemplar_stats$Col_validity_ERROR),
        plot.title = ggplot2::element_text(size = 10)) +
      # Add axis labels
      ggplot2::labs(x = "Percentage Incorrect",
                    y = ggplot2::element_blank(),
                    title = "ERROR") +
      # Increase the number of breaks on the x-axis
      ggplot2::scale_x_continuous(n.breaks = 10)

    #### OVERALL ####
    plot_TOTAL <-
      ggplot2::ggplot(data = exemplar_stats,
                      ggplot2::aes(y = posY)) +
      # RESPONSE TIME
      ggplot2::geom_point(
        ggplot2::aes(x = 1,
                     col = Rel_validity_RT),
        shape = 15,
        size = 5) +
      # ERROR
      ggplot2::geom_point(
        ggplot2::aes(x = 2,
                     col = Rel_validity_ERROR),
        shape = 15,
        size = 5) +
      # ALL
      ggplot2::geom_point(
        ggplot2::aes(x = 3,
                     col = Rel_validity_TOTAL),
        shape = 15,
        size = 5) +
      # STYLING
      ggplot2::theme_minimal()  +
      ggplot2::theme(rect = ggplot2::element_rect(fill = "white")) +
      # Custom color profile
      ggplot2::scale_color_manual(values = c("valid" = "#009E73", # green
                                             "invalid" = "#D55E00", # red
                                             "unreliable" = "#000000"),
                                  breaks = c("valid", "invalid", "unreliable")) + # black
      # axis labels
      ggplot2::scale_y_continuous(breaks = exemplar_stats$posY,
                                  labels = exemplar_stats$trial_name,
                                  minor_breaks = FALSE,
                                  sec.axis = ggplot2::sec_axis(trans = ~.,
                                                               breaks = categories$posY,
                                                               labels = categories$block_name),
                                  expand = ggplot2::expansion(add = 0.5)) +
      ggplot2::scale_x_continuous(breaks = 1:3,
                                  labels = c("RT", "ERROR", "OVERALL"),
                                  expand = ggplot2::expansion(add = 0.5),
                                  minor_breaks = FALSE) +
      ggplot2::theme(legend.position = "top",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     # Color code exemplars
                     axis.text.y.left =
                       ggtext::element_markdown(color = exemplar_stats$Col_validity_TOTAL),
                     plot.title = ggplot2::element_text(size = 10)) +
      ggplot2::labs(x = ggplot2::element_blank(),
                    y = ggplot2::element_blank(),
                    color = ggplot2::element_blank(),
                    title = "TOTAL")

    #### COMBINE PLOTS ####
    # Unify the plots into one diagram

    # Get legend so that it can be plotted separately
    plot_legend <-
      cowplot::get_legend(plot_TOTAL +
                            # Increase the size of the text
                            ggplot2::theme_minimal(base_size = 15) +
                            ggplot2::theme(rect = ggplot2::element_rect(fill = "white")) +
                            # Horizontal legend
                            ggplot2::theme(legend.direction = "horizontal") +
                            # Increase the size of the squares to indicate the colors
                            ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5))))

    # COMBINE
    combined_plot <-
      cowplot::plot_grid(
        # Add the legend to the top of the plot
        plot_legend,
        # Add the rest of the combined plots
        cowplot::plot_grid(
          # Add the two boxplots below one another
          # ... with equal spacing
          cowplot::plot_grid(plot_RT,
                             plot_ERROR,
                             ncol = 1,
                             nrow = 2),
          # Add the overall plot to the right of that
          plot_TOTAL +
            # remove the legend, is plotted above all plots
            ggplot2::theme(legend.position = "none"),
          labels = c("", "C"),
          # boxplots more width than the overview plot
          rel_widths = c(0.70, 0.3),
          # set dimensions
          ncol = 2,
          nrow = 1),
        # Adjust the space the legend can take up
        rel_heights = c(0.1, 2),
        nrow = 2,
        ncol = 1,
        align = "top"
      )

    #### SAVE ####
    # Save with fixed size so that proportions are guaranteed
    cowplot::ggsave2(plot = combined_plot,
                     file = here::here("plots",
                                       "IATs",
                                       glue::glue("{IAT}_{year_of_interest}.png")),
                     device = "png",
                     units = "cm",
                     # default sizes: 22.3 x 16.4 cm
                     height = plotHeight,
                     width = 25,
                     bg = "white")
} # END FUNCTION

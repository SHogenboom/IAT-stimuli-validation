#' GOAL
#' Visualize the distribution of the response parameters in the different samples
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
#' ... and visualize the overall reliability of the (in)validity judgment
#' All plot a final comparison which allows one to see the final verdict on
#' ... the validity of exemplars per category
#' @param exemplar_samples the raw data from the bootstrapped re- samples
#' ... needs to include the mean RT and error percentage for each of the samples
#' @param exemplar_stats the aggregated conclusions. Including the percentage
#' ... of validity per criterion.
#' @param filename the path and name of the output pdf
#' @return saves a pdf in the designated path/filename (filename)

results_plot <- function(exemplar_samples,
                         exemplar_stats,
                         filename) {
    #### PREPARE DATA ####
    exemplar_stats <-
        exemplar_stats %>%
        # Restructure for consistent plotting
        arrange(category, trial_name) %>%
        # Add index for plotting
        mutate(
            # Give each category a unique value
            "category_id" = as.numeric(factor(category)) - 1,
            # Give each exemplar a unique plotting position within that category
            "posY" = 1:n() + category_id) %>%
        # Create color label to show reliably (in)valid exemplars
        mutate()

    # Add plotting positions
    exemplar_samples <-
        left_join(exemplar_samples,
                  exemplar_stats[, c("trial_name", "posY")],
                  by = "trial_name")

    # CATEGORIES
    categories <-
        exemplar_stats %>%
        group_by(category) %>%
        summarize("posY" = mean(posY)) %>%
        ungroup()

    #### RESPONSE TIME ####
    plot_RT <-
        ggplot() +
        # CRITERION
        # @Greenwald2020: RT < 800 ms
        geom_vline(xintercept = 800,
                   col = "#0072B2") + # dark-blue
        # SAMPLE DISTRIBUTION
        geom_boxplot(data = exemplar_samples,
                     aes(x = M_RT,
                         y = posY,
                         group = posY)) +
        # STYLING
        # e.g., white background
        theme_minimal() +
        # Change y-axis labels
        scale_y_continuous(breaks = exemplar_stats$posY,
                           labels = exemplar_stats$trial_name,
                           # do not show minor gridlines
                           minor_breaks = FALSE,
                           # Add informative secondary axis
                           sec.axis = sec_axis(trans = ~.,
                                               breaks = exemplar_stats$posY,
                                               labels = glue::glue("{exemplar_stats$Perc_validity_RT}%"))
        ) +
        # Visualize the (in)validity with colored labels
        theme(# Color code exemplars
            axis.text.y.left =
                ggtext::element_markdown(color = exemplar_stats$Col_validity_RT),
            # Color code percentages
            axis.text.y.right =
                ggtext::element_markdown(color = exemplar_stats$Col_validity_RT)) +
        # Add axis labels
        labs(x = "Mean Response Time (in ms)",
             y = element_blank()) +
        # Increase the number of breaks on the x-axis
        scale_x_continuous(n.breaks = 10)

    #### ERROR RATE ####
    plot_ERROR <-
        ggplot() +
        # CRITERION
        # @Greenwald2020: Error < 10 %
        geom_vline(xintercept = 10,
                   col = "#0072B2") + # dark-blue
        # SAMPLE DISTRIBUTION
        geom_boxplot(data = exemplar_samples,
                     aes(x = P_ERROR,
                         y = posY,
                         group = posY)) +
        # STYLING
        # e.g., white background
        theme_minimal() +
        # Change y-axis labels
        scale_y_continuous(breaks = exemplar_stats$posY,
                           labels = exemplar_stats$trial_name,
                           # do not show minor gridlines
                           minor_breaks = FALSE,
                           # Add informative secondary axis
                           sec.axis = sec_axis(trans = ~.,
                                               breaks = exemplar_stats$posY,
                                               labels = glue::glue("{exemplar_stats$Perc_validity_ERROR}%"))
        ) +
        # Visualize the (in)validity with colored labels
        theme(# Color code exemplars
            axis.text.y.left =
                ggtext::element_markdown(color = exemplar_stats$Col_validity_ERROR),
            # Color code percentages
            axis.text.y.right =
                ggtext::element_markdown(color = exemplar_stats$Col_validity_ERROR)) +
        # Add axis labels
        labs(x = "Percentage Incorrect",
             y = element_blank()) +
        # Increase the number of breaks on the x-axis
        scale_x_continuous(n.breaks = 10)

    #### OVERALL ####
    plot_TOTAL <-
        ggplot(data = exemplar_stats,
               aes(y = posY)) +
        # RESPONSE TIME
        geom_point(aes(x = 1,
                       col = Rel_validity_RT),
                   shape = 15,
                   size = 5) +
        # ERROR
        geom_point(aes(x = 2,
                       col = Rel_validity_ERROR),
                   shape = 15,
                   size = 5) +
        # ALL
        geom_point(aes(x = 3,
                       col = Rel_validity_TOTAL),
                   shape = 15,
                   size = 5) +
        # STYLING
        theme_minimal()  +
        # Custom color profile
        scale_color_manual(values = c("valid" = "#009E73", # green
                                      "invalid" = "#D55E00", # red
                                      "unreliable" = "#000000"),
                           breaks = c("valid", "invalid", "unreliable")) + # black
        # axis labels
        scale_y_continuous(breaks = exemplar_stats$posY,
                           labels = exemplar_stats$trial_name,
                           minor_breaks = FALSE,
                           sec.axis = sec_axis(trans = ~.,
                                               breaks = categories$posY,
                                               labels = categories$category),
                           expand = expansion(add = 0.5)) +
        scale_x_continuous(breaks = 1:3,
                           labels = c("RT", "ERROR", "OVERALL"),
                           expand = expansion(add = 0.5),
                           minor_breaks = FALSE) +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 45, hjust = 1),
              # Color code exemplars
              axis.text.y.left =
                  ggtext::element_markdown(color = exemplar_stats$Col_validity_TOTAL),) +
        labs(x = element_blank(),
             y = element_blank(),
             color = element_blank())

    #### COMBINE PLOTS ####
    # Unify the plots into one diagram

    # Get legend so that it can be plotted separately
    plot_legend <-
        cowplot::get_legend(plot_TOTAL +
                                # Increase the size of the text
                                theme_minimal(base_size = 20) +
                                # Horizontal legend
                                theme(legend.direction = "horizontal") +
                                # Increase the size of the squares to indicate the colors
                                guides(color = guide_legend(override.aes = list(size = 5))))

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
                                   labels = c("A", "B"),
                                   ncol = 1,
                                   nrow = 2),
                # Add the overall plot to the right of that
                plot_TOTAL +
                    # remove the legend, is plotted above all plots
                    theme(legend.position = "none"),
                labels = c("", "C"),
                # boxplots more width than the overview plot
                rel_widths = c(0.75, 0.25),
                # set dimensions
                ncol = 2,
                nrow = 1),
            # Adjust the space the legend can take up
            rel_heights = c(0.05, 0.95),
            nrow = 2,
            ncol = 1
        )

    #### SAVE ####
    # Save with fixed size so that proportions are guaranteed
    cowplot::ggsave2(plot = combined_plot,
                     file = filename,
                     device = "png",
                     units = "cm",
                     # default sizes: 22.3 x 16.4 cm
                     height = 20,
                     width = 22.3)

} # END FUNCTION

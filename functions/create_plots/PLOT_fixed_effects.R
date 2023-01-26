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

#'
#' GOAL
#' Visualize the fixed effects of the mixed-effects model which inspects
#' how stimulus types compare with oneanother
#'
#' @param mod the model from the mixed-effects function
#' @param DV for the title of the plot
#' @return a ggplot2 object

plot_fixed_effects <- function(mod,
                               validity_estimates,
                               DV) {

  #### FIXED EFFECTS ####
  # Extract coefficients from the model
  fixed_estimates <-
    as.data.frame(coefficients(summary(mod))) |>
    # Order for prettier plotting
    dplyr::arrange(desc(Estimate)) |>
    # Add plotting position
    dplyr::mutate("posY" = (dplyr::n():1) + 2) |>
    # Compute significance levels
    dplyr::mutate("sign_asterisk" =
                    dplyr::case_when(`Pr(>|t|)` < .001 ~ "***",
                                     `Pr(>|t|)` < .01 ~ "**",
                                     `Pr(>|t|)` < .05 ~ "*",
                                     `Pr(>|t|)` < .1 ~ ".",
                                     # All other cases
                                     TRUE ~ ""))

  # Convert estimates to the percentage_valid (the dependent variable)
  fixed_estimates$percentage_valid <-
    c(fixed_estimates$Estimate[1], # intercept
      # Estimate + Intercept
      fixed_estimates$Estimate[2:nrow(fixed_estimates)] +
        fixed_estimates$Estimate[1])

  # Create easy-to-read labels
  fixed_estimates$axis_labels <-
    c("Image", # Intercept
      gsub(rownames(fixed_estimates)[2:nrow(fixed_estimates)],
           pattern = "stimulus_type",
           replacement = ""))
  fixed_estimates$plot_labels <-
    glue::glue("{round(fixed_estimates$percentage_valid, 2)} {fixed_estimates$sign_asterisk}")

  #### RAW DATA ####
  validity_estimates <-
    validity_estimates |>
    dplyr::group_by(stimulus_type) |>
    dplyr::mutate("sec_axis_label" = glue::glue("N = {dplyr::n()}")) |>
    dplyr::left_join(fixed_estimates |>
                       dplyr::select(axis_labels, posY),
                     by = c("stimulus_type" = "axis_labels"))

  # Add descriptives of N to fixed effects
  fixed_estimates <-
    dplyr::left_join(fixed_estimates,
                     validity_estimates |>
                       dplyr::select(stimulus_type, sec_axis_label) |>
                       unique(),
                     by = c("axis_labels" = "stimulus_type"))

  #### CREATE PLOT ####
  ggplot2::ggplot(data = fixed_estimates,
                  ggplot2::aes(x = percentage_valid,
                               y = posY)) +
    # ADD ESTIMATES
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = plot_labels),
                       nudge_y = 0.3,
                       size = 3) +
    # ADD STANDARD ERRORS
    ggplot2::geom_errorbar(# Estimate - SE // Estimate + SE
      ggplot2::aes(xmin = percentage_valid - `Std. Error`,
                   xmax = percentage_valid + `Std. Error`),
      width = .2) +
    # ADD RAW DATA
    ggplot2::geom_jitter(
      data = validity_estimates,
      # Adjust relative position (with 1 being center)
      ggplot2::aes(x = get(glue::glue("Perc_validity_{DV}")),
        y = posY - 0.4,
                   # Give the reliability color
                   col = get(glue::glue("Rel_validity_{DV}"))),
      height = 0.2,
      size = 0.3
    ) +
    # STYLING
    ggplot2::theme_minimal() +
    # Relabel & Rescale x-axis
    ggplot2::scale_x_continuous(
      # Add a axis tick for each 10%
      breaks = seq(from = 0,
                   to = 100,
                   by = 10),
      # Do not plot 5% breaks
      minor_breaks = FALSE,
      # Force consistent axis - even if only 90-100% is used
      # Set to a minus values to allow errorbars to go below 0
      limits = c(-10, 100),
      # Prevent out-of-bounds jitters
      oob = scales::squish,
      # Add percentage sign to axis labels
      labels = function(x) paste0(x, "%"),
      # Reduce spacing around the axes
      expand = c(0, 2)
    ) +
    ggplot2::scale_y_continuous(
      breaks = fixed_estimates$posY,
      labels = fixed_estimates$axis_labels,
      sec.axis = ggplot2::sec_axis(~ .,
                                   breaks = fixed_estimates$posY,
                                   labels = fixed_estimates$sec_axis_label)
      ) +
    ggplot2::labs(x = "Percentage Valid",
                  y = ggplot2::element_blank(),
                  title = DV,
                  col = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(size = 10)
    ) +
    # Custom color profile
    ggplot2::scale_color_manual(values = c("valid" = "#009E73", # green
                                           "invalid" = "#D55E00", # red
                                           "unreliable" = "#000000"), # black
                                breaks = c("valid", "invalid", "unreliable")) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 5)))
}


fixed_effect_RT_ERROR <- function(subset_validity_estimates,
                                  mixed_effects_RT,
                                  mixed_effects_ERROR) {
  # VISUALIZE
  fixed_effects_RT <-
    plot_fixed_effects(mod = mixed_effects_RT,
                       validity_estimates = subset_validity_estimates,
                       DV = "RT")
  fixed_effects_ERROR <-
    plot_fixed_effects(mod = mixed_effects_ERROR,
                       validity_estimates = subset_validity_estimates,
                       DV = "ERROR")

  # SHARED LEGEND
  # Extract common legend
  shared_legend <-
    cowplot::get_legend(fixed_effects_RT)

  # COMBINE PLOTS
  combined_plot <-
  cowplot::plot_grid(
    shared_legend,
    fixed_effects_RT +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = ggplot2::element_blank()),
    fixed_effects_ERROR + ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = ggplot2::element_blank()),
    # Three rows
    nrow = 3,
    # Assign heights of the plots, legend should take up less space
    rel_heights = c(0.2, 1, 1),
    # Align the plots left
    axis = "l")

  # Save plot for later use.
  cowplot::ggsave2(combined_plot,
                   filename = here::here("plots",
                                         "fixed_effects_RT_ERROR.png"),
                   height = 10,
                   units = "cm",
                   bg = "white")
}

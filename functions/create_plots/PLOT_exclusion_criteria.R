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
#' Visualize the impact of the exclusion criteria in a two-part figure.
#' Part 1: Compressed Data (i.e., participants)
#' Part 2: Raw Data (i.e., trials)
#'
#' @param IAT_name name of the IAT's folder in `datasets/OSF`
#' @param year_of_interest numeric
#'
#' @return a plot
combined_exclusion_plots <- function(IAT_name,
                                     year_of_interest) {
  # Initialize
  plotlist <- list()

  for (datType in c("Compressed", "Raw")) {
    # Load exclusion statistics
    if (IAT_name != "FULL_SAMPLE") {
      # Load the data from a single IAT (for Pilot & Appendix)
      exclusion_stats <-
        data.table::fread(
          here::here(
            "datasets",
            "OSF",
            IAT_name,
            glue::glue("{datType}_{year_of_interest}_exclusion_stats.csv")
          )
        )
    } else {
      # Combine the exclusion statistics for all included_IATs
      exclusion_stats <-
        get_full_sample_stats(
          year_of_interest,
          type = datType
        )
    } # END IF full_sample

    # CREATE PLOT
    # See below for code
    plotlist[[datType]] <-
      exclusion_plot(
        exclusion_stats,
        datType
      )
  } # END datType LOOP

  #### COMBINE PLOTS ####
  # Extract the legend, so it can be plotted only once
  plot_legend <- cowplot::get_legend(plotlist[["Compressed"]])

  # Combine the two plots
  combined_exclusion_plot <-
    cowplot::plot_grid(plot_legend, # first plot the legend
      # plots without legends
      plotlist[["Compressed"]] + ggplot2::theme(legend.position = "none"),
      plotlist[["Raw"]] + ggplot2::theme(legend.position = "none"),
      # Three rows (1 legend + 2 plots)
      nrow = 3,
      # Assign heights of the plots, legend should take up less space
      rel_heights = c(0.2, 1, 1),
      # Align the plots left
      axis = "l"
    )

  #### SAVE AS png ####
  cowplot::ggsave2(combined_exclusion_plot,
    filename = here::here(
      "plots",
      "exclusion_criteria",
      glue::glue("{IAT_name}_{year_of_interest}.png")
    ),
    units = "cm",
    height = 10,
    bg = "white"
  )
}

#' GOAL
#' Visualize the impact of the exclusion criteria
#' @param exclusion_stats an exclusion_stats df from the raw or compressed exclusion
#' ... functions
#' @param datType "Compressed" or "Raw" - determines the x-axis label
#' @return a ggplot object that can be included in a manuscript

exclusion_plot <- function(exclusion_stats,
                           datType) {
  # Number of participants BEFORE exclusion
  n_prior <-
    sum(exclusion_stats$N[exclusion_stats$Criteria == "Total"])

  # SECONDARY AXIS LABELS
  # The percentage of excluded participants/trials
  plotlables <-
    exclusion_stats |>
    # Compute the percentage of EXCLUDED participants
    dplyr::filter(Type != "Included") |>
    # Determine per criteria
    dplyr::group_by(Criteria) |>
    # Create labels
    dplyr::summarize(
      "perc_excluded" = sum(N) / n_prior * 100,
      "label" = glue::glue("{round(perc_excluded, 2)} %"),
      # For plotting purposes
      "index" = as.numeric(unique(index))
    ) |>
    dplyr::ungroup()

  # PREPARE DATA
  # Convert to factor for plotting the bars in the desired and consistent order
  exclusion_stats$Type <-
    factor(exclusion_stats$Type,
      levels = c("Included", "Excluded", "Missing")
    )

  # CREATE PLOT
  exclusion_plot <-
    ggplot2::ggplot(
      data = exclusion_stats,
      ggplot2::aes(
        x = N,
        y = index,
        fill = Type
      )
    ) +
    # VISUALIZE CRITERIA
    # Visualize the effect of each criteria
    # Bars are stacked (ie. put next to each-other)
    ggplot2::geom_bar(
      stat = "identity",
      position = "stack",
      alpha = 0.4,
      orientation = "y"
    ) +
    # STYLING
    ggplot2::theme_minimal() +
    # Set the colors of the bars
    ggplot2::scale_fill_manual(values = c(
      "Included" = "#009E73", # green
      "Excluded" = "#D55E00", # red
      "Missing" = "#56B4E9"
    )) + # light blue
    # Format axes (big numbers separated with comma e.g., 10,000)
    ggplot2::scale_x_continuous(
      labels = scales::comma,
      expand = c(0, 0),
      # repeat axis at the top
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        labels = scales::comma
      )
    ) +
    # Add name labels of the criteria
    # Add secondary labels that include the % excluded participants/trials
    ggplot2::scale_y_continuous(
      breaks = unique(exclusion_stats$index),
      labels = unique(exclusion_stats$Criteria),
      expand = c(0, 0),
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        breaks = plotlables$index,
        labels = plotlables$label,
        name = "Percentage Excluded \n"
      )
    ) +
    ggplot2::labs(
      x = ifelse(datType == "Compressed",
        "N Participants",
        "N Trials"
      ),
      y = "Exclusion Criterion",
      title = stringr::str_to_title(datType),
      fill = ggplot2::element_blank()
    ) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.major.y = ggplot2::element_blank(), # no grid lines for the y-axis
      panel.ontop = TRUE, # show gridlines on top of bars for readibility
      plot.title = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 8),
      plot.title.position = "plot"
    )

  return(exclusion_plot)
} # END exclusion plot FUNCTION

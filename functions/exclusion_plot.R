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
#' Visualize the impact of the exclusion criteria
#' @param exclusion_data an exclusion_data df from the raw or compressed exclusion
#' ... functions
#' @param datType "compressed" or "raw" - determines the x-axis label
#' @return a ggplot object that can be included in a manuscript

plot_exclusion_criteria <- function(exclusion_data,
                                    datType) {

    # Number of participants BEFORE exclusion
    n_prior <-
        sum(exclusion_data$N[exclusion_data$Criteria == "Total"])

    # SECONDARY AXIS LABELS
    # The percentage of excluded participants/trials
    plotlables <-
        exclusion_data %>%
        # Compute the percentage of EXCLUDED participants
        filter(Type != "Included") %>%
        # Determine per criteria
        group_by(Criteria) %>%
        # Create labels
        summarize("perc_excluded" = sum(N) / n_prior * 100,
                  "label" = glue::glue("{round(perc_excluded, 2)} %"),
                  # For plotting purposes
                  "index" = as.numeric(unique(index))) %>%
        ungroup()

    # PREPARE DATA
    # Convert to factor for plotting the bars in the desired and consistent order
    exclusion_data$Type <-
        factor(exclusion_data$Type,
               levels = c("Included",  "Excluded", "Missing"))

    # CREATE PLOT
    exclusion_plot <-
        ggplot(data = exclusion_data,
               aes(x = N,
                   y = index,
                   fill = Type)) +
        # VISUALIZE CRITERIA
        # Visualize the effect of each criteria
        # Bars are stacked (ie. put next to each-other)
        geom_bar(stat = "identity",
                 position = "stack",
                 alpha = 0.4,
                 orientation = "y") +
        # STYLING
        theme_minimal() +
        # Set the colors of the bars
        scale_fill_manual(values = c("Included" = "#009E73", # green
                                     "Excluded" = "#D55E00", # red
                                     "Missing" = "#56B4E9")) + # light blue
        # Format axes (big numbers separated with comma e.g., 10,000)
        scale_x_continuous(labels = scales::comma,
                           expand = c(0, 0),
                          # repeat axis at the top
                           sec.axis = sec_axis(trans = ~.,
                                               labels = scales::comma)) +
        # Add name labels of the criteria
        # Add secondary labels that include the % excluded participants/trials
        scale_y_continuous(breaks = unique(exclusion_data$index),
                           labels = unique(exclusion_data$Criteria),
                           expand = c(0, 0),
                           sec.axis = sec_axis(trans = ~.,
                                               breaks = plotlables$index,
                                               labels = plotlables$label,
                                               name = "Percentage Excluded \n")) +
        labs(x = ifelse(datType == "compressed",
                        "N Participants",
                        "N Trials"),
             y = "Exclusion Criterion",
             fill = element_blank()) +
        theme(legend.position = "top",
              panel.grid.major.y = element_blank(), # no grid lines for the y-axis
              panel.ontop = TRUE) # show gridlines on top of bars for readibility

    return(exclusion_plot)
} # END exclusion plot FUNCTION

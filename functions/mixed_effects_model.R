#' FIT MIXED EFFECTS MODEL
#'
#' @param DV the name of the dependent variable (RT/ERROR/TOTAL)
#' @param sub_validity_estimates the bootstrapped results. If a filter is necessary -
#' ... apply before passing to function.
#' @param results type of results to display (in-text vs. appendix)
mixed_effects_model <-
  function(DV,
           sub_validity_estimates) {

    #### FIT MODEL ####
    # Mixed-Effects Model
    # Dependent Variable: continuous; percentage_validity
    # Fixed-Effect: stimulus_type
    # Random-Effect: IAT
    mod <-
      lmerTest::lmer(
        data = sub_validity_estimates,
        get(glue::glue("Perc_validity_{DV}")) ~ 1 + stimulus_type + (1 | IAT),
        na.action = "na.omit",
        control =
          lme4::lmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 200000)))

    return(mod)
}

#' GOAL
#'
#' Quickly run the planned mixed-effects model without any plotting details.
#' NOTE: this is not the actual analysis we utilized during stage 2 - for that
#' see the function `mixed_effects_model` above.
#'
#' @param DV the name of the dependent variable (Perc_Valid_RT/ERROR/TOTAL)
#' @param sub_validity_estimates the bootstrapped results. If a filter is necessary -
#' ... apply before passing to function.
#'

planned_mixed_effects_model <- function(DV,
                                        sub_validity_estimates) {

  lmerTest::lmer(
    data = sub_validity_estimates,
    # MODEL
    get(DV) ~ 1 + stimulus_type + (1 + stimulus_type | IAT),
    # PARAMETERS
    na.action = "na.omit",
    control =
      lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 200000)
      )
  )

}

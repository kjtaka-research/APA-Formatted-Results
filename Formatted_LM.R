#' Make Table of Formatted lm and lmer Results
#'
#' Takes any number of fitted lm, glm, lmer, or glmer models  and creates a
#'   table of formatted results for fixed effects. Formatting is for in-text
#'   reporting based on the 7th edition of the APA Style Guide.
#'   This function can also be used to compile stats across multiple models
#'   without formatting. Additional options are available for confidence
#'   intervals, Cohen's d for models using categorical variables, and odds
#'   ratios for logistic regression models. This is an early version of a 
#'   function from a package that is still in development, so some code has
#'   has been added to make it work as a standalone function.
#'
#' @param models Takes a single lm or lmer model or multiple models in a list.
#'   It is recommended to use a named list with clear names for each model, but
#'   the individual models will be identified by the DV and data if an unnamed
#'   list is used.
#' @param formatted If formatted = TRUE, takes each relevant statistic and
#'   formats them into a single string formatted for in-text reporting in
#'   line with the 7th edition APA style guide. If formatted = FALSE, results
#'   are presented with separate columns for each unformatted statistic.
#' @param sig_stars Adds stars to denote significance levels.
#'   * p < .05
#'   ** p < .01
#'   *** p < .001
#' @param model_names Alternate method of specifying model names. Instead of
#'   using a named list, you can use a vector of names with the same length as
#'   the number of models tested.
#' @param side_by_side Creates a table with each model as a column, each
#'   coefficient as a row, and the coefficient and significance stars. If
#'   a variable has a different name across models, the rename_coef
#'   argument will be needed to have those rows merged.
#' @param rename_coef If TRUE, will rename predictors based on the
#'   old_coef_names and new_coef_names arguments
#' @param old_coef_names Description here.
#' @param new_coef_names Description here.
#' @param ci Use confint function to get confidence intervals for betas
#' @param ci_method Change default method for the confint function. The default
#'   is set to Wald confidence intervals. Other options are "profile" and "bootstrap".
#'   For lmer models, the default of "profile" and the "bootstrap" options are
#'   often recommended but take longer to run. "Wald" confidence intervals
#'   take less time to run.
#' @param cohens_d Option to calculate Cohen's d and its confidence intervals.
#'   This requires that a pooled SD value is specified with the sd_pooled
#'   argument. The function will rescale coefficients based on the pooled SD
#'   provided while adjusting for the contrast weights for predictors as
#'   specified with the d_contrast argument.
#'   Use with caution, as the output will only be interpretable if the contrasts
#'   reflect a comparison for which Cohen's d would make sense
#'   (e.g., pairwise comparison). The code will still run if predictors in the
#'   model are continuous.
#' @param sd_pooled A single value for the pooled SD to be used for Cohen's d
#'   calculations.
#' @param d_contrast The maximum and minimum values of the contrast codes for
#'   categorical variable in the model. The argument will work for multiple for
#'   different contrasts as long as the difference between the maximum and
#'   minimum contrast values are the same. For example, a model can have
#'   two-level factor represented by a 0.5, -0.5 contrast and a three-level
#'   factor represented by two pairwise contrasts with values of .667 and -.333.
#'   Specifying either c(.5, -.5) or c(.667, -.333) will produce the same
#'   results because the difference between the max and min values is the same,
#'   1. If one factor uses effect coding (1, -1) and another uses simple
#'   coding (0.5, -0.5), the final estimates will for the one that matches the
#'   minimum and maximum contrast values set here. The default is c(0.5, -0.5).
#' @param d_which If there are only some variables for which you want to
#'   estimate Cohen's d for, enter those variable names.
#' @param OR If true, estimates odds ratios from logistic regression models
#' @param OR_models If odds ratios should only be included for some models,
#'   include the names of the models here.
#' @export
make_lm_table <- function(models, formatted = T, sig_stars = F,
                          model_names = NULL,
                          side_by_side = F, rename_coef = F,
                          old_coef_names = NULL, new_coef_names = NULL,
                          ci = F, ci_method = "Wald", ci_level = .95,
                          cohens_d = F, sd.pooled = NULL,
                          d_contrast = c(-0.5, 0.5), d_which = NULL,
                          OR = F, OR_models = NULL
                          ) {
  # Required packages
  require(lme4)
  require(tidyverse)
  # Internal functions
  formatted_round = function(x, decimal_places = 2, remove_lead0 = F, p_value = F) {
    if (p_value == TRUE) {
      rounded.x = ifelse(x < .001, "< .001", sub("^0+", "", format(round(x, 3), nsmall = 3)))
    } else {
      rounded.x = trimws(format(round(x, decimal_places), nsmall = decimal_places), which = "both")
      if (remove_lead0 == T) {
        rounded.x = sub("^(-?)0.", "\\1.", rounded.x)
      }
    }
  rounded.x
  }
  p_stars <- function(p_values) {
    dplyr::case_when(
      p_values < .001 ~ "***",
      p_values > .001 & p_values < .01 ~ "**",
      p_values > .01 & p_values < .05 ~ "*",
      p_values > .05 & p_values < .1 ~ "`",
      .default = "")
  }
  if (cohens_d == TRUE | OR == TRUE) {ci = TRUE} # override for d CIs
  if (class(models)[1] != "list") { # Making code work with single model as input
    models = list(models)
  }
  if (models |> names() |> is.null() == F) {
    model_names = names(models)
  }
  output = list()
  df.lm = list()
  for (i in c(1:length(models))) {
    if (class(models[[i]])[1] == "lmerMod") {
          models[[i]] = lmerTest::as_lmerModLmerTest(models[[i]])
        }
    output[[i]] =  summary(models[[i]])$coefficients |> as.data.frame()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2] |>
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Term = row.names(summary(models[[i]])$coefficients)
    output[[i]]$Data = summary(models[[i]])$call["data"] |> as.character() |>
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Class = rep(class(models[[i]])[1], times = nrow(output[[i]]))
    output[[i]]$df = ifelse(output[[i]]$Class == "lmerModLmerTest",
                            output[[i]]$df,
                            ifelse(output[[i]]$Class == "lm",
                              rep(summary(models[[i]])$df[2],
                                  times = nrow(output[[i]])), NA))
    output[[i]]$TestType = ifelse(
      sum(stringr::str_detect(names(output[[i]]), "t value")) == 1, "t",
      ifelse(
        sum(stringr::str_detect(names(output[[i]]), "z value")) == 1, "z", NA
        )
      ) |>
      rep(times = nrow(output[[i]]))
    names(output[[i]])[stringr::str_detect(names(output[[i]]), "t value|z value") == T] = "TestValue"
    names(output[[i]])[stringr::str_detect(names(output[[i]]), "Pr") == T] = "p.value"
    output[[i]] = dplyr::select(output[[i]], Class, Data, DV, Term,
                                Estimate, `Std. Error`,
                                TestType, df, TestValue, p.value)

    if(ci == TRUE | cohens_d == TRUE | OR == TRUE) {
      model_ci = confint(models[[i]], method = ci_method) |> as.data.frame()
      names(model_ci) = c("CI.lower", "CI.upper")
      model_ci = dplyr::mutate(model_ci, Term = rownames(model_ci))
      output[[i]] = dplyr::left_join(output[[i]], model_ci, by = "Term")
    }
    if (cohens_d == TRUE) {
      ba = max(d_contrast) - min(d_contrast)
      output[[i]]$d.est = (output[[i]]$Estimate * ba) / sd.pooled
      output[[i]]$d.CI.lower = (output[[i]]$CI.lower * ba) / sd.pooled
      output[[i]]$d.CI.upper = (output[[i]]$CI.upper * ba) / sd.pooled
    }
    if (OR == TRUE) {
      output[[i]]$OR = exp(output[[i]]$Estimate)
      output[[i]]$OR.CI.lower = exp(output[[i]]$CI.lower)
      output[[i]]$OR.CI.upper = exp(output[[i]]$CI.upper)
    }
    if (is.null(model_names) == FALSE) {
      output[[i]]$Model = rep(model_names[i], times = nrow(summary(models[[i]])$coefficients))
    }
  }
  table = do.call(rbind.data.frame, output)
  #table$df = ifelse(table$Class == "lm", as.vector(unlist(df.lm)),
  #                  ifelse(table$Class == "lmerModLmerTest" | table$Class == "lmerMod", table$df))
  row.names(table) = NULL
  table$Significance = p_stars(table$p.value)
  if (is.null(model_names) == FALSE) {
    table = dplyr::select(table, Model, Term, Estimate, everything())
  }
  if (side_by_side == T) {
    formatted = F # override default
    table$Result = paste0(formatted_round(table$Estimate), " ", table$Significance)
    if (rename_coef == T) {
      for (coef_no in c(1:length(old_coef_names))) {
        table$Term = stringr::str_replace_all(table$Term, old_coef_names[coef_no], new_coef_names[coef_no])
      }
    }
    table = dplyr::select(table, Model, Term, Result) |>
      tidyr::pivot_wider(id_cols = Term, names_from = Model, values_from = Result)
  }
  if (formatted == TRUE) {
    # Key code
    table$Result = with(
      table,
      paste0("B = ", formatted_round(Estimate), ", SE = ",
             formatted_round(`Std. Error`), ", ",
             dplyr::case_when(
               TestType == "z" ~ "z",
               TestType == "t" ~ paste0("t(", round(df, digits = 1), ")")
               ),
             " = ", formatted_round(TestValue),
             ifelse(p.value < .001, ", p ", ", p = "),
             formatted_round(p.value, p_value = T)
             )
      )
    if (ci == TRUE | cohens_d == TRUE | OR == TRUE) {
      formatted_ci = paste0(
        ", ", round(ci_level * 100, 2), "% CI [",
        formatted_round(table$CI.lower), ", ",
        formatted_round(table$CI.upper), "]")
    }
    if (ci == TRUE & cohens_d == FALSE & OR == FALSE){
      table$Result = paste0(
        table$Result, formatted_ci)
    }
    if (cohens_d == TRUE) {
      formatted_ds = paste0(
        ", d = ", formatted_round(table$d.est),
        ", ", round(ci_level * 100, 2), "% CI [",
        formatted_round(table$d.CI.lower), ", ",
        formatted_round(table$d.CI.upper), "]"
      )
      if (is.null(d_which) == FALSE) {
        included_terms = paste(d_which, sep = "|")
        table$Result = dplyr::case_when(
          stringr::str_detect(table$Result, included_terms) == T ~
            paste0(table$Result, formatted_ds),
          .default = table$Result)
      } else {
        table$Result = paste0(table$Result, formatted_ds)
      }
    }
    if (OR == TRUE) {
      formatted_OR = paste0(
        ", OR = ", formatted_round(table$OR),
        ", ", round(ci_level * 100, 2), "% CI [",
        formatted_round(table$OR.CI.lower), ", ",
        formatted_round(table$OR.CI.upper), "]"
      )
      if (is.null(OR_models) == FALSE) {
        included_models = paste(paste0("\\b", OR_models, "\\b"), collapse = "|")
        table$Result = dplyr::case_when(
          stringr::str_detect(table$Model, included_models) == T ~
            paste0(table$Result, formatted_OR),
          .default = table$Result)
      } else {
        table$Result = paste0(table$Result, formatted_OR)
      }
    }
    if (is.null(model_names) == FALSE) {
      table = table |> dplyr::select(Model, Term, Result, Significance)
    } else {
      table = table |> dplyr::select(Data, DV, Term, Result, Significance)
    }
    if (sig_stars == F) {
      table = dplyr::select(table, -Significance)
    }
  }
  table
}

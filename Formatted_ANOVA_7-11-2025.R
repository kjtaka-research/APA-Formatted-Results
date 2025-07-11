#' Make Table of Formatted ANOVA Results
#'
#' Takes any number of fitted lm or lmer, tests them as ANOVAs, and creates
#'   a table of any main effects and interactions formatted for in-text
#'   reporting in APA format. This can also be used to compile unformatted
#'   stats for all terms in multiple models. Includes partial eta squared and
#'   its confidence interval. This function uses `car::Anova()` with type 3 SS
#'   as the default.
#'
#' @param models Takes a single lm or lmer model or multiple models in a list.
#'   It is recommended to use a named list with clear names for each model, but
#'   the individual models will be identified by the DV and data if an unnamed
#'   list is used.
#' @param type Specifies SS type for the ANOVAs. Defaults to type 3, but
#'   the appropriate orthogonal contrasts, such as contr.sum, contr.poly,
#'   or a custom contrast, must be set when the lm or lmer models are fitted
#'   for the results to correctly get type 3 SS
#' @param formatted If formatted = TRUE, takes each relevant statistic and
#'   formats them into a single string formatted for in-text reporting in
#'   line with the 7th edition APA style guide. If formatted = FALSE, results
#'   are presented with separate columns for each statistic and no rounding
#' @param partial Specifies whether to use partial eta squared rather than
#'   eta squared. Defaults to partial
#' @param eta_ci Changes confidence interval. Default is 90% CI because
#'    the test is one-sided
#' @param eta_digits Option for the number of decimal places to report for
#'   eta squared, partial eta squared, and their CIs
#' @param model_names Alternate method of specifying model names. Instead of
#'   using a named list, you can use a vector of names with the same length as
#'   the number of models tested.
#'
#' @returns A data frame with the results. If formatted is chosen, this will be
#'   the model names or the data and DV, the term or variable, and a single
#'   column with all results for each term  in a single string.
#'
#' @examples
#' ## Setting example models with built-in dataset
#' ### Fitting models with contrasts
#' test.anova1 <- lm(len ~ supp + as.factor(dose), data = ToothGrowth,
#'     contrasts = list(supp = contr.sum, `as.factor(dose)` = contr.sum))
#' test.anova2 <- lm(len ~ supp * as.factor(dose), data = ToothGrowth,
#'     contrasts = list(supp = contr.sum, `as.factor(dose)` = contr.sum))
#' ## Getting results
#' make_anova_table(list("Main effect model" = test.anova1,
#'     "Factorial model" = test.anova2))
#' @export
make_anova_table <- function(models, type = "III", formatted = TRUE,
                             partial = TRUE, eta_ci = .9, eta_digits = 2,
                             model_names = NULL) {
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
  output = list()
  # Making it so that the models argument can accommodate a single model and not just a list
  if (class(models) != "list") {
    models = list(models)
  }
  # Putting in another method of specifying model names using a named list
  if (models |> names() |> is.null() == F) {
    model_names = names(models)
  }
  for (i in c(1:length(models))) {
    output[[i]] =  car::Anova(models[[i]], type = type, test = "F") |>
      as.data.frame()
    output[[i]]$Term = car::Anova(models[[i]], type = type, test = "F") |>
      as.data.frame() |> row.names()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2] |>
      rep(times = nrow(output[[i]]))
    output[[i]]$Data = summary(models[[i]])$call["data"] |>
      as.character() |> rep(times = nrow(output[[i]]))
    # Getting the df and columns consistent for regular and mixed ANOVA
    if (class(models[[i]]) == "lm") {
      output[[i]]$df = paste0(output[[i]]$Df, ", ", tail(output[[i]]$Df, n = 1))
      output[[i]] = dplyr::select(output[[i]], -Df)
    }
    if (stringr::str_detect(class(models[[i]]), "lmer") == TRUE) {
      output[[i]]$df = paste0(
        output[[i]]$Df, ", ",
        ifelse(
          output[[i]]$Df.res <10, round(output[[i]]$Df.res, 2),
          ifelse(output[[i]]$Df.res >= 10, round(output[[i]]$Df.res, 1), NA)
          )
        )
      output[[i]] = dplyr::select(output[[i]], -Df, -Df.res)
    }
    names(output[[i]]) = sub("F value", "F", names(output[[i]]))
    output[[i]] = dplyr::select(output[[i]], Term, DV, Data, `F`, df, `Pr(>F)`)
    # Effect sizes
    output[[i]] = dplyr::left_join(
      output[[i]],
      car::Anova(models[[i]], type = type, test = "F") |>
        effectsize::eta_squared(ci = eta_ci, partial = partial) |>
        as.data.frame(), by = c("Term" = "Parameter")
      )
    if (is.null(model_names) == FALSE) {
      output[[i]]$Model = rep(model_names[i], times = nrow(output[[i]]))
    }
  }
  table = do.call(rbind.data.frame, output)
  # Renaming variables so that both regular and mixed ANOVA output work
  if (is.null(model_names) == FALSE) {
    table = dplyr::select(table, Model, Data, DV, Term, `F`, df, everything())
  } else {
    table = dplyr::select(table, Data, DV, Term, `F`, df, everything())
  }
  if (formatted == TRUE) {
    names(table) = stringr::str_replace(names(table), "Eta2_partial", "Eta2")
    table$Result = with(
      table,
      paste0("F(", df, ") = ", formatted_round(`F`),
             ifelse(`Pr(>F)` < .001, ", p < .001",
                    paste0(", p = ", formatted_round(`Pr(>F)`, p_value = T))),
             ifelse(partial == T, ", \U03B7\U209A\U00B2 = ",
                    ifelse( partial == F, ", \U03B7\U00B2 = ", NA)),
             formatted_round(Eta2, decimal_places = eta_digits,
                             remove_lead0 = TRUE),
             ", ", round(eta_ci * 100, 2), "% CI [",
             formatted_round(CI_low, decimal_places = eta_digits,
                             remove_lead0 = TRUE), ", ",
             formatted_round(CI_high, decimal_places = eta_digits,
                             remove_lead0 = TRUE), "]"))
    table$Result = ifelse(table$Term == "Residuals", NA, table$Result)
    if (is.null(model_names) == FALSE) {
      table = table |> dplyr::select(Model, Term, Result) |>
        dplyr::filter(Term != "Residuals")
    } else {
      table = table |> dplyr::select(Data, DV, Term, Result) |>
        dplyr::filter(Term != "Residuals")
    }
  }
  row.names(table) = NULL
  table |> dplyr::filter(Term != "(Intercept)")
}

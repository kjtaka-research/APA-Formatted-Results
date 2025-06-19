#### File information
# File name: formatted_anova_6-19-25.R
# Author: Koji J. Takahashi
# Updated: 6-17-2025

### General Comments
# These functions take any number of lm and lmer models and report the ANOVAs and effect sizes 
# Formatted test results are for in-text reporting in APA style
# Can use formatted = FALSE to get the raw values
# Can enter a single model or multiple models in a list. 
# Can enter multiple as a named list and output will be labeled accordingly
# If a list is used without names, models will be identifiable by the DV and data frame 
# Regular and mixed ANOVAs can be run together, but noting the type in a named list is recommended
# e.g., list("Between-subjects ANOVA 1" = model1, "Mixed ANOVA" = model2)
# Default effect size is partial eta squared with an option for eta squared. 
# CI is 90% (for one-sided test)

make.anova.table <- function(models, type = "III", formatted = TRUE, 
                             eta.digits = 3, #  Change number of decimal places for eta^2 and CI
                             partial = TRUE,
                             model.names = NULL) {
  require(car)
  require(dplyr)
  require(stringr)
  # Custom function for rounding
  formatted.round = function(x, digits = 2, remove.lead0 = FALSE) {
    rounded.x = trimws(format(round(x, digits), nsmall = digits), which = "left")
    if (remove.lead0 == TRUE) {
      rounded.x = stringr::str_remove(rounded.x, "^0+")
    }
    rounded.x
  }
  output = list()
  # Making it so that the models argument can accommodate a single model and not just a list
  if (class(models) != "list") {
    models = list(models)
  }
  # Putting in another method of specifying model names using a named list
  if (models |> names() |> is.null() == F) {
    model.names = names(models)
  }
  for (i in c(1:length(models))) {
    output[[i]] =  car::Anova(models[[i]], type = type, test = "F") |> as.data.frame()
    output[[i]]$Term = car::Anova(models[[i]], type = type, test = "F") |> as.data.frame() |> row.names()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2]  |> 
      rep(times = nrow(output[[i]]))
    output[[i]]$Data = summary(models[[i]])$call["data"] |> as.character() |>  
      rep(times = nrow(output[[i]]))
    # Getting the df and columns consistent for regular and mixed ANOVA
    if (class(models[[i]]) == "lm") {
      output[[i]]$df = paste0(output[[i]]$Df, ", ", tail(output[[i]]$Df, n = 1))
      output[[i]] = dplyr::select(output[[i]], -Df)
    }
    if (str_detect(class(models[[i]]), "lmer") == TRUE) {
      output[[i]]$df = paste0(output[[i]]$Df, ", ", ifelse(output[[i]]$Df.res <10, round(output[[i]]$Df.res, 2), 
                                                           ifelse(output[[i]]$Df.res >= 10, round(output[[i]]$Df.res, 1), NA)))
      output[[i]] = dplyr::select(output[[i]], -Df, -Df.res)
    }
    names(output[[i]]) = names(output[[i]]) |> stringr::str_replace("F value", "F")
    output[[i]] = dplyr::select(output[[i]], Term, DV, Data, `F`, df, `Pr(>F)`)
    # Effect sizes
    output[[i]] = dplyr::left_join(output[[i]], 
                                   car::Anova(models[[i]], type = type, test = "F") |>
                                     effectsize::eta_squared(ci = .9, partial = partial) |> as.data.frame(), 
                                   by = c("Term" = "Parameter"))
    if (is.null(model.names) == FALSE) {
      output[[i]]$Model = rep(model.names[i], times = nrow(output[[i]]))
    }
  }
  table = do.call(rbind.data.frame, output)
  # Renaming variables so that both regular and mixed ANOVA output work
  if (is.null(model.names) == FALSE) {
    table = dplyr::select(table, Model, Term, everything()) 
  } else {
    table = dplyr::select(table, Data, DV, Term, everything()) 
  }
  if (formatted == TRUE) {
    names(table) = stringr::str_replace(names(table), "Eta2_partial", "Eta2")
    table$Result = with(table, paste0("F(", df, ") = ", formatted.round(F), 
                                      ifelse(`Pr(>F)` < .001, ", p < .001", 
                                             paste0(", p = ", formatted.round(`Pr(>F)`, digits = 3, remove.lead0 = T))),
                                      ifelse(partial == T, ", Eta_p2 [90% CI] = ", ifelse( partial == F, ", Eta2 [90% CI] = ", NA)),
                                      formatted.round(Eta2, digits = eta.digits, remove.lead0 = TRUE),
                                      " [", formatted.round(CI_low, digits = eta.digits, remove.lead0 = TRUE), ", ", 
                                      formatted.round(CI_high, digits = eta.digits, remove.lead0 = TRUE), "]"))
    table$Result = ifelse(table$Term == "Residuals", NA, table$Result)
    if (is.null(model.names) == FALSE) {
      table = table |> dplyr::select(Model, Term, Result, df) |> dplyr::filter(Term != "Residuals")
    } else {
      table = table |> dplyr::select(Data, DV, Term, Result, df) |> dplyr::filter(Term != "Residuals")
    }
  }
  row.names(table) = NULL
  table |> dplyr::select(-df) |> dplyr::filter(Term != "(Intercept)")
}
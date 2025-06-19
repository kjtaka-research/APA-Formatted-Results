#### File information
# File name: Formatted_LM_6-19-25.R
# Author: Koji J. Takahashi
# Updated: 6-19-2025

### General Comments
# These functions take any number of lm and lmer models and report results 
# Separate function for lm/lmer models and for glm/glmer models
# Formatted test results are for in-text reporting in APA style
# Can use formatted = FALSE to get the raw values
# Can enter a single model or multiple models in a list. 
# Can enter multiple as a named list and output will be labeled accordingly
# If a list is used without names, models will be identifiable by the DV and data frame 
# Regular and mixed models can be run together, but noting the type in a named list is recommended
# e.g., list("OLS Regression 1" = lm1, "LME Model 1" = lmer1) if lm1 and lmer1 are fitted models
# If OR = TRUE in the make.glm.table function, prints odds ratios and 95% Wald confidence intervals
# Option for estimating Cohen's d in LME models based on Westfall et al., 2014
## This takes all fixed and random effects in the model and (assuming all fixed effects are categorical) 
## and estimates an operative Cohen's d with an operative SD that accounts for 
## variability across levels of both fixed and random factors. 
## This option is still being fine-tuned and not recommended without further documentation (as of 6-19-2025)
## see https://github.com/kjtaka-research/lme-effects-power/blob/main/lmerd_function_6-17-25.R for this standalone function

# Making table for lm and lmer model results
make.lm.table <- function(models, formatted = T, 
                          sig.stars = T,
                          model.names = NULL,
                          side_by_side = F, rename_coef = F, 
                          old_coef_names = NULL, new_coef_names = NULL,
                          lmerd = F, # calculates Cohen's d for lmer models using the formula from Westfall et al., 2014
                          lmerd.contrast = c(-0.5, 0.5) # Sets the contrast for categorical variables used by the lmerd
                          ) {
  require(dplyr)
  require(stringr)
  require(lme4)
  library(dplyr) # Current version of code uses dplyr piping
  if (side_by_side == T) {
    require(tidyr)
  }
  # Making it so that the models argument can accommodate a single model and not just a list
  if (class(models) != "list") {
    models = list(models)
  }
  # Putting in another method of specifying model names using a named list
  if (models |> names() |> is.null() == F) {
    model.names = names(models)
  }
  APA.round = function(x) { # To get the correct number of decimal. places for APA format
    rounded.x = dplyr::case_when(abs(x) < 10 ~ trimws(format(round(x, 2), nsmall = 2), which = "left"),
                                 abs(x) > 10 ~ trimws(format(round(x, 1), nsmall = 1), which = "left"))
    rounded.x
  }
  # Function to get correct operative d estimates (based on fromulas from Westfall et al., 2014)
  lmer.d <- function (model = model, contrast = c(0.5, -0.5)) {
    lmerd.output = list()
    ba = max(contrast) - min(contrast) # For transformations needed to make calculations insensitive to contrast weights
    c2 = (ba/2)^2 # For transforming random slope variance
    contr.est = lme4::fixef(model) * ba # Extracting fixed effects, transformed for contrast weights
    contr.SE = pull(as.data.frame(summary(model)$coefficients), `Std. Error`) * ba
    contr.CI.low = contr.est - (1.96 * contr.SE)
    contr.CI.high = contr.est + (1.96 * contr.SE)
    unweighted.variances = list()
    variance.list = list()
    for (j in c(1:length(lme4::VarCorr(model)))) {
      variance.list[[j]] = diag(as.matrix(lme4::VarCorr(model)[[j]])) #to extract variances and not covariances
      unweighted.variances[[j]] = variance.list[[j]]
      variance.list[[j]][stringr::str_detect(names(variance.list[[j]]), "Intercept") == F] = 
        variance.list[[j]][stringr::str_detect(names(variance.list[[j]]), "Intercept") == F] * c2
    }
    variance.list[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
    unweighted.variances[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
    lmerd.output$sd.pooled = variance.list |> unlist() |> sum() |> sqrt()
    lmerd.output$d.est = contr.est / lmerd.output$sd.pooled
    lmerd.output$d.CI.low = contr.CI.low / lmerd.output$sd.pooled
    lmerd.output$d.CI.high = contr.CI.high / lmerd.output$sd.pooled
    lmerd.output$weighted.variances = variance.list |> unlist()
    lmerd.output$unweighted.variances = unweighted.variances |> unlist()
    lmerd.output
  }
  output = list()
  df.lm = list()
  for (i in c(1:length(models))) {
    output[[i]] =  summary(models[[i]])$coefficients |> as.data.frame()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2]  |> 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Term = row.names(summary(models[[i]])$coefficients)
    output[[i]]$Data = summary(models[[i]])$call["data"] |> as.character() |> 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Class = rep(class(models[[i]])[1] , times = nrow(summary(models[[i]])$coefficients))
    df.lm[[i]] = ifelse(class(models[[i]])[1] == "lm", 
                        rep(summary(models[[i]])$df[2], times = nrow(summary(models[[i]])$coefficients)),
                        ifelse(class(models[[i]])[1] == "lmerModLmerTest" | class(models[[i]])[1] == "lmerMod" |
                                 class(models[[i]])[1] == "glmerModLmerTest" | class(models[[i]])[1] == "glmerMod", 
                               as.data.frame(summary(models[[i]])$coefficients)$df, NA)) |>
      rep(times = nrow(summary(models[[i]])$coefficients))
    if (lmerd == T) {
      output[[i]]$d.est = lmer.d(model = models[[i]], contrast = lmerd.contrast)$d.est
      output[[i]]$d.CI.low = lmer.d(model = models[[i]], contrast = lmerd.contrast)$d.CI.low
      output[[i]]$d.CI.high = lmer.d(model = models[[i]], contrast = lmerd.contrast)$d.CI.high
    }
    if (is.null(model.names) == FALSE) {
      output[[i]]$Model = rep(model.names[i], times = nrow(summary(models[[i]])$coefficients))
    }
  }
  table = do.call(rbind.data.frame, output)
  table$df = ifelse(table$Class == "lm", as.vector(unlist(df.lm)), 
                    ifelse(table$Class == "lmerModLmerTest" | table$Class == "lmerMod", table$df))
  row.names(table) = NULL
  # Added to change code so that it is insensitive to t or z tests. If this creates errors, replace p.value with `Pr(>|t|)` in later code
  names(table)[stringr::str_detect(names(table), "Pr") == T] <- "p.value"
  table$stars = dplyr::case_when(table$p.value < .001 ~ "***", 
                                 table$p.value > .001 & table$p.value < .01 ~ "**",
                                 table$p.value > .01 & table$p.value < .05 ~ "*",
                                 table$p.value > .05 & table$p.value < .1 ~ "`",
                                 .default = "")
  if (is.null(model.names) == FALSE) {
    table = dplyr::select(table, Model, Term, Estimate, everything()) 
  } else {
    table = dplyr::select(table, Data, DV, Term, Estimate, everything()) 
  }
  if (side_by_side == T) {
    # override default
    formatted = F
    table$Result = paste0(APA.round(table$Estimate), " ", table$stars)
    if (rename_coef == T) { 
      for (coef_no in c(1:length(old_coef_names))) { 
        table$Term = stringr::str_replace_all(table$Term, old_coef_names[coef_no], new_coef_names[coef_no])
      }
    }
    table = dplyr::select(table, Model, Term, Result) |>
      tidyr::pivot_wider(id_cols = Term, names_from = Model, values_from = Result)
  }
  if (formatted == TRUE) {
    # Added to make code insensitive to t or z tests
    table$TestType = ifelse(sum(stringr::str_detect(names(table), "t value")) == 1, "t",
                            ifelse(sum(stringr::str_detect(names(table), "z value")) == 1, "z", NA)) |>
      rep(times = nrow(table))
    names(table)[stringr::str_detect(names(table), "t value|z value") == T] = "TestValue"
    # Key code
    table$Result = with(table, 
                        paste0("B = ", APA.round(Estimate), ", SE = ", APA.round(`Std. Error`),
                               ", ", dplyr::case_when(TestType == "z" ~ "z", 
                                                      TestType == "t" ~ paste0("t(", APA.round(df), ")")), 
                               " = ", trimws(format(round(TestValue, 2), nsmall = 2), which = "left"), 
                               ifelse(p.value < .001, ", p < .001", paste0(", p = ", 
                                                                           stringr::str_remove(format(round(p.value, 3), nsmall = 3), "^0+")))))
    
    if (lmerd == T) {
      table$Result = paste0(table$Result, ", d = ", APA.round(table$d.est), 
                            " [", APA.round(table$d.CI.low), ", ", APA.round(table$d.CI.high), "]")
    }
    if (is.null(model.names) == FALSE) {
      table = table |> dplyr::select(Model, Term, Result, stars)
    } else {
      table = table |> dplyr::select(Data, DV, Term, Result, stars)
    }
    if (sig.stars == F) {
      table = dplyr::select(table, -stars)
    }
  }
  table
}


# Making glm and glmer tables

make.glm.table <- function(models, formatted = T, OR = F, log = F, 
                           model.names = NULL, sig.stars = T,
                           side_by_side = F, rename_coef = F, 
                           old_coef_names = NULL, new_coef_names = NULL) {
  output = list()
  df.lm = list()
  # Making it so that the models argument can accommodate a single model and not just a list
  if (class(models) != "list") {
    models = list(models)
  }
  # Putting in another method of specifying model names using a named list
  if (models |> names() |> is.null() == F) {
    model.names = names(models)
  }
  APA.round = function(x) { # To get the correct number of decimal. places for APA format
    rounded.x = dplyr::case_when(abs(x) < 10 ~ trimws(format(round(x, 2), nsmall = 2), which = "left"),
                                 abs(x) > 10 ~ trimws(format(round(x, 1), nsmall = 1), which = "left"))
    rounded.x
  }
  for (i in c(1:length(models))) {
    output[[i]] =  summary(models[[i]])$coefficients |> as.data.frame()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2]  |> 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Term = row.names(summary(models[[i]])$coefficients)
    output[[i]]$Data = summary(models[[i]])$call["data"] |> as.character() |> 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Class = rep(class(models[[i]])[1] , times = nrow(summary(models[[i]])$coefficients))
    if (is.null(model.names) == FALSE) {
      output[[i]]$Model = rep(model.names[i], times = nrow(summary(models[[i]])$coefficients))
    }
  }
  table = do.call(rbind.data.frame, output)
  row.names(table) = NULL
  names(table)[stringr::str_detect(names(table), "Pr") == T] <- "p.value"
  stars = dplyr::case_when(table$p.value < .001 ~ "***", 
                           table$p.value > .001 & table$p.value < .01 ~ "**",
                           table$p.value > .01 & table$p.value < .05 ~ "*",
                           table$p.value > .05 & table$p.value < .1 ~ "`",
                           .default = "")
  if (is.null(model.names) == FALSE) {
    table = dplyr::select(table, Model, Term, Estimate, everything()) 
  } else {
    table = dplyr::select(table, Data, DV, Term, Estimate, everything()) 
  }
  if (OR == TRUE) {
    table$OR = exp(table$Estimate)
    table$OR.CI.low = exp((table$Estimate - (1.96 * table$`Std. Error`)))
    table$OR.CI.high = exp((table$Estimate + (1.96 * table$`Std. Error`)))
  }
  if (side_by_side == T) {
    # override default
    formatted = F
    table$Result = paste0(APA.round(table$Estimate), " ", stars)
    if (rename_coef == T) { 
      for (coef_no in c(1:length(old_coef_names))) { 
        table$Term = stringr::str_replace_all(table$Term, old_coef_names[coef_no], new_coef_names[coef_no])
      }
    }
    table = dplyr::select(table, Model, Term, Result) |>
      pivot_wider(id_cols = Term, names_from = Model, values_from = Result)
  }
  if (formatted == TRUE) {
    # Added to make code insensitive to t or z tests
    table$TestType = ifelse(sum(stringr::str_detect(names(table), "t value")) == 1, "t",
                            ifelse(sum(stringr::str_detect(names(table), "z value")) == 1, "z", NA)) |>
      rep(times = nrow(table))
    names(table)[stringr::str_detect(names(table), "t value|z value") == T] <- "TestValue"
    # Making formatted results
    table$Result = with(table, 
                        paste0("B = ", APA.round(Estimate), ", SE = ", APA.round(`Std. Error`),
                               ", ", TestType, " = ",  trimws(format(round(TestValue, 2), nsmall = 2), which = "left"), 
                               ifelse(p.value < .001, ", p < .001", 
                                      paste0(", p = ", 
                                             stringr::str_remove(format(round(p.value, 3), nsmall = 3), "^0+")))))
    if (OR == TRUE) {
      table$Result = with(table, paste0(Result, ", OR = ", APA.round(OR), " [", 
                                        APA.round(OR.CI.low), ", ", APA.round(OR.CI.high), "]"))
    }
    if (log == TRUE) {
      table$Result = stringr::str_replace(table$Result, "B", "B log")
    }
    if (is.null(model.names) == FALSE) {
      table = table |> dplyr::select(Model, Term, Result)
    } else {
      table = table |> dplyr::select(Data, DV, Term, Result)
    }
    if (sig.stars == T) {
      table$Significance = stars
    }
  }
  table
}

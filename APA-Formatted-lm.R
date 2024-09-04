# This is to make a table of results from linear models, t-tests, and ANOVAs
## Formatted for pasting results into the main text of manuscript in APA format

# Function to make lm or lmer tables

make.lm.table <- function(models, formatted = T, 
                          lmerd = F, # if TRUE, calculates Cohen's d using the formula from Westfall et al., 2014
                          lmerd.contrast = c(-0.5, 0.5) # Sets the contrast for categorical variables used by the lmerd argument (i.e., can manually set to c(-1, 1) if those are the contrast weights you are using)
) { 
  output = list()
  df.lm = list()
  for (i in c(1:length(models))) {
    output[[i]] =  summary(models[[i]])$coefficients %>% as.data.frame()
    output[[i]]$DV = all.names(summary(models[[i]])$call["formula"])[2]  %>% 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Term = row.names(summary(models[[i]])$coefficients)
    output[[i]]$Data = summary(models[[i]])$call["data"] %>% as.character() %>% 
      rep(times = nrow(summary(models[[i]])$coefficients))
    output[[i]]$Class = rep(class(models[[i]])[1] , times = nrow(summary(models[[i]])$coefficients))
    df.lm[[i]] = ifelse(class(models[[i]])[1] == "lm", 
                        rep(summary(models[[i]])$df[2], times = nrow(summary(models[[i]])$coefficients)),
                        ifelse(class(models[[i]])[1] == "lmerModLmerTest" | class(models[[i]])[1] == "lmerMod" |
                                 class(models[[i]])[1] == "glmerModLmerTest" | class(models[[i]])[1] == "glmerMod", 
                               as.data.frame(summary(models[[i]])$coefficients)$df, NA)) %>%
      rep(times = nrow(summary(models[[i]])$coefficients))
    if (lmerd == T) {
      ba = max(lmerd.contrast) - min(lmerd.contrast)
      output[[i]]$contr.est = output[[i]]$Estimate * ba
      output[[i]]$sd.pooled = rep(sqrt(lme4::VarCorr(models[[i]]) %>% dplyr::as_tibble() %>% dplyr::pull(vcov) %>% sum()),
                                  times = nrow(summary(models[[i]])$coefficients))
      output[[i]]$d.est = output[[i]]$contr.est / output[[i]]$sd.pooled
      output[[i]] = dplyr::select(output[[i]], -contr.est, )
    }
  }
  table = do.call(rbind.data.frame, output)
  table$df = ifelse(table$Class == "lm", as.vector(unlist(df.lm)), 
                    ifelse(table$Class == "lmerModLmerTest" | table$Class == "lmerMod", table$df))
  row.names(table) = NULL
  table = dplyr::select(table, Data, DV, Term, Estimate, everything())
  if (formatted == TRUE) {
    table$Result = with(table, 
                        paste0("B = ", ifelse(abs(Estimate) <10, format(round(Estimate, 2), nsmall = 2), 
                                              ifelse(abs(Estimate) >= 10, format(round(Estimate, 1), nsmall = 1), NA)), 
                               ", SE = ", ifelse(`Std. Error` <10, format(round(`Std. Error`, 2), nsmall = 2), 
                                                 ifelse(`Std. Error` >= 10, format(round(`Std. Error`, 1), nsmall = 1), NA)),
                               ", t(", ifelse(df <10, round(df, 2), ifelse(df >= 10, round(df, 1), NA)), 
                               ") = ", format(round(`t value`, 2), nsmall = 2), 
                               ifelse(`Pr(>|t|)` < .001, ", p < .001", 
                                      paste0(", p = ", 
                                             stringr::str_remove(format(round(`Pr(>|t|)`, 3), nsmall = 3), "^0+")))))
    if (lmerd == T) {
      table$Result = with(table, paste0(Result, ", d = ", 
                                        ifelse(abs(d.est) < 10, format(round(d.est, 2), nsmall = 2), 
                                               ifelse(abs(d.est) >= 10, format(round(d.est, 1), nsmall = 1), NA))))
    }
    table = table %>% dplyr::select(Data, DV, Term, Result)
  }
  table
}

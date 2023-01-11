##### Linear Regression Summary Table Function #####
regsumtab <- function(data, dependent, explanatory, include) {
  require("dplyr")
  require("tidyverse")
  require("MASS")
  
  # Output structure
  out <- data.frame(labs = c("r2", "p", "AIC",
                             c("(Intercept)",
                               unlist(sapply(explanatory, function(x) {
                                 return(if(is.numeric(data[, x])) {
                                   return(x)}
                                   else {paste0(x, levels(eval(parse(text = paste0("data$", x)))))})
                               })))))
  
  # Univariate models and results
  uni <- lapply(explanatory, function(x) {
    y <- lm(as.formula(
      paste(dependent, x, sep = "~")
    ), data)
    
    coef <- as.data.frame(cbind(summary(y)$coefficients, confint.default(y))) %>%
      rename("lci" = "2.5 %",
             "uci" = "97.5 %") %>% 
      rownames_to_column() %>%
      filter(rowname != "(Intercept)") %>%
      mutate(uni = paste0(
        sprintf("%.2f", round(Estimate, 2)),
        " (",
        sprintf("%.2f", round(lci, 2)),
        " to ",
        sprintf("%.2f", round(uci, 2)),
        ")")) %>%
      dplyr::select(rowname, uni)
  }) %>%
    bind_rows()
  
  out <- out %>%
    left_join(uni, by = c("labs" = "rowname"))
  
  # Multivariate models of included variables
  m.mod <- lm(as.formula(paste(dependent,
                               paste(include,
                                     collapse = "+"),
                               sep = "~")),
              data)
  
  m1 <- as.data.frame(cbind(summary(m.mod)$coefficients, confint.default(m.mod))) %>%
    rename("lci" = "2.5 %",
           "uci" = "97.5 %") %>%
    mutate(m1 = paste0(
      sprintf("%.2f", round(Estimate, 2)),
      " (",
      sprintf("%.2f", round(lci, 2)),
      " to ",
      sprintf("%.2f", round(uci, 2)),
      ")")) %>%
    dplyr::select(m1) %>%
    bind_rows(data.frame(
      m1 = c(
        # R-squared
        sprintf("%.2f",
                round(summary(m.mod)$r.squared,
                      2)), 
        # P value
        if_else(pf(summary(m.mod)$fstatistic[1],
                   summary(m.mod)$fstatistic[2],
                   summary(m.mod)$fstatistic[3],
                   lower.tail = FALSE)
                < 0.001,
                "<0.001",
                as.character(sprintf("%.3f",
                                     round(pf(summary(m.mod)$fstatistic[1],
                                              summary(m.mod)$fstatistic[2],
                                              summary(m.mod)$fstatistic[3]),
                                           3)))),
        # AIC
        round(AIC(m.mod), 0)),
      row.names = c("r2", "p", "AIC"))) %>%
    rownames_to_column()
  
  out <- out %>%
    left_join(m1, by = c("labs" = "rowname"))
  
  # Multivariable models of potential variables
  m <- explanatory[explanatory != include]
  
  multi <- lapply(m, function(x) {
    s <- which(m == x)
    y <- lm(as.formula(paste(dependent,
                             paste(include, x, sep = "+"),
                             sep = "~")),
            data)
    
    z <- as.data.frame(cbind(summary(y)$coefficients, confint.default(y))) %>%
      rename("lci" = "2.5 %",
             "uci" = "97.5 %") %>%
      mutate(value = paste0(
        sprintf("%.2f", round(Estimate, 2)),
        " (",
        sprintf("%.2f", round(lci, 2)),
        " to ",
        sprintf("%.2f", round(uci, 2)),
        ")")) %>%
      dplyr::select(value) %>%
      bind_rows(data.frame(
        value = c(
          # R-squared
          sprintf("%.2f",
                  round(summary(y)$r.squared,
                        2)), 
          # P value
          if_else(pf(summary(y)$fstatistic[1],
                     summary(y)$fstatistic[2],
                     summary(y)$fstatistic[3],
                     lower.tail = FALSE)
                  < 0.001,
                  "<0.001",
                  as.character(sprintf("%.3f",
                                       round(pf(summary(y)$fstatistic[1],
                                                summary(y)$fstatistic[2],
                                                summary(y)$fstatistic[3]),
                                             3)))),
          # AIC
          round(AIC(y), 0)),
        row.names = c("r2", "p", "AIC"))) %>%
      rownames_to_column() %>%
      rename(setNames("value", paste0("m", s + 1)))
    
    return(z)
    
  })
  
  # Combining with output
  for (ii in seq_along(multi)) {
    out <- out %>%
      left_join(multi[[ii]], by = c("labs" = "rowname"))
  }
  
  # Stepwise best fit
  null <- lm(as.formula(paste0(dependent, "~1")), data)
  full <- lm(as.formula(paste(dependent,
                              paste(explanatory, collapse = "+"),
                              sep = "~")),
             data)
  
  s.mod <- stepAIC(full,
                   direction = "both",
                   scope = list(upper = full,
                                lower = null),
                   trace = FALSE)
  
  s <- as.data.frame(cbind(summary(s.mod)$coefficients, confint.default(s.mod))) %>%
    rename("lci" = "2.5 %",
           "uci" = "97.5 %") %>%
    mutate(best = paste0(
      sprintf("%.2f", round(Estimate, 2)),
      " (",
      sprintf("%.2f", round(lci, 2)),
      " to ",
      sprintf("%.2f", round(uci, 2)),
      ")")) %>%
    dplyr::select(best) %>%
    bind_rows(data.frame(
      best = c(
        # R-squared
        sprintf("%.2f",
                round(summary(s.mod)$r.squared,
                      2)), 
        # P value
        if_else(pf(summary(s.mod)$fstatistic[1],
                   summary(s.mod)$fstatistic[2],
                   summary(s.mod)$fstatistic[3],
                   lower.tail = FALSE)
                < 0.001,
                "<0.001",
                as.character(sprintf("%.3f",
                                     round(pf(summary(s.mod)$fstatistic[1],
                                              summary(s.mod)$fstatistic[2],
                                              summary(s.mod)$fstatistic[3]),
                                           3)))),
        # AIC
        round(AIC(s.mod), 0)),
      row.names = c("r2", "p", "AIC"))) %>%
    rownames_to_column()
  
  # Combining with output
  out <- out %>%
    left_join(s, by = c("labs" = "rowname"))
  
  # Replacing NA for clean formatting
  out <- out %>%
    replace(is.na(.), "---")
  
  return(out)
}
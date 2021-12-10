tidy.melded <- function(x, conf.int = FALSE, conf.level = 0.95) {
  # Get the df from one of the models
  model_degrees_freedom <- x[[1]]$df.residual
  
  params <- tibble(models = x) %>%
    mutate(m = 1:n(),
           tidied = map(models, tidy)) %>%
    unnest(cols = tidied) %>%
    select(m, term, estimate, std.error) 
  
  output <- params %>%
    mutate(term = fct_inorder(term)) %>%
    group_by(term) %>%
    summarise(theta = mean(estimate),
              sigma = sqrt(mean(std.error^2) + 1.2 * sum((estimate - mean(estimate))^2)/4),
              .groups = "drop")
  
  # Create tidy output
  output <- output %>%
    mutate(statistic = theta/sigma,
           p.value = 2 * pt(abs(statistic), model_degrees_freedom, lower.tail = FALSE)) %>%
    rename(estimate = theta,
           std.error = sigma)
  
  # Add confidence intervals if needed
  if (conf.int & conf.level) {
    # Convert conf.level to tail values (0.025 when it's 0.95)
    a <- (1 - conf.level) / 2
    
    output <- output %>% 
      mutate(conf.low = estimate + std.error * qt(a, model_degrees_freedom),
             conf.high = estimate + std.error * qt((1 - a), model_degrees_freedom))
  }
  
  # tidy objects only have a data.frame class, not tbl_df or anything else
  class(output) <- "data.frame"
  output
}

glance.melded <- function(x) {
  output <- x %>%
    map_df(~tibble(
      nobs = nobs(.),
      null.deviance = .$null.deviance,
      deviance = .$deviance,
      null.df = degf(.$survey.design),
      df = .$df.residual
    )) %>%
    mutate(r.squared = 1 - deviance/null.deviance,
           adj.rsq = 1 - (1 - r.squared) * null.df/df) %>%
    summarise_at(.vars = vars(-contains("deviance")),
                 .funs = mean)
  
  # glance objects only have a data.frame class, not tbl_df or anything else
  class(output) <- "data.frame"
  output
}
library(tidyverse)
library(survey)
library(mitools)
library(broom)
library(texreg)
library(mice)
library(miceadds)
library(kableExtra)
library(car)
library(jtools)
library(modelr)
library(tikzDevice)
source("tidy-melded-functions.R")

######################################## STEP 1 #######################################
######################## A table with descriptive statistics ##########################
############################ (see Table 5 in the appendix) ############################

# Loading in the subset of the whole dataset to be used in the main analysis
load("icils-2018-data-with-noncogs-subset.RData")

# Defining survey design object
des <- svrepdesign(data = students1, weights = ~totwgts, repweights = "srwgt[0-9]+", type = "JK1", scale = 1, mse = TRUE, combine.weights = TRUE)

t1 <- students1 %>%
  group_by(cntry) %>%
  summarise_at(.vars = vars(gender, s_age, s_immbgr, starts_with("s_excomp_"), s_comphome,
                            s_degree, high, -contains("sfe"), timedrop, perfdrop, nrr),
               .funs = list(mean = mean, sd = sd), na.rm = TRUE)

t1 <- t1 %>%
  rename_with(.fn = ~str_remove(string = ., pattern = "s\\_")) %>%
  rename_with(.fn = ~str_replace(string = ., pattern = "excomp\\_", replacement = "excomp."))

t1 <- t1 %>%
  pivot_longer(cols = -cntry, names_to = c("variable", "stat"), names_sep = "_",
               values_to = "value") %>%
  mutate(variable = fct_inorder(variable)) %>%
  pivot_wider(names_from = "cntry", values_from = "value") %>%
  arrange(variable) %>%
  mutate(stat = factor(stat, levels = c("mean", "sd"), labels = c("Mean", "S.D")))

t1 %>%
  mutate(variable = fct_inorder(rep(c("Female", "Age", "Migrant", "Computer experience: 1 - 3 years", "Computer experience: 3 - 5 years", "Computer experience: 5 - 7 years", "Computer experience: 7+ years", "Computers at home", "Parental education", "Parental occupation", "Time drop", "Performance drop", "Response rate"), each = 2))) %>%
  rename(Variable = variable, Statistic = stat) %>%
  kable(digits = 2, format = "latex", caption = "Descriptive statistics", label = "desc", booktabs = TRUE) %>%
  kable_styling() %>%
  collapse_rows(columns = 1, valign = "top") %>%
  landscape() %>%
  cat(., file = "descriptive-stats.tex")

# Tidying up
rm(t1)

######################################## STEP 2 ########################################
################# Correlations between proxies for noncognitive skills #################
############################ (see Table 2 in the main text) ############################

t2 <- svycor(~timedrop + perfdrop + nrr, design = des, sig.stats = TRUE)

# A matrix of correlation coefficients
t2$cors

# A matrix of standard errors of the coefficients
t2$std.err

# A matrix of the values of t-statistics 
t2$t.values

# A matrix of the p-values for the t-tests
t2$p.values

t2 <- t2$cors
rownames(t2) <- colnames(t2) <- c("Drop in response time", "Performance drop", "Item response rate")
kable(x = t2, format = "latex", digits = 2, booktabs = TRUE, caption = "Pairwise correlations between the proxies for non-cognitive skills; ICILS 2018, pooled sample", label = "corr") %>%
  kable_styling() %>%
  footnote(general = "All the correlations are significant at the level of $p = 0.001$.") %>%
  cat(., file = "corrs.tex")

# Tidying up
rm(t2)

######################################## STEP 3 ########################################
######## Models with PARENTAL EDUCATION as a proxy for socioeconomic background ########
############################ (see Table 3 in the main text) ############################

# STEP 3.1: REGRESSION MODELS

# Baseline model: no proxies for non-cognitive skills

mod.0 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.1 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.2 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.3 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.4 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# Step 3.2: Building regression tables

mod.sum.0 <- tibble(
  models = list(mod.0, mod.1, mod.2, mod.3, mod.4),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.0 <- mod.sum.0 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.0$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home",
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+", 
                                "Parents with higher education", "Time drop",  
                                "Performance drop", "Nonresponse"))

texreg(l = mod.sum.0$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "Parents with higher education", "Time drop",  "Performance drop",
                             "Nonresponse"),
       booktabs = TRUE, dcolumn = TRUE, file = "cil-sfe-degree.tex", use.packages = FALSE, 
       caption = "Results from the fixed-effects linear models for the CIL test scores", 
       caption.above = T, label = "tab:cil-degree")

# STEP 3.3: Testing the hypotheses
# A comparison of Models 1 and 2 in Table 3

mod.1.2 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.2 <- mod.1.2 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 2")

# A comparison of Models 1 and 3 in Table 3

mod.1.3 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.3 <- mod.1.3 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 3")

# A comparison of Models 1 and 4 in Table 3

mod.1.4 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.4 <- mod.1.4 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 4")

# A comparison of Models 1 and 5 in Table 3

mod.1.5 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.5 <- mod.1.5 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 5")

t3 <- list(mod.1.2, mod.1.3, mod.1.4, mod.1.5) %>%
  bind_rows() %>%
  group_by(term, Comparison) %>%
  summarise(m = mean(theta),
            s = sqrt(mean(SE^2) + 1.2 * sum((theta - mean(theta))^2)/4),
            .groups = "drop")

t3 <- t3 %>%
  mutate(lower = m + qnorm(0.025) * s,
         upper = m + qnorm(0.975) * s,
         term = str_remove_all(string = term, pattern = "s\\_|\\_sfe"),
         term = factor(term, levels = c("gender", "degree", "immbgr"), 
                      labels = c("Gender", "Parents with higher education",
                                 "Immigrant background")))

tikz(file = "model-comparison.tex", width = 6, height = 3)
t3 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# STEP 3.4: Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model. This is not included in the manuscript, but is available upon request (see footnote 18)

mod.01 <- map2(.x = mod.0, .y = mod.1, .f = anova, method = "Wald", test = "F")
mod.02 <- map2(.x = mod.0, .y = mod.2, .f = anova, method = "Wald", test = "F")
mod.03 <- map2(.x = mod.0, .y = mod.3, .f = anova, method = "Wald", test = "F")
mod.04 <- map2(.x = mod.0, .y = mod.4, .f = anova, method = "Wald", test = "F")

mod.01.f <- mod.01 %>%
  map_dbl(~.$Ftest)
mod.02.f <- mod.02 %>%
  map_dbl(~.$Ftest)
mod.03.f <- mod.03 %>%
  map_dbl(~.$Ftest)
mod.04.f <- mod.04 %>%
  map_dbl(~.$Ftest)

ftest.0 <- rbind(
  micombine.F(Fvalues = mod.01.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.02.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.03.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.04.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.0 <- ftest.0 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.0 %>%
  kable(format = "latex", 
        booktabs = T,
        caption = "Results of the $F$-tests for models with parental education as the indicator of socioeconomic status", 
        label = "tab:degree-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "degree-ftests.tex")

# Step 3.5: Calculating variance inflation factors. Again, not included in the main text or the appendix, but available upon request

mod.0.vif <- mod.0 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.0.vif <- mod.0.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.1.vif <- mod.1 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.1.vif <- mod.1.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.2.vif <- mod.2 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.2.vif <- mod.2.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.3.vif <- mod.3 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.3.vif <- mod.3.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.4.vif <- mod.4 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.4.vif <- mod.4.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.0.vif <- list(mod.0.vif, mod.1.vif, mod.2.vif, mod.3.vif, mod.4.vif) %>%
  bind_rows(.id = "Model")

write_delim(x = mod.0.vif, file = "icils-2018-degree-vif.csv", delim = ";")

# Tidying up. Removing all the objects except from the data, survey design, the .melded functions, and the regression models
rm(list = ls()[!ls() %in% c("students1", "des", paste0("mod.", 0:4), "tidy.melded", "glance.melded")])

######################################## STEP 4 ########################################
######## Models with PARENTAL OCCUPATION as a proxy for socioeconomic background #######
############################ (see Table 4 in the main text) ############################

# STEP 4.1: REGRESSION MODELS

# Baseline model: no proxies for non-cognitive skills

mod.10 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.11 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.12 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.13 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.14 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# STEP 4.2: Building regression models

mod.sum.1 <- tibble(
  models = list(mod.10, mod.11, mod.12, mod.13, mod.14),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.1 <- mod.sum.1 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.1$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home",
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+", 
                                "Parental occupation", "Time drop",  
                                "Performance drop", "Nonresponse"))

texreg(l = mod.sum.1$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "Parental occupation", "Time drop",  "Performance drop", 
                             "Nonresponse"),
       booktabs = TRUE, dcolumn = TRUE, file = "cil-sfe-occ.tex", use.packages = FALSE, 
       caption = "Results from the fixed-effects linear models for the CIL test scores", 
       caption.above = T, label = "tab:cil-occ")

# STEP 4.3: Testing the hypotheses
# A comparison of Models 1 and 2 in Table 4

mod.1.2 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.2 <- mod.1.2 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 2")

# A comparison of Models 1 and 3 in Table 4

mod.1.3 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + perfdrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.3 <- mod.1.3 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 3")

# A comparison of Models 1 and 4 in Table 4

mod.1.4 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + nrr_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.4 <- mod.1.4 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 4")

# A comparison of Models 1 and 5 in Table 4

mod.1.5 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.5 <- mod.1.5 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 5")

t4 <- list(mod.1.2, mod.1.3, mod.1.4, mod.1.5) %>%
  bind_rows() %>%
  group_by(term, Comparison) %>%
  summarise(m = mean(theta),
            s = sqrt(mean(SE^2) + 1.2 * sum((theta - mean(theta))^2)/4),
            .groups = "drop")

t4 <- t4 %>%
  mutate(lower = m + qnorm(0.025) * s,
         upper = m + qnorm(0.975) * s,
         term = str_remove_all(string = term, pattern = "s\\_|\\_sfe"),
         term = factor(term, levels = c("gender", "high", "immbgr"), 
                       labels = c("Gender", "Parental occupation",
                                  "Immigrant background")))

tikz(file = "model-comparison-occ.tex", width = 6, height = 3)
t4 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# STEP 4.4: Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model
# This is not included in the manuscript, but is available upon request (see footnote 18)

mod.11 <- map2(.x = mod.10, .y = mod.11, .f = anova, method = "Wald", test = "F")
mod.12 <- map2(.x = mod.10, .y = mod.12, .f = anova, method = "Wald", test = "F")
mod.13 <- map2(.x = mod.10, .y = mod.13, .f = anova, method = "Wald", test = "F")
mod.14 <- map2(.x = mod.10, .y = mod.14, .f = anova, method = "Wald", test = "F")

mod.11.f <- mod.11 %>%
  map_dbl(~.$Ftest)
mod.12.f <- mod.12 %>%
  map_dbl(~.$Ftest)
mod.13.f <- mod.13 %>%
  map_dbl(~.$Ftest)
mod.14.f <- mod.14 %>%
  map_dbl(~.$Ftest)

ftest.1 <- rbind(
  micombine.F(Fvalues = mod.11.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.12.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.13.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.14.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.1 <- ftest.0 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.1 %>%
  kable(format = "latex", 
        booktabs = TRUE,
        caption = "Results of the $F$-tests for models with parental occupation as the indicator of socioeconomic status", 
        label = "tab:occ-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "occ-ftests.tex")

# STEP 4.5: Calculating variance inflation factors

mod.10.vif <- mod.10 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.10.vif <- mod.10.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.11.vif <- mod.11 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.11.vif <- mod.11.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.12.vif <- mod.12 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.12.vif <- mod.12.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.13.vif <- mod.13 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.13.vif <- mod.13.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.14.vif <- mod.14 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.14.vif <- mod.14.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.10.vif <- list(mod.10.vif, mod.11.vif, mod.12.vif, mod.13.vif, mod.14.vif) %>%
  bind_rows(.id = "Model")

write_delim(x = mod.10.vif, file = "icils-2018-occ-vif.csv", delim = ";")

# Tidying up. Removing all the objects except from the data, survey design, the .melded functions, and the regression models
rm(list = ls()[!ls() %in% c("students1", "des", paste0("mod.", 0:4), paste0("mod.", 10:14), "tidy.melded", "glance.melded")])

######################################## STEP 5 ########################################
############### Models with HISEI as a proxy for socioeconomic background ##############
############################ (see Table 6 in the Appendix) #############################

# STEP 5.1: REGRESSION MODELS

# Baseline model: no proxies for non-cognitive skills

mod.20 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.21 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.22 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.23 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.24 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# STEP 5.2: Building regression tables

mod.sum.2 <- tibble(
  models = list(mod.20, mod.21, mod.22, mod.23, mod.24),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.2 <- mod.sum.2 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.2$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home",
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+", 
                                "HISEI", "Time drop", "Performance drop", "Nonresponse"))

texreg(l = mod.sum.1$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "HISEI", "Time drop",  "Performance drop", "Nonresponse"),
       booktabs = TRUE, dcolumn = TRUE, file = "cil-sfe-hisei.tex", use.packages = FALSE, 
       caption = "Results from the fixed-effects linear models for the CIL test scores", 
       caption.above = TRUE, label = "tab:cil-hisei")

# STEP 5.3: Testing the hypotheses
# A comparison of Models 1 and 2 in Table 6

mod.1.2 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + timedrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.2 <- mod.1.2 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 2")

# A comparison of Models 1 and 3 in Table 6

mod.1.3 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + perfdrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.3 <- mod.1.3 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 3")

# A comparison of Models 1 and 4 in Table 6

mod.1.4 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + nrr_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.4 <- mod.1.4 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 4")

# A comparison of Models 1 and 5 in Table 6

mod.1.5 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_hisei_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.5 <- mod.1.5 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 5")

t5 <- list(mod.1.2, mod.1.3, mod.1.4, mod.1.5) %>%
  bind_rows() %>%
  group_by(term, Comparison) %>%
  summarise(m = mean(theta),
            s = sqrt(mean(SE^2) + 1.2 * sum((theta - mean(theta))^2)/4),
            .groups = "drop")

t5 <- t5 %>%
  mutate(lower = m + qnorm(0.025) * s,
         upper = m + qnorm(0.975) * s,
         term = str_remove_all(string = term, pattern = "s\\_|\\_sfe"),
         term = factor(term, levels = c("gender", "hisei", "immbgr"), 
                       labels = c("Gender", "HISEI",
                                  "Immigrant background")))

tikz(file = "model-comparison-hisei.tex", width = 6, height = 3)
t5 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term, scales = "free_y") + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# Step 5.4: Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model
# This is not included in the manuscript, but is available upon request (see footnote 18)

mod.21 <- map2(.x = mod.20, .y = mod.21, .f = anova, method = "Wald", test = "F")
mod.22 <- map2(.x = mod.20, .y = mod.22, .f = anova, method = "Wald", test = "F")
mod.23 <- map2(.x = mod.20, .y = mod.23, .f = anova, method = "Wald", test = "F")
mod.24 <- map2(.x = mod.20, .y = mod.24, .f = anova, method = "Wald", test = "F")

mod.21.f <- mod.21 %>%
  map_dbl(~.$Ftest)
mod.22.f <- mod.22 %>%
  map_dbl(~.$Ftest)
mod.23.f <- mod.23 %>%
  map_dbl(~.$Ftest)
mod.24.f <- mod.24 %>%
  map_dbl(~.$Ftest)

ftest.2 <- rbind(
  micombine.F(Fvalues = mod.21.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.22.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.23.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.24.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.2 <- ftest.2 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.2 %>%
  kable(format = "latex", 
        booktabs = TRUE,
        caption = "Results of the $F$-tests for models with HISEI as the indicator of socioeconomic status", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "hisei-ftests.tex")

# Step 5.5: Calculating variance inflation factors

mod.20.vif <- mod.20 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.20.vif <- mod.20.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.21.vif <- mod.21 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.21.vif <- mod.21.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.22.vif <- mod.22 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.22.vif <- mod.22.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.23.vif <- mod.23 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.23.vif <- mod.23.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.24.vif <- mod.24 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.24.vif <- mod.24.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.20.vif <- list(mod.20.vif, mod.21.vif, mod.22.vif, mod.23.vif, mod.24.vif) %>%
  bind_rows(.id = "Model")

write_delim(x = mod.20.vif, file = "icils-2018-hisei-vif.csv", delim = ";")

# Tidying up. Removing all the objects except from the data, survey design, the .melded functions, and the regression models
rm(list = ls()[!ls() %in% c("students1", "des", paste0("mod.", 0:4), paste0("mod.", 10:14), paste0("mod.", 20:24), "tidy.melded", "glance.melded")])

######################################## STEP 6 ########################################
############### Models with NISB as a proxy for socioeconomic background ##############
############################ (see Table 7 in the Appendix) #############################

# STEP 6.1: REGRESSION MODELS

# Baseline model: no proxies for non-cognitive skills

mod.30 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.31 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.32 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.33 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.34 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# Step 6.2: Building regression tables

mod.sum.3 <- tibble(
  models = list(mod.30, mod.31, mod.32, mod.33, mod.34),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.3 <- mod.sum.3 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.3$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home",
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+", 
                                "NISB", "Time drop", "Performance drop", "Nonresponse"))

texreg(l = mod.sum.3$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "NISB", "Time drop",  "Performance drop", "Nonresponse"),
       booktabs = TRUE, dcolumn = TRUE, file = "cil-sfe-occ.tex", use.packages = FALSE, 
       caption = "Results from the fixed-effects linear models for the CIL test scores", 
       caption.above = TRUE, label = "tab:cil-nisb")

# STEP 6.3: Testing the hypotheses
# A comparison of Models 1 and 2 in Table 7

mod.1.2 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.2 <- mod.1.2 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 2")

# A comparison of Models 1 and 3 in Table 7

mod.1.3 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + perfdrop_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.3 <- mod.1.3 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 3")

# A comparison of Models 1 and 4 in Table 7

mod.1.4 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + nrr_sfe,
                              data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.4 <- mod.1.4 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 4")

# A comparison of Models 1 and 5 in Table 7

mod.1.5 <- withPV(
  mapping = cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe,
  data = des,
  action = quote(
    withReplicates(des,
                   function(w = totwgts, data = students1) {
                     m1 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe,
                              data = data, weights = w)
                     m2 <- lm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, data = data, weights = w)
                     return(coef(m1)[c(2, 4, 10)] - coef(m2)[c(2, 4, 10)])
                   })
  ))

mod.1.5 <- mod.1.5 %>%
  map(as.data.frame) %>%
  map(~mutate(.data = ., term = rownames(.))) %>%
  bind_rows(.id = "pv") %>%
  as_tibble() %>%
  mutate(Comparison = "1 vs 5")

t6 <- list(mod.1.2, mod.1.3, mod.1.4, mod.1.5) %>%
  bind_rows() %>%
  group_by(term, Comparison) %>%
  summarise(m = mean(theta),
            s = sqrt(mean(SE^2) + 1.2 * sum((theta - mean(theta))^2)/4),
            .groups = "drop")

t6 <- t6 %>%
  mutate(lower = m + qnorm(0.025) * s,
         upper = m + qnorm(0.975) * s,
         term = str_remove_all(string = term, pattern = "s\\_|\\_sfe"),
         term = factor(term, levels = c("gender", "nisb", "immbgr"), 
                       labels = c("Gender", "NISB",
                                  "Immigrant background")))

tikz(file = "model-comparison-nisb.tex", width = 6, height = 3)
t6 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# Step 6.4: Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model

mod.31 <- map2(.x = mod.30, .y = mod.31, .f = anova, method = "Wald", test = "F")
mod.32 <- map2(.x = mod.30, .y = mod.32, .f = anova, method = "Wald", test = "F")
mod.33 <- map2(.x = mod.30, .y = mod.33, .f = anova, method = "Wald", test = "F")
mod.34 <- map2(.x = mod.30, .y = mod.34, .f = anova, method = "Wald", test = "F")

mod.31.f <- mod.31 %>%
  map_dbl(~.$Ftest)
mod.32.f <- mod.32 %>%
  map_dbl(~.$Ftest)
mod.33.f <- mod.33 %>%
  map_dbl(~.$Ftest)
mod.34.f <- mod.34 %>%
  map_dbl(~.$Ftest)

ftest.3 <- rbind(
  micombine.F(Fvalues = mod.01.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.02.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.03.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.04.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.3 <- ftest.3 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.3 %>%
  kable(format = "latex", 
        booktabs = TRUE,
        caption = "Results of the $F$-tests for models with NISB as the indicator of socioeconomic status", 
        label = "tab:nisb-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "nisb-ftests.tex")

# Step 6.5: Calculating variance inflation factors

mod.30.vif <- mod.30 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.30.vif <- mod.30.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.31.vif <- mod.31 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.31.vif <- mod.31.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.32.vif <- mod.32 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.32.vif <- mod.32.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.33.vif <- mod.33 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.33.vif <- mod.33.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.34.vif <- mod.34 %>%
  map(vif) %>%
  map(enframe, name = "Variable") %>%
  bind_rows(.id = "PV")

mod.34.vif <- mod.34.vif %>%
  pivot_wider(values_from = value, names_from = PV) %>%
  rename_with(.cols = matches("[1-5]"), .fn = ~paste("PV", .)) %>%
  mutate(Variable = str_remove_all(string = Variable, pattern = "s\\_|\\_sfe"))

mod.30.vif <- list(mod.30.vif, mod.31.vif, mod.32.vif, mod.33.vif, mod.34.vif) %>%
  bind_rows(.id = "Model")

write_delim(x = mod.30.vif, file = "icils-2018-nisb-vif.csv", delim = ";")

# Tidying up. Removing all the objects except from the data, survey design, the .melded functions, and the regression models
rm(list = ls()[!ls() %in% c("students1", "des", paste0("mod.", 0:4), paste0("mod.", 10:14), paste0("mod.", 20:24), paste0("mod.", 30:34), "tidy.melded", "glance.melded")])

######################################## STEP 7 ########################################
############################## Models with interacted terms ############################
############################ (see Table 9 in the Appendix) #############################

# Step 7.1: Regression models
# Extending the baseline model: adding time drop interacted with gender

mod.1A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + timegen_sfe + timeedu_sfe + timemig_sfe, design = des)))

mod.1B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfgen_sfe + perfedu_sfe + perfmig_sfe, design = des)))

mod.1C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respgen_sfe + respedu_sfe + respmig_sfe, design = des)))

# Step 7.2: Calculating VIF for the models with interactions

mod.1A %>%
  map(vif)

mod.1B %>%
  map(vif)

mod.1C %>%
  map(vif)

# Very high VIF values for the models including interactions of item response rate with gender, parental education and immigrant background

# Step 7.3: Building regression tables
mod.sum.4 <- tibble(
  models = list(mod.1, mod.1A, mod.2, mod.1B),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.4 <- mod.sum.4 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.4$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Time drop", "Time drop X gender",
                                "Time drop X education", "Time drop X migrant", 
                                "Performance drop", "Performance drop X gender",
                                "Performance drop X education", "Performance drop X migrant"))

texreg(l = mod.sum.4$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+",
                             "Parental education", "Time drop", "Time drop X gender",
                             "Time drop X education", "Time drop X migrant", 
                             "Performance drop", "Performance drop X gender",
                             "Performance drop X education", "Performance drop X migrant"),
       file = "cil-deg-interactions.tex", caption = "Results from the fixed-effects linear models for the CIL test scores with interaction terms between the proxies for non-cognitive skills and gender, parental education, and immigrant status", label = "tab:cil-inter", caption.above = TRUE, booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE)

# Tidying up
rm(list = ls())

######################################## STEP 8 ########################################
####### Table 1 in the text --- means and standard deviations of the CIL scores ########
############################ (see Table 1 in the main text) ############################

# Loading in the whole dataset 
load("icils-2018-data-with-noncogs.RData")

# Defining survey design object
des <- svrepdesign(data = students, weights = ~totwgts, repweights = "srwgt[0-9]+", type = "JK1", scale = 1, mse = TRUE, combine.weights = TRUE)

t1 <- withPV(
  mapping = cil ~ pv1cil + pv2cil + pv3cil + pv4cil + pv5cil,
  data = des,
  action = quote(svyby(formula = ~cil, by = ~cntry, design = des, FUN = svymean))
) %>%
  MIcombine() %>%
  summary()

t1 <- tibble(cntry = rownames(t1),
             Mean = t1$results)

t2 <- withPV(
  mapping = cil ~ pv1cil + pv2cil + pv3cil + pv4cil + pv5cil,
  data = des,
  action = quote(svyby(formula = ~cil, by = ~cntry, design = des, FUN = svyvar))
) %>%
  MIcombine() %>%
  summary()

t2 <- tibble(cntry = rownames(t2),
             `S.D.` = sqrt(t2$results))

t3 <- count(students, cntry)

t3 <- left_join(t3, t1) %>%
  left_join(t2) %>%
  as_factor() %>%
  select(1, 3, 4, 2) %>%
  mutate(cntry = str_remove(string = cntry, pattern = "\\([A-Za-z\\s]+\\)|, Republic of"),
         cntry = str_trim(string = cntry))

kable(x = t3, format = "latex", digits = c(NA, 1, 1, 0), col.names = c("Country", "Mean", "S.D.", "Sample size"), format.args = list(big.mark = ","), booktabs = TRUE, caption = "Average CIL test scores, standard deviations and sample size, by participating country/region/city", label = "pvs") %>%
  cat(., file = "tab-1-pvs.tex")

# Tidying up
rm(t1, t2, t3, des)

######################################## STEP 9 #######################################
#################### An alternative treatment of the missing data #####################
############################ (see Table 8 in the appendix) ############################

# Step 9.1: Extracting and preparing for analysis a relevant subset of the data
d <- select(students, cntry, idcntry, idschool, idstud, matches("pv[1-5]cil"), s_sex, s_age, s_immbgr, is2g15aa, s_excomp, s_p1isced, s_p2isced, timedrop, perfdrop, nrr, totwgts, matches("srwgt[0-9]+"))

# Dropping observations that are "missing by design/not administered" (see Sect. A.3 in the appendix)
d <- d %>% 
  filter(!s_sex %in% 8, 
         !s_age %in% 98,
         !s_immbgr %in% 8,
         !is2g15aa %in% 8,
         !s_excomp %in% 8, 
         !s_p1isced %in% 8,
         !s_p2isced %in% 8)

# Some necessary recoding
d <- d %>%
  mutate(s_immbgr = ifelse(s_immbgr %in% 0:1, s_immbgr, 9))

d <- d %>%
  mutate_at(.vars = vars(matches("s_p[1-2]isced")),
            .funs = ~replace(., . %in% 9, NA))

d <- d %>%
  mutate(s_degree = s_p1isced %in% 4 | s_p2isced %in% 4,
         s_degree = replace(s_degree, is.na(s_p1isced) & is.na(s_p2isced), 9))

# A modified computers at home variable, recoded as binary

d <- d %>%
  mutate(s_comphome = ifelse(is2g15aa %in% 1:2, 0, 1),
         s_comphome = replace(s_comphome, is.na(is2g15aa), 9))

d <- d %>%
  mutate(s_excomp = ifelse(s_excomp %in% 0:4, s_excomp, 9))

# Creating dummy variables for s_immbgr, s_comphome, s_degree, s_excomp

d <- d %>%
  dummy_cols(select_columns = c("s_immbgr", "s_comphome", "s_degree", "s_excomp"),
             remove_first_dummy = T)
d <- drop_na(d, perfdrop)

# Transforming the variables of interest to have them centered at group means

d <- d %>%
  mutate_all(.funs = unclass) %>%
  group_by(cntry, idschool) %>%
  mutate_at(.vars = vars(matches("pv[1-5]cil"), s_sex, s_age, starts_with("s_immbgr_"),
                         starts_with("s_comphome_"), starts_with("s_excomp_"), 
                         starts_with("s_degree_"), 
                         timedrop, perfdrop, nrr),
            .funs = list(
              sfe = ~. - weighted.mean(x = ., w = totwgts)
            )) %>%
  ungroup()

# Step 9.3: Creating a new survey design object

des <- svrepdesign(data = d, weights = ~totwgts, repweights = "srwgt[0-9]+", type = "JK1", scale = 1, mse = TRUE, combine.weights = TRUE)

# Step 9.4: Regression models
mod.0 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ s_sex_sfe + s_age_sfe + s_immbgr_1_sfe + s_immbgr_9_sfe +
                          s_comphome_1_sfe + s_comphome_9_sfe + s_excomp_1_sfe + s_excomp_2_sfe +
                          s_excomp_3_sfe + s_excomp_4_sfe + s_excomp_9_sfe + s_degree_1_sfe +
                          s_degree_9_sfe, design = des)))

mod.1 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ s_sex_sfe + s_age_sfe + s_immbgr_1_sfe + s_immbgr_9_sfe +
                          s_comphome_1_sfe + s_comphome_9_sfe + s_excomp_1_sfe + s_excomp_2_sfe +
                          s_excomp_3_sfe + s_excomp_4_sfe +s_excomp_9_sfe + s_degree_1_sfe +
                          s_degree_9_sfe + timedrop_sfe, design = des)))

mod.2 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ s_sex_sfe + s_age_sfe + s_immbgr_1_sfe + s_immbgr_9_sfe +
                          s_comphome_1_sfe + s_comphome_9_sfe + s_excomp_1_sfe + s_excomp_2_sfe +
                          s_excomp_3_sfe + s_excomp_4_sfe +s_excomp_9_sfe + s_degree_1_sfe +
                          s_degree_9_sfe + perfdrop_sfe, design = des)))

mod.3 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ s_sex_sfe + s_age_sfe + s_immbgr_1_sfe + s_immbgr_9_sfe +
                          s_comphome_1_sfe + s_comphome_9_sfe + s_excomp_1_sfe + s_excomp_2_sfe +
                          s_excomp_3_sfe + s_excomp_4_sfe +s_excomp_9_sfe + s_degree_1_sfe +
                          s_degree_9_sfe + nrr_sfe, design = des)))

mod.4 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ s_sex_sfe + s_age_sfe + s_immbgr_1_sfe + s_immbgr_9_sfe +
                          s_comphome_1_sfe + s_comphome_9_sfe + s_excomp_1_sfe + s_excomp_2_sfe +
                          s_excomp_3_sfe + s_excomp_4_sfe +s_excomp_9_sfe + s_degree_1_sfe +
                          s_degree_9_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# Step 9.4: Building regression tables

mod.sum.0 <- tibble(
  models = list(mod.0, mod.1, mod.2, mod.3, mod.4),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.0 <- mod.sum.0 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.0$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant student", "Migrant status: Unknown",
                                "Computers at home: 2 or more", "Computers at home: Unknown",
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Computer experience: Unknown", "Parental education", 
                                "Parental education: Unknown",
                                "Time drop",  "Performance drop", "Nonresponse"))

texreg(l = mod.sum.0$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant student", "Migrant status: Unknown",
                             "Computers at home: 2 or more", "Computers at home: Unknown",
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+",
                             "Computer experience: Unknown", "Parental education", 
                             "Parental education: Unknown",
                             "Time drop",  "Performance drop", "Nonresponse"),
       booktabs = T, dcolumn = T, file = "cil-dfe-edu-no-miss.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-nomiss")
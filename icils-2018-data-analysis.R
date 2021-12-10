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

# Loading in the whole dataset 
load("icils-2018-data-with-noncogs.RData")

# Step 1: Table 1 in the text --- means and standard deviations of the CIL scores, as well as sample size, by country
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
rm(list = ls())

# Loading in the subset to be used in the main analysis
load("icils-2018-data-with-noncogs-subset.RData")

# Defining survey design object
des <- svrepdesign(data = students1, weights = ~totwgts, repweights = "srwgt[0-9]+", type = "JK1", scale = 1, mse = TRUE, combine.weights = TRUE)

# Step 2: Table 2 in the text

t4 <- svycor(~timedrop + perfdrop + nrr, design = des, sig.stats = TRUE)

t4$cors
t4$std.err
t4$t.values
t4$p.values

t5 <- t4$cors
rownames(t5) <- colnames(t5) <- c("Drop in response time", "Performance drop", "Item response rate")
kable(x = t5, format = "latex", digits = 2, booktabs = TRUE, caption = "Pairwise correlations between the proxies for non-cognitive skills; ICILS 2018, pooled sample", label = "corr") %>%
  kable_styling() %>%
  footnote(general = "All the correlations are significant at the level of $p = 0.001$.") %>%
  cat(., file = "tab-2-corrs.tex")

rm(t4, t5)

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

# A list with the models

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

# STEP 3.2: Testing the hypotheses
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
         term = factor(term, levels = c("gender", "degree", "immbgr"), 
                      labels = c("Gender", "Parents with higher education",
                                 "Immigrant background")))

tikz(file = "model-comparison.tex", width = 6, height = 3)
t6 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model
# This is not included in the manuscript, but is available upon request (see footnote 18)

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

# Calculating variance inflation factors

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
rm(list = ls()[!ls() %in% c("students1", "des", paste("mod", 0:4, sep = "."), "tidy.melded", "glance.melded")])

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

# A list with the models

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

# STEP 4.2: Testing the hypotheses
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
         term = factor(term, levels = c("gender", "high", "immbgr"), 
                       labels = c("Gender", "Parental occupation",
                                  "Immigrant background")))

tikz(file = "model-comparison-occ.tex", width = 6, height = 3)
t6 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model
# This is not included in the manuscript, but is available upon request (see footnote 18)

mod.01 <- map2(.x = mod.10, .y = mod.11, .f = anova, method = "Wald", test = "F")
mod.02 <- map2(.x = mod.10, .y = mod.12, .f = anova, method = "Wald", test = "F")
mod.03 <- map2(.x = mod.10, .y = mod.13, .f = anova, method = "Wald", test = "F")
mod.04 <- map2(.x = mod.10, .y = mod.14, .f = anova, method = "Wald", test = "F")

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
        caption = "Results of the $F$-tests for models with parental occupation as the indicator of socioeconomic status", 
        label = "tab:occ-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "occ-ftests.tex")

# Calculating variance inflation factors

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
rm(list = ls()[!ls() %in% c("students1", "des", "tidy.melded", "glance.melded")])

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

# A list with the models

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
       booktabs = TRUE, dcolumn = TRUE, file = "cil-sfe-occ.tex", use.packages = FALSE, 
       caption = "Results from the fixed-effects linear models for the CIL test scores", 
       caption.above = TRUE, label = "tab:cil-hisei")

# STEP 5.2: Testing the hypotheses
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
         term = factor(term, levels = c("gender", "hisei", "immbgr"), 
                       labels = c("Gender", "HISEI",
                                  "Immigrant background")))

tikz(file = "model-comparison-hisei.tex", width = 6, height = 3)
t6 %>%
  ggplot(mapping = aes(x = Comparison, y = m)) + 
  geom_ref_line(h = 0, colour = "#bdbdbd", size = 0.5) +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.1) + 
  geom_point() + 
  facet_wrap(~term, scales = "free_y") + 
  theme_bw() + 
  labs(x = "Model comparison", y = "Estimated difference between coefficients")
dev.off()

# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model
# This is not included in the manuscript, but is available upon request (see footnote 18)

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
        booktabs = TRUE,
        caption = "Results of the $F$-tests for models with HISEI as the indicator of socioeconomic status", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "hisei-ftests.tex")

# Calculating variance inflation factors

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
rm(list = ls()[!ls() %in% c("students1", "des", "tidy.melded", "glance.melded")])

# Models with NISB as a proxy for socioeconomic background
# Baseline model: no proxies for non-cognitive skills

mod.10 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.11 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.12 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.13 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.14 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_nisb_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# A list with the models

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
                                "Computer experience: [5, 7)", "Computer experience: 7+", "NISB",
                                "Time drop",  "Performance drop", "Nonresponse"))

texreg(l = mod.sum.1$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", "NISB",
                             "Time drop",  "Performance drop", "Nonresponse"),
       booktabs = T, dcolumn = T, file = "cil-dfe-nisb.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-nisb")


# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model

mod.101 <- map2(.x = mod.10, .y = mod.11, .f = anova, method = "Wald", test = "F")
mod.102 <- map2(.x = mod.10, .y = mod.12, .f = anova, method = "Wald", test = "F")
mod.103 <- map2(.x = mod.10, .y = mod.13, .f = anova, method = "Wald", test = "F")
mod.104 <- map2(.x = mod.10, .y = mod.14, .f = anova, method = "Wald", test = "F")

mod.101.f <- mod.101 %>%
  map_dbl(~.$Ftest)
mod.102.f <- mod.102 %>%
  map_dbl(~.$Ftest)
mod.103.f <- mod.103 %>%
  map_dbl(~.$Ftest)
mod.104.f <- mod.104 %>%
  map_dbl(~.$Ftest)

ftest.1 <- rbind(
  micombine.F(Fvalues = mod.101.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.102.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.103.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.104.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.1 <- ftest.1 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.1 %>%
  kable(format = "latex", 
        booktabs = T,
        caption = "Results of the $F$-tests", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "nisb-ftests.tex")

# Calculating variance inflation factors

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

write_delim(x = mod.10.vif, file = "icils-2018-nisb-vif.csv", delim = ";")

# Models with parental education as a proxy for socioeconomic background
# Baseline model: no proxies for non-cognitive skills

mod.20 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.21 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.22 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.23 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.24 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# A list with the models

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
                                "Parental education", "Time drop",  "Performance drop",
                                "Nonresponse"))

texreg(l = mod.sum.2$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "Parental education", "Time drop",  "Performance drop", 
                             "Nonresponse"),
       booktabs = T, dcolumn = T, file = "cil-dfe-degree.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-degree")

# Testing the hypotheses
# Model 1 vs Model 2
# PV1

m201.1 <- withReplicates(design = des,
                       theta = function(w = totwgts, data = students1) {
                         m1 <- lm(pv1cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, data = data, weights = w)
                         m2 <- lm(pv1cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, data = data, weights = w)
                         return(coef(m1)[2:10] - coef(m2)[2:10])
                       })

# PV2
m201.2 <- withReplicates(design = des,
                         theta = function(w = totwgts, data = students1) {
                           m1 <- lm(pv2cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, data = data, weights = w)
                           m2 <- lm(pv2cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, data = data, weights = w)
                           return(coef(m1)[2:10] - coef(m2)[2:10])
                         })
# PV3
m201.3 <- withReplicates(design = des,
                         theta = function(w = totwgts, data = students1) {
                           m1 <- lm(pv3cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, data = data, weights = w)
                           m2 <- lm(pv3cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, data = data, weights = w)
                           return(coef(m1)[2:10] - coef(m2)[2:10])
                         })

# PV4
m201.4 <- withReplicates(design = des,
                         theta = function(w = totwgts, data = students1) {
                           m1 <- lm(pv4cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, data = data, weights = w)
                           m2 <- lm(pv4cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, data = data, weights = w)
                           return(coef(m1)[2:10] - coef(m2)[2:10])
                         })

# PV5
m201.5 <- withReplicates(design = des,
                         theta = function(w = totwgts, data = students1) {
                           m1 <- lm(pv5cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe, data = data, weights = w)
                           m2 <- lm(pv5cil_sfe ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe, data = data, weights = w)
                           return(coef(m1)[2:10] - coef(m2)[2:10])
                         })

mod.201 <- list(m201.1, m201.2, m201.3, m201.4, m201.5) %>%
  map(as.data.frame) %>%
  bind_rows(.id = "pv") %>%
  mutate(term = rownames(.),
         term = str_remove_all(string = term, pattern = "\\.+|[0-9]+|s\\_|\\_+|sfe")) %>%
  filter(term %in% c("gender", "degree", "immbgr"))

mod.201 %>%
  as_tibble() %>%
  group_by(term) %>%
  summarise(d = mean(theta),
            s = sqrt(mean(SE^2) + 1.2 * sum((theta - mean(theta))^2))/4)
  


# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model

mod.201 <- map2(.x = mod.20, .y = mod.21, .f = anova, method = "Wald", test = "F")
mod.202 <- map2(.x = mod.20, .y = mod.22, .f = anova, method = "Wald", test = "F")
mod.203 <- map2(.x = mod.20, .y = mod.23, .f = anova, method = "Wald", test = "F")
mod.204 <- map2(.x = mod.20, .y = mod.24, .f = anova, method = "Wald", test = "F")

mod.201.f <- mod.201 %>%
  map_dbl(~.$Ftest)
mod.202.f <- mod.202 %>%
  map_dbl(~.$Ftest)
mod.203.f <- mod.203 %>%
  map_dbl(~.$Ftest)
mod.204.f <- mod.204 %>%
  map_dbl(~.$Ftest)

ftest.2 <- rbind(
  micombine.F(Fvalues = mod.201.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.202.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.203.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.204.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.2 <- ftest.2 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.2 %>%
  kable(format = "latex", 
        booktabs = T,
        caption = "Results of the $F$-tests", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "degree-ftests.tex")

# Plotting the coefficients for gender, SES, and migrant status

fig_1 <- list(
  summary(MIcombine(mod.20)), 
  summary(MIcombine(mod.21)), 
  summary(MIcombine(mod.22)), 
  summary(MIcombine(mod.23)), 
  summary(MIcombine(mod.24))) %>%
  bind_rows(.id = "Model")

fig_1 <- tibble(term = rownames(fig_1), as_tibble(fig_1)) %>%
  filter(str_detect(string = term, pattern = "gender|s_degree|s_imm")) %>%
  mutate(term = str_remove(string = term, pattern = "\\_sfe[.0-9]+"),
         term = factor(term, levels = c("gender", "s_degree", "s_immbgr"), 
                       labels = c("Gender", "Parental education", "Migrant status")))

tikz(file = "icils-2018-hyp-test-edu.tex", width = 6, height = 3, standAlone = F)
fig_1 %>%
  ggplot(mapping = aes(x = Model, y = results)) + 
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = `(lower`, ymax = `upper)`), width = 0.1) + 
  facet_wrap(~term) + 
  labs(y = "Estimate") + 
  theme_bw() + 
  theme(axis.title = element_text(hjust = 1))
dev.off()

# Calculating VIF

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

write_delim(x = mod.20.vif, file = "vif-values-cil-degree.csv", delim = ';')

# Models with parental occupation as a proxy for socioeconomic background
# Baseline model: no proxies for non-cognitive skills

mod.30 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe, design = des)))

# Extending the baseline model: adding time drop

mod.31 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe, design = des)))

# Extending the baseline model: adding performance drop

mod.32 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + perfdrop_sfe, design = des)))

# Extending the baseline model: adding response rate

mod.33 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + nrr_sfe, design = des)))

# Extending the baseline model: adding all three proxies

mod.34 <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + high_sfe + timedrop_sfe + perfdrop_sfe + nrr_sfe, design = des)))

# A list with the models

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
                                "Parental occupation", "Time drop",  "Performance drop",
                                "Nonresponse"))

texreg(l = mod.sum.3$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+", 
                             "Parental occupation", "Time drop",  "Performance drop", 
                             "Nonresponse"),
       booktabs = T, dcolumn = T, file = "cil-dfe-occ.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-occ")


# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model

mod.301 <- map2(.x = mod.30, .y = mod.31, .f = anova, method = "Wald", test = "F")
mod.302 <- map2(.x = mod.30, .y = mod.32, .f = anova, method = "Wald", test = "F")
mod.303 <- map2(.x = mod.30, .y = mod.33, .f = anova, method = "Wald", test = "F")
mod.304 <- map2(.x = mod.30, .y = mod.34, .f = anova, method = "Wald", test = "F")

mod.301.f <- mod.301 %>%
  map_dbl(~.$Ftest)
mod.302.f <- mod.302 %>%
  map_dbl(~.$Ftest)
mod.303.f <- mod.303 %>%
  map_dbl(~.$Ftest)
mod.304.f <- mod.304 %>%
  map_dbl(~.$Ftest)

ftest.3 <- rbind(
  micombine.F(Fvalues = mod.301.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.302.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.303.f, df1 = 1)[1:4],
  micombine.F(Fvalues = mod.304.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.3 <- ftest.3 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1", "Model 5 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.3 %>%
  kable(format = "latex", 
        booktabs = T,
        caption = "Results of the $F$-tests", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "occ-ftests.tex")

# Calculating VIF

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

# Plotting the coefficients for gender, SES, and migrant status

fig_2 <- list(
  summary(MIcombine(mod.30)), 
  summary(MIcombine(mod.31)), 
  summary(MIcombine(mod.32)), 
  summary(MIcombine(mod.33)), 
  summary(MIcombine(mod.34))) %>%
  bind_rows(.id = "Model")

fig_2 <- tibble(term = rownames(fig_2), as_tibble(fig_2)) %>%
  filter(str_detect(string = term, pattern = "gender|high|s_imm")) %>%
  mutate(term = str_remove(string = term, pattern = "\\_sfe[.0-9]+"),
         term = factor(term, levels = c("gender", "high", "s_immbgr"), 
                       labels = c("Gender", "Parental occupation", "Migrant status")))

tikz(file = "icils-2018-hyp-test-occ.tex", width = 6, height = 3, standAlone = F)
fig_2 %>%
  ggplot(mapping = aes(x = Model, y = results)) + 
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = `(lower`, ymax = `upper)`), width = 0.1) + 
  facet_wrap(~term) + 
  labs(y = "Estimate") + 
  theme_bw() + 
  theme(axis.title = element_text(hjust = 1))
dev.off()

ggsave(filename = "icils-2018-hyp-test-occ.jpeg", dpi = 540, width = 6, height = 3)

########## MODELS WITH INTERACTIONS

#######   EDUCATION AS A MEASURE OF SES

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

# Calculating VIF for the models with interactions

mod.1A %>%
  map(vif)

mod.1B %>%
  map(vif)

mod.1C %>%
  map(vif)

mod.sum.4 <- tibble(
  models = list(mod.21, mod.1A, mod.22, mod.1B),
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
       file = "cil-deg-interactions.tex", caption = "Results from the fixed-effects linear models for the CIL test scores with interaction terms between the proxies for non-cognitive skills and gender, parental education, and immigrant status", label = "tab:cil-inter", caption.above = TRUE, booktabs = T, dcolumn = T, use.packages = F)


mod.2A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + timegen_sfe, design = des)))

mod.2B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + timeedu_sfe, design = des)))

mod.2C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + timedrop_sfe + timemig_sfe, design = des)))

mod.3A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfgen_sfe, design = des)))

mod.3B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfedu_sfe, design = des)))

mod.3C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfmig_sfe, design = des)))

mod.4A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respgen_sfe, design = des)))

mod.4B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respedu_sfe, design = des)))

mod.4C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + s_comphome_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respmig_sfe, design = des)))

mod.sum.5 <- tibble(
  models = list(mod.21, mod.2A, mod.2B, mod.2C),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.5 <- mod.sum.5 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.5$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Time drop", "Time drop X gender",
                                "Time drop X education", "Time drop X migrant"))

mod.sum.6 <- tibble(
  models = list(mod.22, mod.3A, mod.3B, mod.3C),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.6 <- mod.sum.6 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.6$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Performance drop", 
                                "Performance drop X gender",
                                "Performance drop X education", "Performance drop X migrant"))

mod.sum.7 <- tibble(
  models = list(mod.23, mod.4A, mod.4B, mod.4C),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.7 <- mod.sum.7 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$", "Adj. $R^2$"),
    gof = c(.y$nobs, .y$r.squared, .y$adj.rsq),
    gof.decimal = c(F, T, T)
  )))

screenreg(l = mod.sum.7$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Response rate", 
                                "Response X gender",
                                "Response X education", "Response X migrant"))

mod.4B %>%
  map(vif)

# Running a series of F-tests to see if adding the proxies increases the explanatory power relative to the baseline model

mod.401 <- map2(.x = mod.24, .y = mod.1A, .f = anova, method = "Wald", test = "F")
mod.402 <- map2(.x = mod.24, .y = mod.1B, .f = anova, method = "Wald", test = "F")
mod.403 <- map2(.x = mod.24, .y = mod.1C, .f = anova, method = "Wald", test = "F")

mod.401.f <- mod.401 %>%
  map_dbl(~.$Ftest)
mod.402.f <- mod.402 %>%
  map_dbl(~.$Ftest)
mod.403.f <- mod.403 %>%
  map_dbl(~.$Ftest)

ftest.4 <- rbind(
  micombine.F(Fvalues = mod.401.f, df1 = 3)[1:4],
  micombine.F(Fvalues = mod.402.f, df1 = 3)[1:4],
  micombine.F(Fvalues = mod.403.f, df1 = 3)[1:4]
) %>%
  as_tibble()

ftest.4 <- ftest.4 %>%
  mutate(Comparison = c("Model 2 vs Model 1", "Model 3 vs Model 1", "Model 4 vs Model 1")) %>%
  select(5, 1, 3, 4, 2)

ftest.3 %>%
  kable(format = "latex", 
        booktabs = T,
        caption = "Results of the $F$-tests", 
        label = "tab:hisei-ftest",
        digits = c(NA, 1, 0, 1, 3), 
        col.names = c("Comparison", "Test statistic", "df1", "df2", "$p$ value")) %>%
  kable_styling() %>%
  cat(., file = "occ-ftests.tex")

# Extending the baseline model: adding time drop interacted with gender

mod.2A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfgen_sfe, design = des)))

mod.2B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfedu_sfe, design = des)))

mod.2C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfmig_sfe, design = des)))

mod.2D <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + perfdrop_sfe + perfgen_sfe + perfedu_sfe + perfmig_sfe, design = des)))

mod.sum.5 <- tibble(
  models = list(mod.22, mod.2A, mod.2B, mod.2C, mod.2D),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.5 <- mod.sum.5 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$"),
    gof = c(.y$nobs, .y$r.squared),
    gof.decimal = c(F, T)
  )))

screenreg(l = mod.sum.5$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home: 1",
                                "Computers at home: 2", "Computer at home: 3 or more", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Performance drop",  "Performance drop X gender",
                                "Performance drop X education", "Performance drop X migrant"))

texreg(l = mod.sum.5$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home: 1",
                             "Computers at home: 2", "Computer at home: 3 or more", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+",
                             "Parental occupation", "Performance drop",  "Performance drop $\\times$ gender",
                             "Performance drop $\\times$ education", "Performance drop $\\times$ migrant"),
       booktabs = T, dcolumn = T, file = "cil-dfe-edu-perf.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-edu-perf")

# Extending the baseline model: adding time drop interacted with gender

mod.3A <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respgen_sfe, design = des)))

mod.3B <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respedu_sfe, design = des)))

mod.3C <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respmig_sfe, design = des)))

mod.3D <- withPV(
  mapping = list(cil ~ pv1cil_sfe + pv2cil_sfe + pv3cil_sfe + pv4cil_sfe + pv5cil_sfe),
  data = des,
  action = quote(svyglm(cil ~ gender_sfe + s_age_sfe + s_immbgr_sfe + is2g15aa_2_sfe + is2g15aa_3_sfe + is2g15aa_4_sfe + s_excomp_1_sfe + s_excomp_2_sfe + s_excomp_3_sfe + s_excomp_4_sfe + s_degree_sfe + nrr_sfe + respgen_sfe + respedu_sfe + respmig_sfe, design = des)))

mod.sum.6 <- tibble(
  models = list(mod.23, mod.3A, mod.3B, mod.3C, mod.3D),
  coefs = map(models, tidy.melded),
  gofs = map(models, glance.melded))

mod.sum.6 <- mod.sum.6 %>%
  mutate(regtabs = map2(.x = coefs, .y = gofs, .f = ~createTexreg(
    coef.names = as.character(.x$term),
    coef = .x$estimate,
    se = .x$std.error,
    pvalues = .x$p.value,
    gof.names = c("$N$", "$R^2$"),
    gof = c(.y$nobs, .y$r.squared),
    gof.decimal = c(F, T)
  )))

screenreg(l = mod.sum.6$regtabs, omit.coef = "(Intercept)",
          custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home: 1",
                                "Computers at home: 2", "Computer at home: 3 or more", 
                                "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                                "Computer experience: [5, 7)", "Computer experience: 7+",
                                "Parental education", "Response rate",  "Response rate X gender",
                                "Response rate X education", "Response rate X migrant"))

texreg(l = mod.sum.6$regtabs, omit.coef = "(Intercept)",
       custom.coef.names = c("Gender", "Age", "Migrant", "Computers at home: 1",
                             "Computers at home: 2", "Computer at home: 3 or more", 
                             "Computer experience: [1, 3)", "Computer experience: [3, 5)",
                             "Computer experience: [5, 7)", "Computer experience: 7+",
                             "Parental occupation", "Response rate",  "Response rate $\\times$ gender",
                             "Response rate $\\times$ education", "Response rate $\\times$ migrant"),
       booktabs = T, dcolumn = T, file = "cil-dfe-edu-nrr.tex", use.packages = F, caption = "Results from the fixed-effects linear models for the CIL test scores", caption.above = T, label = "tab:cil-edu-nrr")

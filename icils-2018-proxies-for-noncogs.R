library(tidyverse)
library(haven)
library(lme4)
library(broom)
library(texreg)
library(fastDummies)

####################################################################################################
############################## PART 1: LOADING IN THE DATA #########################################
####################################################################################################

# Data from ICILS 2018 are distributed in the form of separate files for each participating country and respondent type. With 14 participating countries and 3 respondent groups (i.e., students, teachers, headmasters), there is a total of 42 distinct files. The first step in the analysis consists in selecting the files containing data on students and binding them together in a single tibble.

# Each individual dataset will be first saved as an element of a list. To that end, we first create a list of length 14, the number of countries participating in ICILS 2018. The elements of the list will be named, with their names corresponding to the names of the files in which the data are stored. The data come in the SPSS format, so the function haven::read_sav() will be used to make the import. Note also that the option `user_na` of that function is set to `TRUE`.

# Assuming that all the ICILS 2018 files are in the working directory, we create a character vector with names of all the files.
files <- list.files()

# An empty list to which the data will be saved
students <- vector(mode = "list", length = 14)
names(students) <- files[str_detect(string = files, pattern = "BSG")]

# Reading in the students data files for each participating country
for (i in names(students)) {
  students[[i]] <- read_sav(i, user_na = TRUE)
}

# Binding the elements of the list together to arrive at a single tibble
students <- students %>%
  bind_rows() %>%
  rename_with(.fn = tolower)

####################################################################################################
################ PART 2: ITEM NUMBER AND THE PROBABILITY OF CORRECT ANSWER #########################
####################################################################################################

# Extracting the variables representing responses to the test items and turning the data to a long format
items <- students %>%
  select(cntry, idcntry, idschool, idstud, assessment, matches("c[12][bhsgraf][0-9]+[a-su-z]+")) %>%
  pivot_longer(cols = matches("c[12][bhsgraf][0-9]+[a-z]+"), names_to = "item", 
               values_to = "response")

# Excluding items that are "missing by design"
items <- items %>%
  filter(!(response %in% c(8, 98)))

# Rearranging rows so that their ordering corresponds to the order of items on the test
items <- items %>%
  mutate(assessment = word(string = assessment, start = -1, end = -1),
         module = toupper(str_sub(string = item, start = 1, end = 3)),
         `1` = str_sub(string = assessment, start = 2, end = 2),
         `2` = str_sub(string = assessment, start = 3, end = 3)) 

items <- items %>%
  mutate_at(.vars = vars("1", "2"), 
            .funs = ~factor(., levels = 1:5, labels = c("C1B", "C1H", "C1S", "C2G", "C2R")))

items <- items %>%
  pivot_longer(cols = `1`:`2`, names_to = "sequence", values_to = "block") %>%
  filter(module == block) %>%
  select(-block)

items <- items %>%
  arrange(idcntry, idschool, idstud, sequence)

# Adding a variable indicating item number. Rescaling item number so that it is bounded between 0 and 1
items <- items %>%
  group_by(cntry, idcntry, idschool, idstud) %>%
  mutate(item_no = 1:n(),
         item_no_r = 1/(n() - 1) * item_no - 1/(n() - 1)) %>%
  ungroup()

# There are two types of codes for missing responses. Responses to questions that were presented to a student, but were not answered, are coded as 9. Responses to questions that were not reached by the student are coded as 7. Following Borghans and Schils (2018), I recode the latter as NA and the former as wrong answers (i.e., 0).

items <- items %>%
  mutate(response = replace(response, (item == "c1s07zm") & (response %in% 1:22), 1)) %>%
  mutate(response = replace(response, (item == "c2r03zm") & (response %in% 1:4), 1)) %>%
  mutate(response = replace(response, response %in% c(9, 99), 0)) %>%
  mutate(response = replace(response, response %in% c(7, 97), NA))

# Responses to items C1B04ZM, C1S05ZM, C1S06ZMA, C1S06ZMB, C1S06ZMC, C2G07MA, C2G07MB, C2G07MC, C2R01ZM, and C2R09ZM are removed because I cannot identify a distinction between correct and incorrect responses in these items easily.

items <- items %>%
  filter(!(item %in% c("c1b04zm", "c1s05zm", "c1s06zma", "c1s06zmb", "c1s06zmc", "c2g07ma", "c2g07mb", "c2g07mc", "c2r01zm", "c2r09zm")))

# For all other items, responses are coded 0 for incorrect and 1 for correct. Responses with partial credit are recoded as 0.5

items <- items %>%
  group_by(item) %>%
  mutate(resp = response/(max(response, na.rm = TRUE))) %>%
  mutate(resp = replace(resp, (resp > 0) & (resp < 1), 0.5)) %>%
  ungroup()

# Creating a new student identifier

items <- items %>%
  mutate(respid = paste(cntry, idstud, sep = "-"))

# Modeling
# Baseline model with crossed random effects of students and items

m1 <- lmer(resp ~ item_no_r + (1|respid) + (1|item), data = items, control = lmerControl(optimizer = "Nelder_Mead"))

# Let's allow the slope of item number to vary across respondents
m2 <- update(m1, .~. - (1|respid) + (item_no_r|respid))

screenreg(l = list(m1, m2), digits = 3)

## Model comparison
m1_vs_m2 <- anova(m1, m2)

m1_vs_m2 %>%
  tidy()

# Extracting random effects from the last model
m2_ranef <- ranef(m2)

# Creating a tibble with student ID and the student-specific slope from model m2. I call the latter variable perfdrop as in "a drop in performance in the course of the test". The tibble is called "noncogs" as in "proxies for noncognitive skills"

noncogs <- tibble(
  respid = rownames(m2_ranef$respid),
  perfdrop = m2_ranef$respid[, 2]
) %>%
  mutate(perfdrop = fixef(m2)[2] + perfdrop)

####################################################################################################
################ PART 3: ITEM NUMBER AND THE TIME TAKEN TO COMPLETE THE TASK #######################
####################################################################################################

# I will now look at the data about the time taken to complete the tasks as a still another measure of effort exerted on the test. I will repeat the steps taken above to calculate the first measure of test effort

timing <- students %>%
  select(cntry, idcntry, idschool, idstud, assessment, matches("c[12][bhsgr][0-9]+t")) %>%
  pivot_longer(cols = matches("c[12][bhsgr][0-9]+t"), names_to = "item", values_to = "time")

# Removing items "missing by design"

timing <- timing %>%
  filter(time != 9998)

# Recoding 9999 into missing

timing <- timing %>% 
  mutate(time = replace(time, time == 9999, NA))

# Rearranging the data so that the ordering of items corresponds to the actual ordering of modules in the test

timing <- timing %>%
  mutate(assessment = word(string = assessment, start = -1, end = -1),
         module = toupper(str_sub(string = item, start = 1, end = 3)),
         `1` = str_sub(string = assessment, start = 2, end = 2),
         `2` = str_sub(string = assessment, start = 3, end = 3))

timing <- timing %>%
  mutate_at(.vars = vars("1", "2"), 
            .funs = ~factor(., levels = 1:5, labels = c("C1B", "C1H", "C1S", "C2G", "C2R")))

timing <- timing %>%
  pivot_longer(cols = `1`:`2`, names_to = "sequence", values_to = "block") %>%
  filter(module == block) %>%
  arrange(idcntry, idschool, idstud, sequence) %>%
  select(-block)

# Adding the variable representing item number and item number rescaled (see above)

timing <- timing %>%
  group_by(cntry, idcntry, idschool, idstud) %>%
  mutate(item_no = 1:n(),
         item_no_r = 1/(n() - 1) * item_no - 1/(n() - 1))

# Defining a new respondent ID, just like before.

timing <- timing %>%
  mutate(respid = paste(cntry, idstud, sep = "-"))

# # Modeling
# Baseline model with crossed random effects of students and items

m3 <- lmer(time ~ item_no_r + (1|respid) + (1|item), data = timing, control = lmerControl(optimizer = "Nelder_Mead"))

# A model with random slopes
m4 <- lmer(time ~ item_no_r + (item_no_r|respid) + (1|item), data = timing, control = lmerControl(optimizer = "Nelder_Mead"))

# I get a warning that the model is singular (although isSingular(m4) is FALSE). Indeed, the correlation between random intercepts and random slopes associated with respondents is 1. I therefore fit a simpler model with random slopes uncorrelated with the intercepts

m4a <- lmer(time ~ item_no_r + (0 + item_no_r|respid) + (1|item), data = timing, control = lmerControl(optimizer = "Nelder_Mead"))

screenreg(l = list(m3, m4a), digits = 3)
m3_vs_m4 <- anova(m3, m4a)

m3_vs_m4 %>%
  tidy()

# Extracting random effects from model m4a
ranef_m4a <- ranef(m4a)

# Creating a tibble with student ID and the student-specific slope from model m4a. I call the latter variable timedrop as in "drop in response times in the later portion of the test". The tibble is then merged with the tibble noncogs created earlier

noncogs <- tibble(
  respid = rownames(ranef_m4a$respid),
  timedrop = ranef_m4a$respid[, "item_no_r"]
) %>%
  left_join(noncogs, .)

####################################################################################################
################################ PART 4: ITEM NONRESPONSE ##########################################
####################################################################################################

# Creating a tibble called nrr as in "Non-Response Rate". It contains, along with identification variables, an item response rate as a measure of conscientiousness

# Student-specific non-response rate

nrr <- students %>%
  select(cntry, idschool, idstud, is2g03:is2g30) %>%
  pivot_longer(cols = starts_with("is2"), names_to = "item", values_to = "response")

# There is a total of 1,245 respondents who didn't fill in the questionnaire. Put in another way, the background questionnaire was not administered to these students. They should therefore be excluded from the analysis. 

nrr <- nrr %>%
  filter(response != 8) %>%
  group_by(cntry, idschool, idstud) %>%
  summarise(nrr = 1 - mean(response == 9),
            .groups = "drop")
  
noncogs <- noncogs %>%
  separate(col = respid, into = c("cntry", "idstud"), sep = "\\-") %>%
  mutate(idstud = parse_number(idstud)) %>%
  left_join(nrr, .)

# Merging the resulting variables with the master dataset

students <- left_join(students, noncogs)

# Tidying up

rm(list = ls()[ls() != "students"])

####################################################################################################
############################# PART 5: DATA CLEANING, RECODING, ETC. ################################
####################################################################################################

# Some necessary recoding of the variables to be used in the analysis
# Student's gender: 
# - s_sex is coded 1 for girls, 0 for boys, 8 for NAs;
# - s_age is coded in years/months
# - s_degree is coded 1 for those whose parents (at least one of them) have a university education and 0 otherwise
# - high is coded 1 for students who parents (at least one of them) works in 1-digit ISCO-08 groups 1 through 3

students <- students %>%
  mutate(gender = ifelse(s_sex %in% 0:1, s_sex, NA),
         s_immbgr = ifelse(s_immbgr %in% 0:1, s_immbgr, NA))

students <- students %>%
  mutate_at(.vars = vars(matches("s_p[1-2]isced")),
            .funs = ~replace(., . %in% 8:9, NA))

students <- students %>%
  mutate(s_degree = s_p1isced %in% 4 | s_p2isced %in% 4,
         s_degree = replace(s_degree, is.na(s_p1isced) & is.na(s_p2isced), NA))

# Desktop or laptop computers at home; experience using computers

students <- students %>%
  mutate(is2g15aa = replace(is2g15aa, is2g15aa %in% 8:9, NA),
         s_excomp = replace(s_excomp, s_excomp %in% 8:9, NA))

# Parental occupation

students <- students %>%
  mutate_at(.vars = vars(matches("s_p[1-2]isco")),
            .funs = ~replace(., . %in% 9998:9999, NA)) 

students <- students %>%
  mutate(high = s_p1isco %in% 1000:3999 | s_p2isco %in% 1000:3999,
         high = replace(high, is.na(s_p1isco) & is.na(s_p2isco), NA))

# A modified computers at home variable, recoded as binary

students <- students %>%
  mutate(s_comphome = ifelse(is2g15aa %in% 1:2, 0, 1))

students1 <- drop_na(students, gender, s_age, s_immbgr, s_excomp, is2g15aa, s_comphome, s_degree, high, s_hisei, s_nisb, timedrop, perfdrop, nrr)

# Adding the products of the proxies for non-cognitive skills and the background characteristics

students1 <- students1 %>%
  mutate(timegen = timedrop * gender,
         timeedu = timedrop * s_degree,
         timeocc = timedrop * high,
         timesei = timedrop * s_hisei,
         timeisb = timedrop * s_nisb,
         timemig = timedrop * s_immbgr,
         perfgen = perfdrop * gender,
         perfedu = perfdrop * s_degree,
         perfocc = perfdrop * high,
         perfsei = perfdrop * s_hisei,
         perfisb = perfdrop * s_nisb,
         perfmig = perfdrop * s_immbgr,
         respgen = nrr * gender,
         respedu = nrr * s_degree,
         respocc = nrr * high,
         respsei = nrr * s_hisei,
         respisb = nrr * s_nisb,
         respmig = nrr * s_immbgr)

students1 <- mutate(students1, s_age = replace(s_age, s_age > 20, NA))

### Changing the factor variables into dummy variables

students1 <- dummy_cols(.data = students1, select_columns = c("is2g15aa", "s_excomp"), remove_first_dummy = T)

# Transforming the variables of interest to have them centered at group means

students1 <- students1 %>%
  mutate_all(.funs = unclass) %>%
  group_by(cntry, idschool) %>%
  mutate_at(.vars = vars(matches("pv[1-5]cil"), gender, s_age, s_immbgr,
                         starts_with("s_excomp_"), s_degree, high, s_hisei, s_nisb, s_comphome,
                         contains("time"), contains("perf"), nrr, contains("resp")),
            .funs = list(
              sfe = ~. - weighted.mean(x = ., w = totwgts, na.rm = TRUE)
            )) %>%
  ungroup()

save(list = "students1", file = "icils-2018-data-with-noncogs-subset.RData")
save(list = "students", file = "icils-2018-data-with-noncogs.RData")

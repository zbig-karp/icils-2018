# ICILS 2018

This repository contains R scripts necessary to reproduce the results of analyses reported in the paper "Non-cognitive skills and social gaps in digital skills: A cross-national analysis". The analyses use data from the International Computer and Information Literacy Study (ICILS), carried out by International Association for the Evaluation of Educational Achievement (IEA). So far, two cycles of the survey have been completed: in 2013 and 2018. The present analysis uses data from the latter cycle. The ICILS data are **publicly available** on [IEA's website](https://www.iea.nl/studies/iea/icils). 
This repository contains R scripts necessary to reproduce the results of analyses reported in the paper "Non-cognitive skills and social gaps in digital skills: A cross-national analysis". The analyses use data from the International Computer and Information Literacy Study (ICILS), carried out by International Association for the Evaluation of Educational Achievement (IEA). So far, two cycles of the survey have been completed: in 2013 and 2018. The present analysis uses data from the latter cycle. The ICILS data are **publicly available** on [IEA&#8217;s website](https://www.iea.nl/studies/iea/icils). 

A principal focus of ICILS is on students&#8217; **digital skills**. ICILS is an example of a **large-scale assessment in education**. As many other large-scale assessments, it uses a **task-based approach** to measuring the skills: the test consists of a series of actual tasks that students are asked to perform and their scores reflect how well they can handle these tasks. More details concerning the methodology of the study can be found in its [technical report](https://www.iea.nl/publications/technical-reports/icils-2018-technical-report).

There are three main files in the repository

1. `icils-2018-proxies-for-noncogs.R` contains code used to prepare the data for the analysis
2. `tidy-melded-functions.R` defines two functions: `tidy-melded` and `glance-melded` which are used to translate the results of the analyses into readable regression tables. They are slightly modified functions developed by [Andrew Heiss](https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/). 
3. `icils-2018-data-analysis.R` contains code to carry out the actual analysis.

The order in which these files are listed above reflects successive steps of the analysis. The code in the first script produces a data frame that is used in the actual analysis, so this script has to be executed first. 
The order in which these files are listed above reflects successive steps of the analysis. The code in the first script produces a data frame that is used in the actual analysis, so this script has to be executed first.
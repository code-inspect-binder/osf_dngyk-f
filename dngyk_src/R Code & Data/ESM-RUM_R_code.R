# R code to import the needed data files and preprocess the data to keep only 
# the users that finished the study

# If you use R Studio, you can use the "document outline" to easily navigate this code.
# Ctrl + Shift + O : to open the document outline

# Clear everything first
rm(list = ls())

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Required packages                                          ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#devtools::install_github("rubenarslan/formr")
library(formr)
library(haven)      # for dta files
library(lubridate)  # to handle dates
library(dplyr)      # for data wrangling
library(tidyr)
library(psych)
library(ggplot2)
library(hms)
library(nlme)
library(GGally)
library(formattable)
library(psychometric) # calculate ICCs
library(patchwork) #to put different ggpraphs into the same figure
library(corrplot) #for correlation table
library(labelled)
library(ggpubr)
library(tibble)
library(openxlsx)
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
library(cowplot)
library(summarytools)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Study Parameters                                                ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

# Here one can specify the study parameters
# This is to make the script easily customizable

{
  # PARAMETERS OF THE STUDY
  
  param_days_duration_study     = 14
  param_surveys_by_day          = 4
  param_max_n_ESM_answers       = param_surveys_by_day*param_days_duration_study
  param_timezone                = "CET"
  param_today                   = lubridate::today(param_timezone)
  param_now_date                = lubridate::now(param_timezone)
  param_t_zero                  = "00:05"
  param_t_end                   = "23:30"
  param_timepoints              = c("09:00", "13:00", "17:00","21:00")  # write the times of the prompts
  param_days_done               = 0
  param_min_number_ESM_answers  = 20
  param_min_compliance_rate     = (param_min_number_ESM_answers/param_max_n_ESM_answers)*100 # 0.35714
  param_interval_length         = 130 # amount of time in minutes the participants had to answer the questionnaire from the first beep (2 hours + 10 minutes of time-out)
  this_year                     = 2021 # The year in which the study was conducted
  variables_names  = c("perseverance",
                       "negativity",
                       "brooding",
                       "replaying",
                       "criticism"#,
                       #"att_control",
                       #"activity" --> these two variables are not included in this paper but are part
                       # of the broader data collection
                       ) # items of interest
  
  # PARAMETERS OF THE RUN
  
  # the number of the formr run on which participants end once they finished the study
  param_run_end_position = 300 
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preprocessing                                              ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# setwd("")

# if you are starting fresh, just reload the following datasets:
df_end_endsurvey    = read.csv(file = "ESM-RUM_03_endsurvey.csv", header = TRUE, sep = ",")
df_end_users        = read.csv(file = "ESM-RUM_04_endusers.csv", header = TRUE, sep = ",")
df_end_ESM          = read.csv(file = "ESM-RUM_02_ESM.csv", header = TRUE, sep = ",")
df_end_demographics = read.csv(file = "ESM-RUM_01_demographics.csv", header = TRUE, sep = ",")
df_end_qualtricsdemographics = read.csv(file = "ESM-RUM_00_qualtricsdemographics.csv", header = TRUE, sep = ",")
describe(df_end_qualtricsdemographics) # I am using this csv (from qualtrics) because it seems the participants misunderstood/wrongly answered the "year of birth" question in the ESM demographics questionnaire.

# check the column types of the data sets
sapply(df_end_endsurvey, class)
sapply(df_end_ESM, class)
sapply(df_end_demographics, class)
sapply(df_end_users, class)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ESM Data Preparation                                            ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# /!\ By default, all the descriptive statistics for the demographics questionnaire are done 
# on all the participants, as no participants were excluded from this study

# some pre processing
df_end_qualtricsdemographics$sex <- as.factor(df_end_qualtricsdemographics$sex)
df_end_demographics$sex <- as.factor(df_end_demographics$sex) #1 = male, 2= female, 3 = other
levels(df_end_demographics$sex) <- c("male","female","other")
df_end_ESM          = df_end_ESM %>% mutate_if(is.character, list(~na_if(.,""))) 
df_end_demographics = df_end_demographics %>% mutate_if(is.character, list(~na_if(.,"")))

df_end_ESM$created  = as.POSIXct(df_end_ESM$created)
df_end_ESM$modified = as.POSIXct(df_end_ESM$modified)
df_end_ESM$ended    = as.POSIXct(df_end_ESM$ended)
df_end_ESM$expired  = as.POSIXct(df_end_ESM$expired)

df_end_demographics$created  = as.POSIXct(df_end_demographics$created)
df_end_demographics$modified = as.POSIXct(df_end_demographics$modified)
df_end_demographics$ended    = as.POSIXct(df_end_demographics$ended)
df_end_demographics$expired  = as.POSIXct(df_end_demographics$expired)


## Add time intervals to dataset ----

summary(df_end_ESM)
# /!\ by looking at the data and created times, it seems that the time recorded in the database 
#     can be behind the time in Belgium (maybe because of winter time?)
#     So if this is the case, just add one hour to the times in the dataframe

df_ESM_data_analysis = df_end_ESM
# uncomment the following if you need to change the time (e.g., when the server wasn't on winter time)
# %>% mutate(
#   created = created + hm("01:00"),
#   modified = modified + hm("01:00"),
#   ended = ended + hm("01:00"),
#   expired = as_datetime(expired) + hm("01:00") )

summary(df_ESM_data_analysis)

# add a column with "time_interval" : either 9:00;13:00;17:00;21:00
#    for this, check the $created column and depending on which interval it is in, create a new column with mutate

df_ESM_data_analysis = df_ESM_data_analysis %>% mutate(
  time_interval = case_when(
    format(created,"%H:%M") >= param_timepoints[1] & format(created,"%H:%M") < param_timepoints[2]  ~ param_timepoints[1],
    format(created,"%H:%M") >= param_timepoints[2] & format(created,"%H:%M") < param_timepoints[3]  ~ param_timepoints[2],
    format(created,"%H:%M") >= param_timepoints[3] & format(created,"%H:%M") < param_timepoints[4]  ~ param_timepoints[3],
    format(created,"%H:%M") >= param_timepoints[4] & format(created,"%H:%M") < param_t_end          ~ param_timepoints[4],
  )
)


## Add "n OBS" ----
# here we will number the observations and days (14 days, 56 observations)

# to determine obs_n you need to get the n_day
# to get n_day you need to know when the participants started
df_start_date = df_end_demographics %>% 
  dplyr::select(session, created) %>% 
  mutate(start_date_study = as_date(created)) %>% 
  dplyr::select(session, start_date_study)

df_ESM_data_analysis = df_ESM_data_analysis %>% 
  full_join(df_start_date, by = "session")

# add column for n days (this works even if a participant did not answer at all one day)
df_ESM_data_analysis = df_ESM_data_analysis %>% 
  mutate(n_day = as.numeric(difftime(as_date(created),
                                     start_date_study,
                                     units = "days"), units="days"),
         time_interval = as.factor(time_interval))

# now we can add the n_obs column
df_ESM_data_analysis = df_ESM_data_analysis %>% mutate(
  n_obs = as.numeric(case_when(
    time_interval == param_timepoints[1]    ~ (n_day - 1)*param_surveys_by_day + 1,
    time_interval == param_timepoints[2]    ~ (n_day - 1)*param_surveys_by_day + 2,
    time_interval == param_timepoints[3]    ~ (n_day - 1)*param_surveys_by_day + 3,
    time_interval == param_timepoints[4]    ~ (n_day - 1)*param_surveys_by_day + 4
  ))
)

## Check for double entries ----
# it seems that some participants were able to sometimes submit twice a survey at a time interval
# due to bugs so we have to remove the erroneous surveys (duplicates)

n_duplicates <- df_ESM_data_analysis %>% 
  arrange(session, created) %>%
  group_by(session, n_obs, time_interval) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  subset(num_dups > 1) %>% count() %>% as.numeric

print(paste0("There were ", n_duplicates, " duplicate ESM entries (two entries or more for one time interval/observation)."))

# first find the duplicates according to n_obs and time_interval
df_ESM_data_analysis = df_ESM_data_analysis %>% 
  arrange(session, created) %>%
  group_by(session, n_obs, time_interval) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  # then remove first the duplicates if one of the survey's submission hasn't been submitted
  subset(num_dups == 1 | (num_dups > 1 & !is.na(ended)) ) %>%
  # we recompute which rows are duplicate 
  group_by(session, n_obs, time_interval) %>% 
  arrange(session, created) %>%
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>%
  # and if the participant had submitted two surveys for the same time, we only kept the first submission (as it is ordered by creation date)
  subset(dup_id == 1) %>%
  dplyr::select(-num_dups, -dup_id)


df_ESM_data_analysis %>% 
  arrange(session, created) %>%
  group_by(session, n_obs, time_interval) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  subset(num_dups > 1) %>% count() %>% as.numeric

summary(df_ESM_data_analysis) 

## Check for missing measurements ----
# add empty rows where there are missing measurements/missing intervals 
# (all the other variables like "created" and the items will equal NA)
df_ESM_data_analysis = df_ESM_data_analysis %>% 
  arrange(session, n_obs) %>%
  complete(session, nesting(n_obs, time_interval, n_day))

summary(df_ESM_data_analysis) 
# session length should be equal to number of participants * max number of observations

# add day of the week  
df_ESM_data_analysis = df_ESM_data_analysis %>% mutate(day_of_week = wday(created, label = TRUE))




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Descriptive Statistics                                          ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Compliance rate of ESM questionnaires ----
## ___________________________________________

# compute the compliance rate of of each participant

df_end_ESM_compliance <- df_ESM_data_analysis %>% group_by(session) %>% dplyr::select(session, ended, expired) %>% filter(!is.na(ended)) %>% summarize(n_surveys_filled = n()) %>% mutate(completion_rate = (n_surveys_filled / param_max_n_ESM_answers)*100)

# get the participants that are above the minimal rate
df_end_ESM_included = df_end_ESM_compliance %>% filter(completion_rate >= param_min_compliance_rate)


# then get the ID of participants that have answered less than 20 measurements
df_end_ESM_compliance_below_min = df_end_ESM_compliance %>% filter(completion_rate < param_min_compliance_rate)
n_excluded_for_completion_rate  = df_end_ESM_compliance_below_min %>% filter(completion_rate < param_min_compliance_rate) %>% nrow(.)

df_end_ESM_compliance_summary <- df_end_ESM_included %>% 
  summarise(mean = mean(completion_rate),
            min = min(completion_rate),
            max = max(completion_rate),
            sd = sd(completion_rate)
  )

## OUTPUT
write.csv(df_end_ESM_compliance_summary, "outputs/output_compliance.csv", row.names = FALSE)

## result
print(paste0(n_excluded_for_completion_rate, " participants were excluded because they failed to answer less than 20 ESM surveys"))

print(paste0("The average compliance rate was ", round(df_end_ESM_compliance_summary$mean, 2), " (min = ", round(df_end_ESM_compliance_summary$min,2),", max = ", round(df_end_ESM_compliance_summary$max,2) ,", SD = ", round(df_end_ESM_compliance_summary$sd,2), ")" ))


## Average age of the sample  ----
## _________________________________

# As we verify the age of the participants in a Qualtrics questionnaire, 
# we only ask them for their year of birth in this questionnaire to minimize personal information

age <- df_end_qualtricsdemographics %>% dplyr::select(age) %>% describe()
age

## result
print(paste0("Participants have a mean age of ", round(age$mean, digits = 2), " (SD = ", round(age$sd, digits = 2), ", min = ", round(age$min, digits = 2), ", max = ", round(age$max, digits = 2), ")"))


## Sex ----
## __________

# 1=male, 2=female, 3=other
df_end_qualtricsdemographics %>% dplyr::select(sex) %>% group_by(sex) %>% count(sex)

descr_sex_percent_male <- df_end_qualtricsdemographics %>% dplyr::select(sex) %>% summarize(percentage <- mean(sex == "male")*100)
descr_sex_percent_female <- df_end_qualtricsdemographics %>% dplyr::select(sex) %>% summarize(percentage <- mean(sex == "female")*100)
descr_sex_percent_other <- df_end_qualtricsdemographics %>% dplyr::select(sex) %>% summarize(percentage <- mean(sex == "other")*100)

print(paste0("The sample was constitued of ", descr_sex_percent_female, "% female, ", descr_sex_percent_male, "% male, and ", descr_sex_percent_other,"% answerred \"other\"" ))

# table(df_end_qualtricsdemographics$sex, useNA = "ifany")
# freq(df_demographics$sex)


## Mother tongue and spoken languages ----
## _________________________________________

descr_df_mother_tongues_N   <- df_end_demographics %>% dplyr::select(mother_tongue) %>% group_by(mother_tongue) %>% count(mother_tongue)
descr_df_spoken_languages_N <- df_end_demographics %>% dplyr::select(other_languages) %>% group_by(other_languages) %>% count(other_languages)

descr_df_mother_tongues_N
descr_df_spoken_languages_N


## Years of formal education ----
## ________________________________

descr_years_education <- df_end_demographics %>% dplyr::select(years_of_studies)
describe(descr_years_education$years_of_studies)

## Occupation ----
## _________________

# Employé
# Indépendant
# Etudiant
# Au chomage
# Doctorant
# Autre

df_end_demographics$occupation <- as.factor(df_end_demographics$occupation)
levels(df_end_demographics$occupation) <- c("Employed","Self-employed","Student","Unemployed","PhD Student","Other")


descr_df_occupation_N <- df_end_demographics %>% dplyr::select(occupation) %>% group_by(occupation) %>% count(occupation)
descr_df_occupation_N

## Country of residence ----
## ___________________________

descr_df_country <- df_end_demographics %>% dplyr::select(country_living) %>% group_by(country_living) %>% count(country_living)
descr_df_country

## START and FINISH of participants recruitment ----
## ___________________________________________________

participant_first <- as_date(min(df_ESM_data_analysis$created, na.rm = TRUE))
participant_last  <- as_date(max(df_ESM_data_analysis$created, na.rm = TRUE))

length_esm_study <- as.numeric(participant_last - participant_first)

print(paste0("ESM data has been collected between ", participant_first, " and ", participant_last, "."))


fig_histogramESM <- ggplot(df_ESM_data_analysis) +
  geom_histogram(aes(x = created), bins = length_esm_study) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0)) + 
  labs(y= "ESM surveys count", x = "Date") 
fig_histogramESM

ggsave("outputs/fig_histogramESM_surveyscountovertime.pdf", plot = fig_histogramESM)


## ESM Data Inspection ----
## __________________________

# number of subjects
length(unique(df_ESM_data_analysis$session))

# how many surveys did the participants respond to in total
sum(!is.na(df_ESM_data_analysis$ended))

# average compliance rate (/!\ this number is only valid after having added the missing surveys rows /!\ )
mean(!is.na(df_ESM_data_analysis$ended))


### response time ----

df_ESM_data_analysis$response_time_minutes = (difftime(df_ESM_data_analysis$ended,df_ESM_data_analysis$created,tz=param_timezone,units="min"))

# histogram 
hist(as.numeric(df_ESM_data_analysis$response_time_minutes), 
     breaks = 100,
     col="lightgray", 
     xlab="Minutes", 
     main="Histogram of Response time")


dens <- density(as.numeric(df_ESM_data_analysis$response_time_minutes), na.rm = TRUE)
plot(dens, frame = FALSE, col = "steelblue", 
     main = "Density plot of response time (minutes)") 

# mean & median
mean(df_ESM_data_analysis$response_time_minutes, na.rm = TRUE)
sd(df_ESM_data_analysis$response_time_minutes, na.rm = TRUE)
median(df_ESM_data_analysis$response_time_minutes, na.rm = TRUE)

### outliers surveys in response time ----
# !! one outlier has been detected. Response time shouldn't be more than 130 minutes considering that they had maximum 2 hours to answer the questionnaire + 10 minutes time out
n_outliers <- df_ESM_data_analysis %>% filter(response_time_minutes > param_interval_length) 
n_outliers$response_time_minutes
n_outliers %>% count() %>% as.numeric()
# we remove the outlier(s)
df_ESM_data_analysis_noNAnoOutliers <- df_ESM_data_analysis %>% filter(response_time_minutes < param_interval_length)

# histogram 
fig_ESM_hist_RT <- ggplot(df_ESM_data_analysis_noNAnoOutliers, aes(x=response_time_minutes))+ 
  geom_histogram(bins=80) +
  labs(y= "Count", x = "Response time in minutes") 
fig_ESM_hist_RT

ggsave("outputs/fig_ESM_hist_RT.pdf", plot = fig_ESM_hist_RT)

# mean & median
seconds_to_period(mean(df_ESM_data_analysis_noNAnoOutliers$response_time_minutes, na.rm = TRUE)*60)
seconds_to_period(sd(df_ESM_data_analysis$response_time_minutes, na.rm = TRUE)*60)
seconds_to_period(median(df_ESM_data_analysis_noNAnoOutliers$response_time_minutes, na.rm = TRUE)*60)

### time to open survey ----

# time the participant took to open the survey from the first text message in seconds
df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds = as.numeric(as_hms(df_ESM_data_analysis_noNAnoOutliers$created - hm(df_ESM_data_analysis_noNAnoOutliers$time_interval)))

# to convert seconds into duration
# df_ESM_data_analysis$open_delay_seconds = seconds_to_period(df_ESM_data_analysis$open_delay_seconds)

# 
n_outliers_timetoopen <- df_ESM_data_analysis_noNAnoOutliers %>% filter(as.numeric(open_delay_seconds)/60 > param_interval_length) %>% count() %>% as.numeric()
n_outliers_timetoopen
max(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds, na.rm = TRUE)/60

# save data set without outlier (1)
df_ESM_data_analysis_noNAnoOutliers
# Save this data
write.csv(df_ESM_data_analysis_noNAnoOutliers, "data/ESM_data_analysis_preprocessed.csv", row.names = FALSE)

# histogram of the delay values
hist(as.numeric(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds)/60, 
     breaks = 150,
     col="lightgray", 
     xlab="Minutes", 
     main="Histogram of time to open survey") 
abline(v=60,col="red")
abline(v=0,col="red")

dens <- density(as.numeric(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds)/60, na.rm = TRUE)
plot(dens, frame = FALSE, col = "steelblue", 
     main = "Density plot of response time (minutes)") 
polygon(dens, col = "steelblue")
abline(v=60,col="red")
abline(v=0,col="red")

# histogram 
fig_ESM_hist_timetoopen <- ggplot(df_ESM_data_analysis_noNAnoOutliers, aes(x=open_delay_seconds/60))+ 
  geom_histogram(bins = 150) +
  labs(y= "Count", x = "Time to open surveys in minutes") + 
  geom_vline(xintercept=0) +
  geom_vline(xintercept=60)

fig_ESM_hist_timetoopen

ggsave("outputs/fig_ESM_hist_timetoopen.pdf", plot = fig_ESM_hist_timetoopen)

# median time to open the ESM survey
seconds_to_period(median(as.numeric(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds), na.rm = TRUE))
seconds_to_period(mean(as.numeric(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds), na.rm = TRUE))
seconds_to_period(sd(as.numeric(df_ESM_data_analysis_noNAnoOutliers$open_delay_seconds), na.rm = TRUE))


### response frequencies ----

# to how many beeps did each participant respond
tapply(df_ESM_data_analysis_noNAnoOutliers$ended, df_ESM_data_analysis_noNAnoOutliers$session, function(x) sum(!is.na(x)))

# histogram of response frequencies
hist(tapply(df_ESM_data_analysis_noNAnoOutliers$ended, df_ESM_data_analysis_noNAnoOutliers$session, function(x) sum(!is.na(x))), col="lightgray", xlab="Response Frequency", main="Histogram of Response Frequencies")

fig_ESM_hist_responsefreq <- df_ESM_data_analysis_noNAnoOutliers %>%
  group_by(session) %>%
  summarise(obs = sum(!is.na(ended))) %>%
    ggplot(.) + 
  geom_histogram(aes(x=obs/56*100)) +
  labs(y= "Count", x = "Frequency of response rate")

fig_ESM_hist_responsefreq

ggsave("outputs/fig_ESM_hist_responsefreq.pdf", plot = fig_ESM_hist_responsefreq)

####
# plot perseverance as a function of n_obs for each subject
gdat <- groupedData(perseverance ~ n_obs | session, data=df_ESM_data_analysis_noNAnoOutliers)
plot(gdat)

# plot perseverance as a function of day of the week
gdat <- groupedData(perseverance.wp_mc ~ n_obs | session, data=df_ESM_data_analysis_noNAnoOutliers)
plot(gdat)

# plot perseverance as a function of negativity
ggplot(df_ESM_data_analysis_noNAnoOutliers, aes(x=perseverance, y=negativity)) + 
  geom_point()+
  geom_smooth(method=lm)

# 
theme_set(theme_minimal(base_size=16))
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Spectral")
df_ESM_data_analysis_noNAnoOutliers %>% dplyr::select(variables_names) %>% 
  ggpairs(
    columns = variables_names
  )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Person level means and within person centered variables         ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# pl_mean = person level mean 
# wp_mc   = within-person mean centered variable

# add the person-level mean to the dataset for each variable

df_ESM_data_analysis_noNAnoOutliers = df_ESM_data_analysis_noNAnoOutliers %>%
  group_by(session) %>% 
  mutate(
    across(.cols = variables_names,
           list(
             pl_mean = ~ mean(.x, na.rm=TRUE),
             wp_mc = ~ .x - mean(.x, na.rm=TRUE)
           ),
           .names = "{.col}.{.fn}"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- Summary stats                                              ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Summary stats function
sumstats <- function(x) { data.frame( 
  mean = mean(x, na.rm = TRUE),
  sd = sd(x, na.rm = TRUE), 
  min = min(x, na.rm = TRUE), 
  max = max(x, na.rm = TRUE)
) }

# Summary stats of person level means
df_ESM_summary <- 
  df_ESM_data_analysis_noNAnoOutliers %>% 
  ungroup() %>%
  # TODO: adapt this code in order to automatically use the variables defined in "variable_means"
  dplyr::select(perseverance.pl_mean,negativity.pl_mean,criticism.pl_mean,brooding.pl_mean,replaying.pl_mean) %>%
  summarise(perseverance  = sumstats(perseverance.pl_mean),
            negativity   = sumstats(negativity.pl_mean),
            criticism    = sumstats(criticism.pl_mean),
            brooding     = sumstats(brooding.pl_mean),
            replaying    = sumstats(replaying.pl_mean)
            ) %>%
  pivot_longer(c(perseverance,negativity,criticism,brooding,replaying))


df_ESM_summary <- do.call(data.frame, df_ESM_summary)
df_ESM_summary <- df_ESM_summary %>% 
  rename(
    mean = value.mean,
    sd = value.sd,
    min = value.min,
    max = value.max
  )


formattable(df_ESM_summary, 
            align =c("l","c","c","c","c"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- Intraclass Correlation Coefficients (ICC)                  ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# = proportion of the total variability (between +  within person variability) due to between-person variability
# ICC = between variance(intercept)/between variance+within variance(=residual)

# This specifically is the ICC1 with an empty model, as specific by Gabriel et al. (2019) as common to use for ESM data


ICCs_results <- list()

for(i in 1:length(variables_names)){
  
  ICCs_results[[variables_names[[i]]]] <- as.numeric(ICC1.lme(eval(sym((variables_names[[i]]))), session, df_ESM_data_analysis))
  variables_names[[i]]
}

ICCs_results

df_ICCs_results = enframe(ICCs_results) %>%
  unnest(cols = c(name, value)) %>%
  rename("ICC" = value) 


# Getting a table for the intra-individual means & SDs, and ICCs for each variable

table_desc <- merge(df_ESM_summary, df_ICCs_results, by = "name")
table_desc <- mutate(table_desc, across(where(is.numeric), round, 2))

write.xlsx(table_desc, "outputs/output_ESM_items_descr_ICCs.xlsx", rowNames = FALSE)
write.csv(table_desc, "outputs/output_ESM_items_descr_ICCs.csv", row.names = FALSE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- Trait Questionnaires analysis                              ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# RRS
# - RRS brooding items = 5, 10, 13, 15, 16
# - RRS reflexion = 7, 11, 12, 20, 21
# - depressive items = 1,2,3,4,6,8,9,14,17,18,19, 22

# GAD-7
# - 7 items
# BDI-II

# remove means computed by formr
df_end_endsurvey <- subset(df_end_endsurvey, select = -c(GAD,BDI2,STAI,RRS))

df_end_endsurvey_totals <- df_end_endsurvey %>% rowwise() %>% mutate(total_GAD7 = sum(GAD_1,GAD_2,GAD_3,GAD_4,GAD_5,GAD_6,GAD_7),
                                                              total_BDI2 = sum(BDI2_1,BDI2_2,BDI2_3,BDI2_4,BDI2_5,BDI2_6,BDI2_7,BDI2_8,BDI2_9,BDI2_10,BDI2_11,BDI2_12,BDI2_13,BDI2_14,BDI2_15,BDI2_16,BDI2_17,BDI2_18,BDI2_19,BDI2_20,BDI2_21),
                                                              total_RRS_brooding = sum(RRS_5,RRS_10,RRS_13,RRS_15,RRS_16),
                                                              total_RRS_reflection = sum(RRS_7,RRS_11,RRS_12,RRS_20,RRS_21),
                                                              total_RRS_depressive = sum(RRS_1,RRS_2,RRS_3,RRS_4,RRS_6,RRS_8,RRS_9,RRS_14,RRS_17,RRS_18,RRS_19,RRS_22),
                                                              total_RRS10 = sum(RRS_5,RRS_10,RRS_13,RRS_15,RRS_16,RRS_7,RRS_11,RRS_12,RRS_20,RRS_21),
                                                              # STAI T form Y-2
                                                              total_STAI_T = sum( (5 - STAI_1), 
                                                                                  (STAI_2),
                                                                                  (5 - STAI_3),
                                                                                  (STAI_4),
                                                                                  (STAI_5),
                                                                                  (5 - STAI_6),
                                                                                  (5 - STAI_7),
                                                                                  (STAI_8) ,
                                                                                  (STAI_9)     ,
                                                                                  (5 - STAI_10) ,
                                                                                  (STAI_11) ,
                                                                                  (STAI_12)     ,
                                                                                  (5 - STAI_13)     ,
                                                                                  (5 - STAI_14)     ,
                                                                                  (STAI_15) ,
                                                                                  (5 - STAI_16) ,
                                                                                  (STAI_17) ,
                                                                                  (STAI_18) ,
                                                                                  (5 - STAI_19) ,
                                                                                  (STAI_20)))


key.list <- list(GAD7=c("GAD_1","GAD_2","GAD_3","GAD_4","GAD_5","GAD_6","GAD_7"),
                 BDI2=c("BDI2_1","BDI2_2","BDI2_3","BDI2_4","BDI2_5","BDI2_6","BDI2_7","BDI2_8","BDI2_9","BDI2_10","BDI2_11","BDI2_12","BDI2_13","BDI2_14","BDI2_15","BDI2_16","BDI2_17","BDI2_18","BDI2_19","BDI2_20","BDI2_21"),
                 RRS_brooding=c("RRS_5","RRS_10","RRS_13","RRS_15","RRS_16"),
                 RRS_reflection=c("RRS_7","RRS_11","RRS_12","RRS_20","RRS_21"), 
                 RRS_depressive= c("RRS_1","RRS_2","RRS_3","RRS_4","RRS_6","RRS_8","RRS_9","RRS_14","RRS_17","RRS_18","RRS_19","RRS_22"),
                 RRS10=c("RRS_5","RRS_10","RRS_13","RRS_15","RRS_16","RRS_7","RRS_11","RRS_12","RRS_20","RRS_21"),
                 STAI_T = c( "-STAI_1", "STAI_2", "-STAI_3", "STAI_4","STAI_5","-STAI_6",
                             "-STAI_7", "STAI_8", "STAI_9", "-STAI_10", "STAI_11", "STAI_12",
                             "-STAI_13", "-STAI_14", "STAI_15", "-STAI_16", "STAI_17",
                             "STAI_18", "-STAI_19", "STAI_20")
)

keys <- make.keys(colnames(df_end_endsurvey_totals[6:75]), keys.list = key.list)

scores <- scoreItems(keys,df_end_endsurvey_totals, totals = TRUE)  # or just use the keys.list
describe(scores$scores)
describe(df_end_endsurvey_totals[c("total_GAD7","total_BDI2","total_RRS_brooding","total_RRS_reflection","total_RRS_depressive","total_RRS10","total_STAI_T")])



df_trait <- df_end_endsurvey_totals[c("session", "total_GAD7","total_BDI2","total_RRS_brooding","total_RRS_reflection","total_RRS_depressive","total_RRS10","total_STAI_T")]


# alphas
scores_alpha <- as.data.frame(scores$alpha) %>% pivot_longer(c("GAD7",
                                                               "BDI2",
                                                               "RRS_brooding",
                                                               "RRS_reflection",
                                                               "RRS_depressive",
                                                               "RRS10",
                                                               "STAI_T"), values_to = "alpha")

# descriptive statistics
questionnaires_stats <- cbind(rownames(describe(df_end_endsurvey_totals[c("total_GAD7","total_BDI2","total_RRS_brooding","total_RRS_reflection","total_RRS_depressive","total_RRS10","total_STAI_T")])), data.frame(describe(df_end_endsurvey_totals[c("total_GAD7","total_BDI2","total_RRS_brooding","total_RRS_reflection","total_RRS_depressive","total_RRS10", "total_STAI_T")]), row.names=NULL)) %>% rename_at(1,~"questionnaire") 

# put alpha together with descriptive statistics

questionnaire_stats <- bind_cols(questionnaires_stats, scores_alpha) %>% dplyr::select(-name, -vars)

## OUTPUT
write.csv(questionnaire_stats, "outputs/output_traitquestionnaires_stats.csv", row.names = FALSE)
write.xlsx(questionnaire_stats, "outputs/output_traitquestionnaires_stats.xlsx", rowNames = FALSE)

# end of descriptive statistics





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- Correlation matrix                                         ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# combine matrix of within person level means with trait questionnaires means

# Matrix with WITHIN-PERSON MEANS of variable
mean_data_mat <- df_ESM_data_analysis_noNAnoOutliers %>%
  dplyr::select(session | ends_with(".pl_mean"))



# Changing names for figure
colnames(mean_data_mat) <- c("session","perseveration", "negativity", "brooding", "replaying", "criticism")

mean_data_mat <- merge(mean_data_mat,  subset(df_trait, select = -c(total_RRS_depressive)), by = ("session"))  
df_trait[,"total_RRS_depressive"]
colnames(mean_data_mat) <- c("session",
                             "ESM Perseveration", 
                             "ESM Negativity", 
                             "ESM Brooding", 
                             "ESM Replaying", 
                             "ESM Criticism", 
                             "GAD-7",
                             "BDI-II",
                             "Brooding",
                             "Reflection",
                             "RRS-10",
                             "STAI-T")



# Correlation table (only with complete cases - so takes out data from single parent)
means <- cor(mean_data_mat[,c(2:12)], use = "complete.obs") # Correlation matrix
res2 <- cor.mtest(mean_data_mat[,c(2:12)], conf.level = .95, use = "complete.obs") # p-values

# Correlation plot (colors on top, numbers on bottom) - JUST MEANS (aggregated within person)
corrplot(means, p.mat = res2$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "gray", 
         method = "color", tl.col = "black", tl.cex = 1, cl.offset = 0.25, cl.cex = 0.75)
corrplot(means, method="number", col = "black", add = TRUE, cl.pos = "n", tl.pos = "n", type = "lower", 
         number.cex = .60, number.digits = 2, tl.col = "black", cl.offset = 4) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Multilevel Reliability of the 5 rumination items together       ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_ESM_data_analysis2 <- read.csv(file = "data/ESM_data_analysis_preprocessed.csv", sep = ",")

# select the 5 rumination items
data_Rum <- df_ESM_data_analysis2 %>%
  dplyr::select(session, n_obs, perseverance, negativity, criticism, replaying, brooding) 
glimpse(data_Rum)

mlr <- mlr(data_Rum , grp = "session", Time = "n_obs", items = c(3:7), icc=TRUE, lmer = TRUE, lme = TRUE, plot = TRUE, na.action = "na.omit")
mlr$summary.by.time

mlr

# The data had  40  observations taken over  56  time intervals for  5 items.
# 
# Alternative estimates of reliability based upon Generalizability theory
# 
# RkF  =  0.99 Reliability of average of all ratings across all items and  times (Fixed time effects)
# R1R  =  0.31 Generalizability of a single time point across all items (Random time effects)
# RkR  =  0.96 Generalizability of average time points across all items (Random time effects)
# Rc   =  0.82 Generalizability of change (fixed time points, fixed items) 
# RkRn =  0.96 Generalizability of between person differences averaged over time (time nested within people)
# Rcn  =  0.77 Generalizability of within person variations averaged over items  (time nested within people)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- SAVE DATA FOR DATA ANALYSIS                                ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write.csv(df_ESM_data_analysis_noNAnoOutliers, "data/ESM_data_processed_for_analysis.csv")



  
  


#########################################################################################################################################################

rm(list = ls())
##### Installing Packages...
##### dplyr, tidyr, moments
#####
install.packages("dplyr")
install.packages("tidyr")
install.packages("moments")
install.packages("MASS")
install.packages("stringr")
install.packages("pROC")
install.packages("matrixStats")
install.packages("mice")
library(dplyr)
library(tidyr)
library(moments)
library(MASS)
library(stringr)
library(pROC)
library(matrixStats)
library(mice)

install.packages("forcats")
library(forcats)

#########################################################################################################################################################

##### Importing original dataset and creating subsets to gather median for ages so that
##### we can impute ages values that were inputted in a range. For example, some ages 
##### were inputted originally as a range of ages for the subjects, most probably to preserve 
##### and protect the privavcy and identities of subjects.
#####
df0 <- read.csv("/Users/justinsaunders/Downloads/ViceNews_FullOISData - Sheet1.csv", header = TRUE, sep = ",")

df0$SubjectAge <- ifelse(df0$SubjectAge == "60-69", 63.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "50-59", 53.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "40-49", 44.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "30-39", 33.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "20-29", 24.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "0-19", 18.00, df0$SubjectAge)
df0$SubjectAge <- ifelse(df0$SubjectAge == "21-23", 22.00, df0$SubjectAge)
df0$SubjectAge <- gsub("[^[:digit:]]", "", df0$SubjectAge)
df0$SubjectAge <- as.numeric(df0$SubjectAge)
pct_missing_SubjectAge <- mean(is.na(df0$SubjectAge)) * 100
cat("Percentage of missing data in Subject Race:", pct_missing_SubjectAge, "%\n")

##### Subsets to the df0 dataframe to include different age groups.
##### Create histograms and the descriptive statistics of the SubjectAge variable for the different df0_subsets.
##### The values below were ran first, before the block of code above, to gather the data relevant to performing    
##### a median imputation for the variable. The above code was then run after to correctly impute the dataframe. 
#####
df0_subset99 <- subset(df0, SubjectAge >= 4 & SubjectAge < 89)
hist(df0_subset99$SubjectAge, main = "Histogram of Subject Age (Ages < 99)", xlab = "Age")
summary(df0_subset99$SubjectAge)
kurtosis(df0_subset99$SubjectAge)
skewness(df0_subset99$SubjectAge)

df0_subset7099 <- subset(df0, SubjectAge >= 70 & SubjectAge < 100)
hist(df0_subset7099$SubjectAge, main = "Histogram of Subject Age (Ages 70-99)", xlab = "Age")
summary(df0_subset7099$SubjectAge)
kurtosis(df0_subset7099$SubjectAge)
skewness(df0_subset7099$SubjectAge)

df0_subset6069 <- subset(df0, SubjectAge >= 60 & SubjectAge < 70)
hist(df0_subset6069$SubjectAge, main = "Histogram of Subject Age (Ages 60-69)", xlab = "Age")
summary(df0_subset6069$SubjectAge)
kurtosis(df0_subset6069$SubjectAge)
skewness(df0_subset6069$SubjectAge)

df0_subset5059 <- subset(df0, SubjectAge >= 50 & SubjectAge < 60)
hist(df0_subset5059$SubjectAge, main = "Histogram of Subject Age (Ages 50-59)", xlab = "Age")
summary(df0_subset5059$SubjectAge)
kurtosis(df0_subset5059$SubjectAge)
skewness(df0_subset5059$SubjectAge)

df0_subset4049 <- subset(df0, SubjectAge >= 40 & SubjectAge < 50)
hist(df0_subset4049$SubjectAge, main = "Histogram of Subject Age (Ages 40-49)", xlab = "Age")
summary(df0_subset4049$SubjectAge)
kurtosis(df0_subset4049$SubjectAge)
skewness(df0_subset4049$SubjectAge)

df0_subset3039 <- subset(df0, SubjectAge >= 30 & SubjectAge < 40)
hist(df0_subset3039$SubjectAge, main = "Histogram of Subject Age (Ages 30-39)", xlab = "Age")
summary(df0_subset3039$SubjectAge)
kurtosis(df0_subset3039$SubjectAge)
skewness(df0_subset3039$SubjectAge)

df0_subset2029 <- subset(df0, SubjectAge >= 20 & SubjectAge < 30)
hist(df0_subset2029$SubjectAge, main = "Histogram of Subject Age (Ages 20-29)", xlab = "Age")
summary(df0_subset2029$SubjectAge)
kurtosis(df0_subset2029$SubjectAge)
skewness(df0_subset2029$SubjectAge)

df0_subset019 <- subset(df0, SubjectAge >= 0 & SubjectAge < 20)
hist(df0_subset019$SubjectAge, main = "Histogram of Subject Age (Ages 0-19)", xlab = "Age")
summary(df0_subset019$SubjectAge)
kurtosis(df0_subset019$SubjectAge)
skewness(df0_subset019$SubjectAge)

df0$SubjectAge <- ifelse(is.na(df0$SubjectAge), 28, df0$SubjectAge)
hist(df0$SubjectAge, main = "Histogram of Subject Age (Ages 0-99)", xlab = "Age")
summary(df0$SubjectAge)
kurtosis(df0$SubjectAge)
skewness(df0$SubjectAge)
pct_missing_SubjectAge_imp <- mean(is.na(df0$SubjectAge)) * 100
cat("Percentage of missing data in SubjectAge variable after Imputation:", pct_missing_SubjectAge_imp, "%\n")



#####
##### Cleaning the Fatal variable.
#####
df0$Fatal <- as.factor(df0$Fatal)
unique(df0$Fatal)
df0$Fatal <- recode_factor(df0$Fatal, " F" = "FATAL")
df0$Fatal <- recode_factor(df0$Fatal, " N" = "NON-FATAL")
df0$Fatal <- recode_factor(df0$Fatal, "F" = "FATAL")
df0$Fatal <- recode_factor(df0$Fatal, "N" = "NON-FATAL")
df0$Fatal <- recode_factor(df0$Fatal, "U" = "Unknown")
pct_missing_Fatal <- mean(is.na(df0$Fatal)) * 100
cat("Percentage of missing data in Fatal:", pct_missing_Fatal, "%\n")



#####
##### Cleaning the Subject Race variable.
#####
df0$SubjectRace[is.na(df0$SubjectRace)] <- "U"
df0$SubjectRace <- as.factor(df0$SubjectRace)
unique(df0$SubjectRace)
df0$SubjectRace <- recode_factor(df0$SubjectRace, "A" = "ASIAN")
df0$SubjectRace <- recode_factor(df0$SubjectRace, "B" = "BLACK")
df0$SubjectRace <- recode_factor(df0$SubjectRace, "L" = "LATINO")
df0$SubjectRace <- recode_factor(df0$SubjectRace, "O" = "OTHER")
df0$SubjectRace <- recode_factor(df0$SubjectRace, "U" = "Unknown")
df0$SubjectRace <- recode_factor(df0$SubjectRace, "W" = "WHITE")
pct_missing_SubjectRace <- mean(is.na(df0$SubjectRace)) * 100
cat("Percentage of missing data in Subject Race:", pct_missing_SubjectRace, "%\n")



#####
##### Cleaning the Subject Armed variable.
#####
df0$SubjectArmed[is.na(df0$SubjectArmed)] <- "U"
df0$SubjectArmed <- as.factor(df0$SubjectArmed)
unique(df0$SubjectArmed)
df0$SubjectArmed <- recode_factor(df0$SubjectArmed, "Y" = "ARMED")
df0$SubjectArmed <- recode_factor(df0$SubjectArmed, "Y " = "ARMED")
df0$SubjectArmed <- recode_factor(df0$SubjectArmed, "N" = "UNARMED")
df0$SubjectArmed <- recode_factor(df0$SubjectArmed, "U" = "Unknown")
pct_missing_SubjectArmed <- mean(is.na(df0$SubjectArmed)) * 100
cat("Percentage of missing data in SubjectArmed variable:", pct_missing_SubjectArmed, "%\n")



#####
##### Cleaning the Subject Gender variable.
#####
df0$SubjectGender <- as.factor(df0$SubjectGender)
unique(df0$SubjectGender)
df0$SubjectGender <- recode_factor(df0$SubjectGender, "F" = "FEMALE")
df0$SubjectGender <- recode_factor(df0$SubjectGender, "M" = "MALE")
df0$SubjectGender <- recode_factor(df0$SubjectGender, "M;U" = "MALE")
df0$SubjectGender <- recode_factor(df0$SubjectGender, "U" = "Unknown")
df0$SubjectGender <- recode_factor(df0$SubjectGender, "N/A" = "Unknown")
pct_missing_SubjectGender <- mean(is.na(df0$SubjectGender)) * 100
cat("Percentage of missing data in SubjectGender variable:", pct_missing_SubjectGender, "%\n")



#####
##### Cleaning the Number of Shots variable.
##### No imputations were necessary and this is a variable we particularly do not want to alter.
##### If there was an "at least" operator in the value, the lowest value was selected for the variable. For example,
##### if the number of shots fired was greater than or equal to 5, then 5 was chosen as the value for the variable.
#####
df0$NumberOfShots <- as.factor(df0$NumberOfShots)
unique(df0$NumberOfShots)
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=4" = "4")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=8" = "8")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=1" = "1")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=2" = "2")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=12" = "12")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=3" = "3")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=20" = "20")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">1" = "1")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">4" = "4")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">/=5" = "5")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">2" = "2")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">3" = "3")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "45 total" = "45")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, ">13" = "13")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "3-5" = "4")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "4-5" = "5")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "1*" = "1")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "Multiple" = "2")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "4;14;16" = "34")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;8;6;4;6" = "29")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "0;2;0" = "2")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "13;9" = "22")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "8;2" = "10")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;5" = "10")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "3;8;8" = "19")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "10;3;2;2" = "17")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "6;5" = "11")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;5;3" = "13")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "9;5;4" = "18")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "73 total" = "73")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "45 total " = "45")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "10;3" = "13")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "4;1" = "5")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "1;4" = "5")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "4;7" = "11")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "1;3" = "4")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "3;1" = "4")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "2;1;4" = "7")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "16;1;25 ;10;16; 16" = "84")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "16;16;2;12" = "46")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "8;5" = "13")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "13;4" = "17")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "3;4;1" = "8")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "1;2" = "3")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "16;2;1;2" = "21")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "9;9;1" = "19")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "2;3" = "5")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "16;9;12" = "37")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "1;1" = "2")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;1" = "6")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;4" = "9")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "9;7;4;7; 2;7;3" = "39")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "5;8;6" = "19")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "not clear" = "")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "no information" = "Unknown")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "Unknown" = "Unknown")
df0$NumberOfShots <- recode_factor(df0$NumberOfShots, "U" = "Unknown")
df0$NumberOfShots <- gsub("[^[:digit:]]", "", df0$NumberOfShots)
df0$NumberOfShots <- as.numeric(df0$NumberOfShots)
pct_missing_NumberOfShots <- mean(is.na(df0$NumberOfShots)) * 100
cat("Percentage of missing data in NumberOfShots variable:", pct_missing_NumberOfShots, "%\n")

df0_NumberOfShots_subset <- subset(df0, NumberOfShots >= 0 & NumberOfShots <= 84)
hist(df0_NumberOfShots_subset$NumberOfShots, main = "Histogram of Number of Shots Fired by Police (Between 0-8 Shots)", xlab = "Shots Fired")
summary(df0_NumberOfShots_subset$NumberOfShots)
sd(df0_NumberOfShots_subset$NumberOfShots)
kurtosis(df0_NumberOfShots_subset$NumberOfShots)
skewness(df0_NumberOfShots_subset$NumberOfShots)

##
#   df0_NumberOfShots_subset$NumberOfShots <- as.numeric(df0_NumberOfShots_subset$NumberOfShots)
#   third_quartile <- quantile(df0_NumberOfShots_subset$NumberOfShots, probs = 0.75)
#   percentage_third_quartile <- sum(df0_NumberOfShots_subset$NumberOfShots <= third_quartile) / length(df0_NumberOfShots_subset$NumberOfShots) * 100
#   cat("Percentage of data found in middle 75% of NumberOfShots variable:", percentage_third_quartile, "%\n")
#   ##
#   df0_NumberOfShots_subset <- subset(df0, NumberOfShots >= 0 & NumberOfShots <= 8)
#   hist(df0_NumberOfShots_subset$NumberOfShots, main = "Histogram of Number of Shots Fired by Police (Between 0-8 Shots)", xlab = "Shots Fired")
#   summary(df0_NumberOfShots_subset$NumberOfShots)
#   sd(df0_NumberOfShots_subset$NumberOfShots)
#   kurtosis(df0_NumberOfShots_subset$NumberOfShots)
#   skewness(df0_NumberOfShots_subset$NumberOfShots)
##

## Create a data frame with the variables to be imputed
# df0_subset <- subset(df0, select = c("SubjectRace", "SubjectAge", "Fatal", "SubjectArmed", "SubjectGender", "NumberOfShots"))
## Impute the missing values using the mice function
# imputed_data <- mice(df0_subset, method = "pmm", seed = 1234)
## Extract the imputed data and store it in a new data frame
# imputed_df <- complete(imputed_data, 1)
#       hist(imputed_df$NumberOfShots, main = "Histogram of Number of Shots Fired by Police MICE PMM (Between 0-8 Shots)", xlab = "Shots Fired")
#       summary(imputed_df$NumberOfShots)
#       sd(imputed_df$NumberOfShots)
#       kurtosis(imputed_df$NumberOfShots)
#       skewness(imputed_df$NumberOfShots)
## Replace the original data frame with the imputed data
# df0$NumberOfShots <- imputed_df$NumberOfShots
# pct_missing_NumberOfShots_imp <- mean(is.na(df0$NumberOfShots)) * 100
# cat("Percentage of missing data in NumberOfShots variable after MICE PMM Imputation:", pct_missing_NumberOfShots_imp, "%\n")

#  Median Imputation of 3 / no MICE PMM Imputation
df0$NumberOfShots <- ifelse(is.na(df0$NumberOfShots), 1, df0$NumberOfShots)
hist(df0$NumberOfShots, main = "Histogram of Number of Shots Fired by Police (Between 0-84 Shots)", xlab = "Shots Fired")
summary(df0$NumberOfShots)
sd(df0$NumberOfShots)
kurtosis(df0$NumberOfShots)
skewness(df0$NumberOfShots)
pct_missing_NumberOfShots_imp <- mean(is.na(df0$NumberOfShots)) * 100
cat("Percentage of missing data in NumberOfShots variable after Median Imputation:", pct_missing_NumberOfShots_imp, "%\n")
#
####### 
#### Other imputation methods
#  # Create a data frame with the variables to be imputed
#    df0_Shots_subset1 <- subset(df0_NumberOfShots_subset, select = c("SubjectAge", "Fatal", "SubjectRace", "SubjectArmed", "SubjectGender", "NumberOfShots"))
#  # Impute the missing values using the mice function
#    imputed_data1 <- mice(df0_Shots_subset1, method = "pmm", seed = 1234)
#  # Extract the imputed data and store it in a new data frame
#    imputed_df1 <- complete(imputed_data1, 1)

#    hist(imputed_df1$NumberOfShots, main = "Histogram of Number of Shots Fired by Police (Between 0-84 Shots)", xlab = "Shots Fired")
#    summary(imputed_df1$NumberOfShots)
#    kurtosis(imputed_df1$NumberOfShots)
#    skewness(imputed_df1$NumberOfShots)

# Replace the original data frame with the imputed data
#  df0$NumberOfShots_imp1 <- imputed_df1$NumberOfShots
#  pct_missing_NumberOfShots_imp1 <- mean(is.na(df0$NumberOfShots)) * 100
#  cat("Percentage of missing data in NumberOfShots variable after Imp 1:", pct_missing_NumberOfShots_imp1, "%\n")


#  mice_object1 <- mice(df0, m = 7) # m is the number of imputed datasets
#  completed_mice1 <- complete(mice_object1, action = "long")

#  hist(completed_mice1$NumberOfShots, main = "Histogram of Number of Shots Fired by Police (Between 0-84 Shots)", xlab = "Shots Fired")
#  summary(completed_mice1$NumberOfShots)
#  kurtosis(completed_mice1$NumberOfShots)
#  skewness(completed_mice1$NumberOfShots)


#  # Generate density plots for the observed and imputed values
#  densityplot(completed_mice1)

#  # Generate strip plots for the observed and imputed values
#  stripplot(completed_mice1)

#  trimming_percentage_NumberOfShots <- .001
#  trim_count_NumberOfShots     <- round(nrow(df0_NumberOfShots_subset) * trimming_percentage_NumberOfShots / 2)
#  sorted_NumberOfShots         <- sort(df0_NumberOfShots_subset$NumberOfShots)
#  trimmed_NumberOfShots        <- sorted_NumberOfShots[-c(1:trim_count_NumberOfShots, (length(sorted_NumberOfShots) -
#                                                      trim_count_NumberOfShots+1):length(sorted_NumberOfShots))]
#  trimmed_median_NumberOfShots <- weightedMedian(trimmed_NumberOfShots)
#  cat("Trimmed Median of NumberOfShots Variable:", trimmed_median_NumberOfShots)



#######



##### 
##### Cleaning the Number of Officers variable. 
##### Again, no imputations were necessary and again, the lowest "at least value" was selected
##### for the the value of the variable, if applicable.
#####
df0$NumberOfOfficers <- as.factor(df0$NumberOfOfficers)
unique(df0$NumberOfOfficers)
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, "0" = "1")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">1" = "2")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">2" = "3")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">3" = "4")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">5" = "6")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">6" = "7")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, ">7" = "8")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, "2 or More" = "2")
df0$NumberOfOfficers <- recode_factor(df0$NumberOfOfficers, "U" = "Unknown")
df0$NumberOfOfficers <- gsub("[^[:digit:]]", "", df0$NumberOfOfficers)
df0$NumberOfOfficers <- as.numeric(df0$NumberOfOfficers)
pct_missing_NumberOfOfficers <- mean(is.na(df0$NumberOfOfficers)) * 100
cat("Percentage of missing data in NumberOfOfficers variable:", pct_missing_NumberOfOfficers, "%\n")

hist(df0$NumberOfOfficers, main = "Histogram of Number of Police Officers Active at Shooting", xlab = "Number of Officers Present")
summary(df0$NumberOfOfficers)
sd(df0$NumberOfOfficers)
kurtosis(df0$NumberOfOfficers)
skewness(df0$NumberOfOfficers)


#  trimming_percentage_NumberOfOfficers <- .
#  sorted_NumberOfOfficers         <- sort(df0$NumberOfOfficers)
#  trim_count_NumberOfOfficers     <- round(nrow(df0) * trimming_percentage_NumberOfOfficers / 2)
#  trimmed_NumberOfOfficers        <- sorted_NumberOfOfficers[-c(1:trim_count_NumberOfOfficers, (length(sorted_NumberOfOfficers) -
#                                                            trim_count_NumberOfOfficers+1):length(sorted_NumberOfOfficers))]
#  trimmed_median_NumberOfOfficers <- weightedMedian(trimmed_NumberOfOfficers)
#  cat("Trimmed Median of NumberOfOfficers Variable:", trimmed_median_NumberOfOfficers)


df0$NumberOfOfficers <- ifelse(is.na(df0$NumberOfOfficers), 2, df0$NumberOfOfficers)
hist(df0$NumberOfOfficers, main = "Histogram of Number of Officers at Shooting after Mean Imputation (Between 0-23 Officers)", xlab = "Number of Police 
                           Officers Active at Shooting")
summary(df0$NumberOfOfficers)
kurtosis(df0$NumberOfOfficers)
skewness(df0$NumberOfOfficers)
pct_missing_NumberOfOfficers_imp <- mean(is.na(df0$NumberOfOfficers)) * 100
cat("Percentage of missing data in NumberOfOfficers variable after Median Imputation", pct_missing_NumberOfOfficers_imp, "%\n")



#####
##### Cleaning the Officer Gender variable.
##### Binary variables are created from the original dataset to reflect the gender of the first responding officer.
##### Since the original dataframe shows the gender for each officer that responded to the shooting separated by a semicolon (;),
##### the format of the variable is not suitable for analysis in its current form. For example, if 5 officers responded to a shooting,
##### then the value for the officer gender variable would look something like this: M;M;F;M;U, where M denotes Male, F denotes Female, and U denotes an unknown gender.
##### Again, 3 binary variables were created, one for each gender-group plus one for the unknown gender group, to reflect the presence of the gender for the first officer listed, &
##### so that proper statistical analysis and interpretation can be performed on the gender of the officers .If the first officer listed (aka first officer who responded to shooting) 
##### was a Female then the new binary variable called FemaleOfficer would be assigned a value of 1, while the other binary race variables are assigned a 0 for the same observation since
##### the officer is of a female gender and not the other genders. For the purposes of this analysis, gender and sex will be considered as equivalents.
#####
df0$OfficerGender <- as.factor(df0$OfficerGender)
unique(df0$OfficerGender)
sum(is.na(df0$OfficerGender))
pct_missing_OfficerGender<- mean(is.na(df0$OfficerGender)) * 100
cat("Percentage of missing data in OfficerGender variable:", pct_missing_OfficerGender, "%\n")

df1 <- df0
df1$FemaleOfficers <- str_count(df1$OfficerGender, "F")
df1$MaleOfficers <- str_count(df1$OfficerGender, "M") - str_count(df1$OfficerGender, "FEMALE")
df1$OfficersGenderUnknown <- df1$NumberOfOfficers - (df1$FemaleOfficers + df1$MaleOfficers)
pct_missing_FemaleOfficers <- mean(is.na(df1$FemaleOfficers)) * 100
cat("Percentage of missing data in FemaleOfficers variable:", pct_missing_FemaleOfficers, "%\n")
pct_missing_MaleOfficers <- mean(is.na(df1$MaleOfficers)) * 100
cat("Percentage of missing data in MaleOfficers variable:", pct_missing_MaleOfficers, "%\n")
pct_missing_OfficersGenderUnknown <- mean(is.na(df1$OfficersGenderUnknown)) * 100
cat("Percentage of missing data in OfficersGenderUnknown variable:", pct_missing_OfficersGenderUnknown, "%\n")

df1$TotalOfficersGender <- rowSums(df1[, c("MaleOfficers", "FemaleOfficers", "OfficersGenderUnknown")], na.rm = TRUE)
df1$checktotalGen <- ifelse(df1$TotalOfficersGender == df1$NumberOfOfficers, TRUE, FALSE)
df1$OfficersGenderUnknown <- ifelse(df1$OfficersGenderUnknown <= -1, 0, df1$OfficersGenderUnknown)
df1$TotalOfficersGender <- rowSums(df1[, c("MaleOfficers", "FemaleOfficers", "OfficersGenderUnknown")], na.rm = TRUE)
df1$checktotalGen <- ifelse(df1$TotalOfficersGender == df1$NumberOfOfficers, TRUE, FALSE)

# Converting the binary Officer Gender variables to numerical variables.
df1$FemaleO <- ifelse(df1$FemaleOfficers >= 1, 1, 0)
df1$MaleO <- ifelse(df1$MaleOfficers >= 1, 1, 0)
df1$UGO <- ifelse(df1$OfficersGenderUnknown >= 1, 1, 0)



#####
##### Cleaning the Officer Race variable.
##### Binary variables are created from the original dataset to reflect the race of the first responding officer.
##### Since the original dataframe shows the all of the races for each officer that responded to the shooting (separated by a semicolon ';'),
##### the format of the variable is not suitable for analysis in its' current form. For example, if 5 officers responded to a shooting,
##### then the value for the variable would look something like this: W;W;L;A;B, where W denotes White, L denotes Latino, A denotes Asian, and
##### B denotes Black. Again, 5 binary variables were created, one for each major race-group, to show the presence of a race for the first officer listed
##### in order to perform proper analyses for the race of the officer. If the first officer listed (aka first officer who responded to shooting) was White
##### then the new binary variable called WhiteOfficer would be assigned a value of 1, while the other binary race variables are assigned a 0 for the same
##### observation since the officer was of the White race and not the other races.
#####
df1$OfficerRace <- as.factor(df1$OfficerRace)
unique(df0$OfficerRace)
sum(is.na(df0$OfficerRace))

df2 <- df1
df2$OfficersWhite    <- str_count(df2$OfficerRace, "W") - str_count(df2$OfficerRace, "W/A") - str_count(df2$OfficerRace, "A/W") - 
  str_count(df2$OfficerRace, "W;W;H;B;W;W;B") -  str_count(df2$OfficerRace, "W;W;H;B;W;W;B") -  
  str_count(df2$OfficerRace, "W;W;H;B;W;W;B")
df2$OfficersBlack    <- str_count(df2$OfficerRace, "B")
df2$OfficersLatino   <- str_count(df2$OfficerRace, "H") - str_count(df2$OfficerRace, "WHITE") - str_count(df2$OfficerRace, "W/H") - 
  str_count(df2$OfficerRace, "W/ H") - str_count(df2$OfficerRace, "H/L") + str_count(df2$OfficerRace, "H: H") +
  str_count(df2$OfficerRace, "L") - str_count(df2$OfficerRace, "H: H") - str_count(df2$OfficerRace, "BLACK")
df2$OfficersAAPI     <- str_count(df2$OfficerRace, "A") - str_count(df2$OfficerRace, "AI/AN")  + str_count(df2$OfficerRace, "I") - 
  str_count(df2$OfficerRace, "AI/AN") - str_count(df2$OfficerRace, "A/PI") - str_count(df2$OfficerRace, "A/W") - 
  str_count(df2$OfficerRace, ";NA") - str_count(df2$OfficerRace, "W/A") - str_count(df2$OfficerRace, "WHITE") - 
  str_count(df2$OfficerRace, "ASIAN") - str_count(df2$OfficerRace, "BLACK") - str_count(df2$OfficerRace, "ASIAN")
df2$OfficersMixed    <- str_count(df2$OfficerRace, "M") + str_count(df2$OfficerRace, "W/A") + str_count(df2$OfficerRace, "A/W")
# df2$OfficersRaceUnknown  <- str_count(df2$OfficerRace, "U") + str_count(df2$OfficerRace, "u") - str_count(df2$OfficerRace, "Multi-Racial") - 
# str_count(df2$OfficerRace, "A/PI Unknown") + str_count(df2$OfficerRace, ";NA")
df2$OfficersRaceUnknown <- df2$NumberOfOfficers - (df2$OfficersWhite + df2$OfficersMixed + df2$OfficersBlack + df2$OfficersAAPI + df2$OfficersLatino)

df2$NumberOfOfficers <- as.numeric(df2$NumberOfOfficers)
df2$TotalOfficersRace <- rowSums(df2[, c("OfficersWhite", "OfficersBlack", "OfficersLatino", "OfficersAAPI", "OfficersMixed", "OfficersRaceUnknown")], na.rm = TRUE)
df2$checktotalRace <- ifelse(df2$TotalOfficersRace == df2$NumberOfOfficers, TRUE, FALSE)
df2$OfficersRaceUnknown <- ifelse(df2$OfficersRaceUnknown <= -1, 0, df2$OfficersRaceUnknown)
df2$TotalOfficersRace <- rowSums(df2[, c("OfficersWhite", "OfficersBlack", "OfficersLatino", "OfficersAAPI", "OfficersMixed", "OfficersRaceUnknown")], na.rm = TRUE)
df2$checktotalRace <- ifelse(df2$TotalOfficersRace == df2$NumberOfOfficers, TRUE, FALSE)

df2$checktotal <- ifelse(df2$TotalOfficersRace == df2$TotalOfficersGender, TRUE, FALSE)
df2$OfficersRaceUnknown <- ifelse(df2$checktotal == FALSE, df2$OfficersRaceUnknown + (df2$TotalOfficersGender - df2$TotalOfficersRace), df2$OfficersRaceUnknown)
df2$TotalOfficersRace <- rowSums(df2[, c("OfficersWhite", "OfficersBlack", "OfficersLatino", "OfficersAAPI", "OfficersMixed", "OfficersRaceUnknown")], na.rm = TRUE)
df2$checktotalRace <- ifelse(df2$TotalOfficersRace == df2$NumberOfOfficers, TRUE, FALSE)
df2$checktotal <- ifelse(df2$TotalOfficersRace == df2$TotalOfficersGender, TRUE, FALSE)

names(df2)[names(df2) == "NumberOfOfficers"] <- "OLDNumberOfOfficers"
names(df2)[names(df2) == "TotalOfficersRace"] <- "NumberOfOfficers"
df2$NumberOfOfficers <- rowSums(df2[, c("OfficersWhite", "OfficersBlack", "OfficersLatino", "OfficersAAPI", "OfficersMixed", "OfficersRaceUnknown")], na.rm = TRUE)
df2$checktotalRace <- ifelse(df2$NumberOfOfficers == df2$OLDNumberOfOfficers, TRUE, FALSE)
df2$checktotal <- ifelse(df2$TotalOfficersGender == df2$NumberOfOfficers, TRUE, FALSE)

df2$WO <- ifelse(df2$OfficersWhite >= 1, 1, 0)
df2$BO <- ifelse(df2$OfficersBlack >= 1, 1, 0)
df2$LO <- ifelse(df2$OfficersLatino >= 1, 1, 0)
df2$AO <- ifelse(df2$OfficersAAPI >= 1, 1, 0)
df2$MO <- ifelse(df2$OfficersMixed >= 1, 1, 0)
df2$URO <- ifelse(df2$OfficersRaceUnknown >= 1, 1, 0)



##### 
##### Checking variables for missing observations. Note that this is different than observations with a
##### of 'Unknown'.
##### 
pct_missing_Fatal <- mean(is.na(df2$Fatal)) * 100
cat("Percentage of missing data in Fatal variable:", pct_missing_Fatal, "%\n")
pct_missing_SubjectArmed <- mean(is.na(df2$SubjectArmed)) * 100
cat("Percentage of missing data in SubjectArmed variable:", pct_missing_SubjectArmed, "%\n")
pct_missing_SubjectRace <- mean(is.na(df2$SubjectRace)) * 100
cat("Percentage of missing data in SubjectRace variable:", pct_missing_SubjectRace, "%\n")
pct_missing_SubjectGender <- mean(is.na(df2$SubjectGender)) * 100
cat("Percentage of missing data in SubjectGender variable:", pct_missing_SubjectGender, "%\n")
pct_missing_SubjectAge <- mean(is.na(df2$SubjectAge)) * 100
cat("Percentage of missing data in SubjectAge variable:", pct_missing_SubjectAge, "%\n")
pct_missing_NumberOfShots <- mean(is.na(df2$NumberOfShots)) * 100
cat("Percentage of missing data in NumberOfShots variable:", pct_missing_NumberOfShots, "%\n")
pct_missing_NumberOfOfficers <- mean(is.na(df2$NumberOfOfficers)) * 100
cat("Percentage of missing data in NumberOfOfficers variable:", pct_missing_NumberOfOfficers, "%\n")
pct_missing_MaleOfficers <- mean(is.na(df2$MaleOfficers)) * 100
cat("Percentage of missing data in MaleOfficers variable:", pct_missing_MaleOfficers, "%\n")
pct_missing_FemaleOfficers <- mean(is.na(df2$FemaleOfficers)) * 100
cat("Percentage of missing data in FemaleOfficers variable:", pct_missing_FemaleOfficers, "%\n")
pct_missing_OfficersGenderUnknown <- mean(is.na(df2$OfficersGenderUnknown)) * 100
cat("Percentage of missing data in OfficersGenderUnknown variable:", pct_missing_OfficersGenderUnknown, "%\n")
pct_missing_OfficersWhite <- mean(is.na(df2$OfficersWhite)) * 100
cat("Percentage of missing data in OfficersWhite variable:", pct_missing_OfficersWhite, "%\n")
pct_missing_OfficersBlack <- mean(is.na(df2$OfficersBlack)) * 100
cat("Percentage of missing data in OfficersBlack variable:", pct_missing_OfficersBlack, "%\n")
pct_missing_OfficersLatino <- mean(is.na(df2$OfficersLatino)) * 100
cat("Percentage of missing data in OfficersLatino variable:", pct_missing_OfficersLatino, "%\n")
pct_missing_OfficersAAPI <- mean(is.na(df2$OfficersAAPI)) * 100
cat("Percentage of missing data in OfficersAAPI variable:", pct_missing_OfficersAAPI, "%\n")
pct_missing_OfficersMixed <- mean(is.na(df2$OfficersMixed)) * 100
cat("Percentage of missing data in OfficersMixed variable:", pct_missing_OfficersMixed, "%\n")
pct_missing_OfficersRaceUnknown <- mean(is.na(df2$OfficersRaceUnknown)) * 100
cat("Percentage of missing data in OfficersRaceUnknown variable:", pct_missing_OfficersRaceUnknown, "%\n")



##### 
##### Converting variables to factor variables and numerical variables, as appropriate.
#####
df2$Fatal <- factor(df2$Fatal)
df2$SubjectArmed <- factor(df2$SubjectArmed)
df2$SubjectRace <- factor(df2$SubjectRace)
df2$SubjectGender <- factor(df2$SubjectGender)
df2$SubjectAge <- as.numeric(df2$SubjectAge)
df2$NumberOfShots <- as.numeric(df2$NumberOfShots)
df2$NumberOfOfficers <- as.numeric(df2$NumberOfOfficers)
df2$MaleOfficers <- as.numeric(df2$MaleOfficers)
df2$FemaleOfficers <- as.numeric(df2$FemaleOfficers)
df2$OfficersGenderUnknown <- as.numeric(df2$OfficersGenderUnknown)
df2$OfficersWhite <- as.numeric(df2$OfficersWhite)
df2$OfficersBlack <- as.numeric(df2$OfficersBlack)
df2$OfficersLatino <- as.numeric(df2$OfficersLatino)
df2$OfficersAAPI <- as.numeric(df2$OfficersAAPI)
df2$OfficersMixed <- as.numeric(df2$OfficersMixed)
df2$OfficersRaceUnknown <- as.numeric(df2$OfficersRaceUnknown)



##### 
##### Printing the different factor levels of each variable used in the final dataframe.
#####
unique(df2$Fatal)
unique(df2$SubjectArmed)
unique(df2$SubjectRace)
unique(df2$SubjectGender)
unique(df2$SubjectAge)
unique(df2$NumberOfShots)
unique(df2$NumberOfOfficers)
unique(df2$MaleOfficers)
unique(df2$FemaleOfficers)
unique(df2$OfficersGenderUnknown)
unique(df2$OfficersWhite)
unique(df2$OfficersBlack)
unique(df2$OfficersLatino)
unique(df2$OfficersAAPI)
unique(df2$OfficersMixed)
unique(df2$OfficersRaceUnknown)
unique(df2$MaleO)
unique(df2$FemaleO)
unique(df2$UGO)
unique(df2$WO)
unique(df2$BO)
unique(df2$LO)
unique(df2$AO)
unique(df2$MO)
unique(df2$URO)



#########################################################################################################################################################
df3 <- subset(df2, select = -c(OLDNumberOfOfficers,TotalOfficersGender,TotalOfficersGender,checktotal,checktotalRace,checktotalGen))
colnames(df3)

#####
##### Creating new dataframe of cleaned data and removing unknown observations.



unique(df3$SubjectGender)
# Assuming that the variable with "Unknown" observations is called "my_variable"
# Reorder and numerize SubjectRace
df3$SubjectGender <- factor(df3$SubjectGender, levels = c("MALE", "FEMALE", "Unknown"))
df3$SubjectGender <- ifelse(df3$SubjectGender == "Unknown", NA, df3$SubjectGender)
unique(df3$SubjectGender)
df3$SubjectGender <- as.numeric(df3$SubjectGender)
unique(df3$SubjectGender)
pct_missing_SubjectGender <- mean(is.na(df3$SubjectGender)) * 100
cat("Percentage of missing data in SubjectGender:", pct_missing_SubjectGender, "%\n")

unique(df3$SubjectRace)
df3$SubjectRace <- factor(df3$SubjectRace, levels = c("WHITE", "BLACK", "LATINO", "ASIAN", "OTHER", "Unknown"))
df3$SubjectRace <- ifelse(df3$SubjectRace == "Unknown", NA, df3$SubjectRace)
unique(df3$SubjectRace)
df3$SubjectRace <- as.numeric(df3$SubjectRace)
unique(df3$SubjectRace)
pct_missing_SubjectRace <- mean(is.na(df3$SubjectRace)) * 100
cat("Percentage of missing data in SubjectRace:", pct_missing_SubjectRace, "%\n")

unique(df3$Fatal)
df3$Fatal <- factor(df3$Fatal, levels = c("FATAL", "NON-FATAL", "Unknown"))
df3$Fatal <- ifelse(df3$Fatal == "Unknown", NA, df3$Fatal)
unique(df3$Fatal)
df3$Fatal <- as.numeric(df3$Fatal)
unique(df3$Fatal)
pct_missing_Fatal1 <- mean(is.na(df3$Fatal)) * 100
cat("Percentage of missing data in Fatal:", pct_missing_Fatal1, "%\n")

unique(df3$SubjectArmed)
df3$SubjectArmed <- factor(df3$SubjectArmed, levels = c("ARMED", "UNARMED", "Unknown"))
df3$SubjectArmed <- ifelse(df3$SubjectArmed == "Unknown", NA, df3$SubjectArmed)
unique(df3$SubjectArmed)
df3$SubjectArmed <- as.numeric(df3$SubjectArmed)
unique(df3$SubjectArmed)
pct_missing_SubjectArmed <- mean(is.na(df3$SubjectArmed)) * 100
cat("Percentage of missing data in SubjectArmed:", pct_missing_SubjectArmed, "%\n")

# Create the SubjectAgeCat variable
df3 <- df3 %>%
  mutate(SubjectAgeCat = case_when(
    SubjectAge < 21 ~ "Youth",
    SubjectAge >= 21 & SubjectAge <= 34 ~ "Young Adult",
    SubjectAge >= 35 & SubjectAge <= 59 ~ "Middle-Aged",
    SubjectAge >= 60 ~ "Elderly"
  ))


######

# Install and load the mice package
install.packages("mice")
library(mice)
# Set up the imputation model
imp_model <- mice(df3, method = "pmm", seed = 123)
# Run the imputation process
imp_df <- complete(imp_model)
# The resulting data frame contains imputed values for the variables with "Unknown" observations
unique(imp_df$SubjectArmed)
# Convert the variable SubjectArmed back to a factor
imp_df$SubjectArmed <- as.factor(imp_df$SubjectArmed)
# Assign the original levels to the factor
levels(imp_df$SubjectArmed) <- c("ARMED", "UNARMED")
unique(imp_df$SubjectArmed)
# Convert the variable Fatal back to a factor
unique(imp_df$Fatal)
imp_df$Fatal <- as.factor(imp_df$Fatal)
# Assign the original levels to the factor
levels(imp_df$Fatal) <- c("FATAL", "NON-FATAL")
unique(imp_df$Fatal)
# Convert the variable SubjectRace back to a factor
unique(imp_df$SubjectRace)
imp_df$SubjectRace <- as.factor(imp_df$SubjectRace)
# Assign the original levels to the factor
levels(imp_df$SubjectRace) <- c("WHITE", "BLACK", "LATINO", "ASIAN", "OTHER")
unique(imp_df$SubjectRace)
# Convert the variable SubjectGender back to a factor
unique(imp_df$SubjectGender)
imp_df$SubjectGender <- as.factor(imp_df$SubjectGender)
# Assign the original levels to the factor
levels(imp_df$SubjectGender) <- c("MALE", "FEMALE")
unique(imp_df$SubjectGender)

library(dplyr)

imp_df <- imp_df %>%
  filter(SubjectRace != "OTHER") %>%
  mutate(SubRace2 = ifelse(SubjectRace == "WHITE", "White", "Non-White"),
         OfficersNHNW = OfficersBlack + OfficersAAPI,
         ArmedRace = paste(SubjectArmed, SubjectRace, sep = " "),
         ArmedRaceAge = paste(SubjectArmed, SubjectRace, SubjectAgeCat, sep = " "))


imp_df$RaceAge <- paste(as.character(imp_df$SubjectRace), as.character(imp_df$SubjectAgeCat), sep = " ")
imp_df$RaceAge2 <- paste(as.character(imp_df$SubRace2), as.character(imp_df$SubjectAgeCat), sep = " ")
imp_df$RaceGender <- paste(as.character(imp_df$SubRace2), as.character(imp_df$SubjectGender), sep = " ")
imp_df$RaceGender2 <- paste(as.character(imp_df$SubjectRace), as.character(imp_df$SubjectGender), sep = " ")
imp_df$RaceGender <- factor(imp_df$RaceGender) 
imp_df$RaceGender2 <- factor(imp_df$RaceGender2) 
imp_df$RaceAge2 <- factor(imp_df$RaceAge2) 
imp_df$RaceAge <- factor(imp_df$RaceAge) 
imp_df$Fatal <- factor(imp_df$Fatal) 
imp_df$SubRace2 <- factor(imp_df$SubRace2)
imp_df$SubjectGender <- factor(imp_df$SubjectGender)
imp_df$SubjectRace <- factor(imp_df$SubjectRace)
imp_df$SubjectArmed <- factor(imp_df$SubjectArmed)
imp_df$MaleO <- factor(imp_df$MaleO) 
imp_df$FemaleO <- factor(imp_df$FemaleO) 
imp_df$ArmedRace <- factor(imp_df$ArmedRace) 
imp_df$ArmedRaceAge <- factor(imp_df$ArmedRaceAge) 
imp_df$RaceGender2 <- factor(imp_df$RaceGender2) 
imp_df$Fatal <- relevel(imp_df$Fatal, ref = "NON-FATAL")
imp_df$SubjectRace <- relevel(imp_df$SubjectRace, ref = "WHITE")
imp_df$SubRace2 <- relevel(imp_df$SubRace2, ref = "White")
imp_df$SubjectArmed <- relevel(imp_df$SubjectArmed, ref = "ARMED")
imp_df$SubjectAgeCat <- relevel(imp_df$SubjectAgeCat, ref = "Young Adult")
imp_df$Region <- relevel(imp_df$Region, ref = "Pacific W")
imp_df$SubjectGender <- relevel(imp_df$SubjectGender, ref = "MALE")
imp_df$Gov <- relevel(imp_df$Gov, ref = "Republican")
imp_df$RaceAge <- relevel(imp_df$RaceAge, ref = "WHITE Young Adult")
imp_df$RaceAge2 <- relevel(imp_df$RaceAge2, ref = "White Middle-Aged")
imp_df$RaceGender2 <- relevel(imp_df$RaceGender2, ref = "WHITE MALE")
imp_df$RaceGender <- relevel(imp_df$RaceGender, ref = "White MALE")
imp_df$MaleO <- relevel(imp_df$MaleO, ref = "0")
imp_df$FemaleO <- relevel(imp_df$FemaleO, ref = "0")
imp_df$ArmedRace <- relevel(imp_df$ArmedRace, ref = "UNARMED WHITE")
imp_df$ArmedRaceAge <- relevel(imp_df$ArmedRaceAge, ref = "ARMED WHITE Youth")




unique(imp_df$Fatal)
unique(imp_df$SubjectRace)
unique(imp_df$SubRace2)
unique(imp_df$SubjectArmed)
unique(imp_df$Region)
unique(imp_df$SubjectAgeCat)
unique(imp_df$SubjectGender)
unique(imp_df$Gov)
unique(imp_df$RaceAge)
unique(imp_df$RaceAge2)
unique(imp_df$RaceGender)
unique(imp_df$RaceGender2)
unique(imp_df$ArmedRaceAge)



#####
df4 <- df3 %>%
  filter(Fatal != "Unknown") %>% filter(SubjectArmed != "Unknown") %>% filter(SubjectRace != "Unknown") %>% 
  filter(SubjectRace != "OTHER") %>% filter(SubjectGender != "Unknown")


unique(df4$Fatal)
unique(df4$SubjectArmed)
unique(df4$SubjectRace)
unique(df4$SubjectGender)
unique(df4$SubjectAge)
unique(df4$NumberOfShots)
unique(df4$NumberOfOfficers)
unique(df4$MaleOfficers)
unique(df4$FemaleOfficers)
unique(df4$OfficersGenderUnknown)
unique(df4$OfficersWhite)
unique(df4$OfficersBlack)
unique(df4$OfficersLatino)
unique(df4$OfficersAAPI)
unique(df4$OfficersMixed)
unique(df4$OfficersRaceUnknown)
unique(df4$MaleO)
unique(df4$FemaleO)
unique(df4$UGO)
unique(df4$WO)
unique(df4$BO)
unique(df4$LO)
unique(df4$AO)
unique(df4$MO)
unique(df4$URO)


# Alternatively, you can use the negation operator `!` to subset for observations that are NOT greater than or equal to zero:
# df4 <- subset(df4, !OfficersRaceUnknown > 0)






#########################################################################################################################################################

#### Final dataframe (df)
###
colnames(df4)
df <- df4[ , c("NumberOfSubjects","Fatal","SubjectArmed","SubjectRace","SubjectGender","SubjectAge","NatureOfStop",
               "OfficerRace", "OfficerGender","NumberOfOfficers","NumberOfShots","Department","City","FullNarrative",
               "FemaleOfficers","MaleOfficers","OfficersGenderUnknown","MaleO","FemaleO","UGO","OfficersWhite","OfficersBlack",
               "OfficersLatino","OfficersAAPI","OfficersMixed","OfficersRaceUnknown","WO","BO","LO","AO","MO","URO")]
colnames(df)


# Recode levels of  variables
df$SubjectRace <- relevel(df$SubjectRace, ref = "WHITE")
df$SubjectGender <- relevel(df$SubjectGender, ref = "FEMALE")
df$SubjectArmed <- relevel(df$SubjectArmed, ref = "ARMED")
df$Fatal <- relevel(df$Fatal, ref = "NON-FATAL")

unique(df$Fatal)
unique(df$SubjectArmed)
unique(df$SubjectRace)
unique(df$SubjectGender)
unique(df$SubjectAge)
unique(df$NumberOfShots)
unique(df$NumberOfOfficers)
unique(df$MaleOfficers)
unique(df$FemaleOfficers)
unique(df$OfficersGenderUnknown)
unique(df$OfficersRaceUnknown)
unique(df$OfficersWhite)
unique(df$OfficersBlack)
unique(df$OfficersLatino)
unique(df$OfficersAAPI)
unique(df$OfficersMixed)
unique(df$MaleO)
unique(df$FemaleO)
unique(df$UGO)
unique(df$WO)
unique(df$BO)
unique(df$LO)
unique(df$AO)
unique(df$MO)
unique(df$URO)






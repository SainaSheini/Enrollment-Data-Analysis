---
title: "Report Code"
author: "Saina Sheini"
date: "May 27, 2022"
output:
  html_document:
    df_print: paged
  pdf_document: default
---
```{r label = "Time period - for automated report", message=FALSE, warning=FALSE}
# what is the term of this research?
term <- "Fall 2021"
```

\ 
\section{\large{\text{Overview} }}

\quad In this document, I analyzed three data sets provided by the University: "enrollment", "grades" and "programs". The enrollment data set includes information about registration for a single DU term `r term`, covering student registration status, their demographics, and program. The grades data set provides final grades for each student for all courses in that same term. And finally, the program data set offers a mapping of college, degree, and academic unit information. Using these three data sets, I attempted to find answers to eight challenging questions in three major sections.

\quad In Part One, I proceeded to both clean the data sets and perform a few sanity checks on them. These checks are especially vital for automation processes. Next, I followed through with the final data product generation process, generating a student-level data set. In the third part, I tried my hand at finding the best methods of answering the questions through data manipulation and statistics. Last, I made minor changes to the student-level data set created in the previous sections and completed some final preparation tasks.
\ 
\section{\large{`r paste0("Part", 1, ":", " Quick investigation on what the data offer, data cleaning and necessary prepration for the automation process")`} }

\quad Before getting to the questions, it is necessary to make sure the provided data sets are reliable. In addition, considering that all future data sets will have the same characteristics, I aimed to design an automated process. Automation needs a few extra steps of data preparation. All major data structure factors, including attribute types and names of any future data sets should be inline with the automation code. Therefore, aside from NAs and duplicated value identification steps, formatting unification and verifying student ID (see table 2), I have also created a demonstration of attribute types for each data set (see table 1), and a table of data set dimensions (see table 3). \

```{r label = "Data and Packages", message=FALSE, warning=FALSE}

#---- LIBRARIES ---- 
library(tidyverse)
library(formattable)
library(tibble)
library(formattable)
library(knitr)
library(kableExtra)
library(plotrix)
library(gridExtra)
library(viridis)
library(gridExtra)
library(cowplot)
library(gmodels)
library(ggpubr)
library(janitor)
library(pander)

#---- INPUT PATHS ----

## this is the location of all project files
DU_path_input <- "/Users/saina/Desktop/DU/Data_Scien_Tech_Assess/"

## names of all necessary input data file
DU_file_Enrollment <- "fall_enrollment.csv"
DU_file_Grades <- "grades.csv"
DU_file_Program <- "program_data.csv"

#---- READ IN DATA ----

Enrollment <- read.csv(paste0(DU_path_input, DU_file_Enrollment),stringsAsFactors=FALSE)
Grades <- read.csv(paste0(DU_path_input, DU_file_Grades),stringsAsFactors=FALSE)
Program <- read.csv(paste0(DU_path_input, DU_file_Program),stringsAsFactors=FALSE)

#---- OUTPUT PATHS ----

DU_path_output <- "/Users/saina/Desktop/DU/Outputs/"
Product_name <- "Student_Lavel.csv"
Attr_names <- "Attr_names.csv" # this file is produced in this script, but will be input for automation script

#print("Done with reading data and calling packages")
```

```{r label = "Refinements", message=FALSE, warning=FALSE, fig.pos = 'h'}

# SOME REFINEMENTS AND DATA CHECKS

## unified column name formats
names(Enrollment) <- tolower(names(Enrollment))
names(Grades) <- tolower(names(Grades))
names(Program) <- tolower(names(Program))

## drop nonsensical duplicated records 
### not dropping duplicated records of Grades data because they are meaningful (different courses same term). 
Program_duplicated <- (nrow(Program) == nrow(distinct(Program)))
### Program data does not include any duplicated records
Enrollment_duplicated <- (nrow(Enrollment) == nrow(distinct(Enrollment)))
### Enrollment does
Enrollment <- distinct(Enrollment) # 15 duplicates

## check variable types
TypeEnrollment <- rownames_to_column(as.data.frame(sapply(Enrollment, typeof)), "Enrollment Attributes")
names(TypeEnrollment)[2] <- "Types"

TypeGrades <- rownames_to_column(as.data.frame(sapply(Grades, typeof)), "Grades Attributes")
names(TypeGrades)[2] <- "Types"

TypeProgram <- rownames_to_column(as.data.frame(sapply(Program, typeof)), "Program Attributes")
names(TypeProgram)[2] <- "Types"

### a table announcing these numbers
knitr::kable(
  list(TypeGrades, TypeEnrollment, TypeProgram),  
  caption = 'Attribute types', booktabs = TRUE) %>% 
  kable_styling(latex_options = "HOLD_position")

## creating a data set of column names to use in automation pre-checks. Need to be ran only once
### to flag new columns in next data sets
colsNames <- c(names(Enrollment), names(Grades), names(Program))
#write.csv(as.data.frame(colsNames), paste0(DU_path_output, Attr_names), col.names = c("Names"),sep = ",", row.names = F)

## creating a list of all student ids; this will be used for student level final product.
student_ids <- unique(c(unique(Enrollment$id), unique(Grades$id))) # 9746
Missing_ids <- setdiff(unique(Grades$id), unique(Enrollment$id)) # 0, mistakenly not named in enrollment data set
DroppedOut_ids <- setdiff(unique(Enrollment$id), unique(Grades$id)) # 1278

### a table announcing these numbers
Number <- c(length(student_ids), length(Missing_ids), length(DroppedOut_ids))
Students <- c("All", "Error in Enrollment", "Not Enrolled")

kable(data.frame(Students, Number), caption = "Students Information") %>% 
  kable_styling(latex_options = "HOLD_position", full_width = F)

## what does this data look like?
### creating a table of dim()
Row <- c(dim(Enrollment)[1], dim(Grades)[1], dim(Program)[1])
Column <- c(dim(Enrollment)[2], dim(Grades)[2], dim(Program)[2])
Data <- c("Enrollment", "Grades", "Program")
kable(data.frame(Data, Row, Column), caption = "Data Sets Dimension") %>% # dimension
  kable_styling(latex_options = "HOLD_position", full_width = F)

```

```{r label = "Automation pre-checks", message=FALSE, warning=FALSE}

# SANITY CHECK FLAGS

## are they any NAs in any of the three data?
## do the columns have the same names as I want them to have in the data sets? (for automation)

Attr_names_dt <- read.csv(paste0(DU_path_output, Attr_names)) # created in last chunk
dts <- list(Enrollment,Grades, Program)
appended_dt <- NULL

for (i in 1:3){
  # NAs
  A <- dts[[i]] %>%
    summarise(across(everything(), ~ sum(is.na(.x))))
  appended_dt <- as.data.frame(append(appended_dt, A))
    if (i == 3 & (sum(colSums(appended_dt) != 0))){
      print("We have NAs in a dataset") # flagging the need of attention
      print(appended_dt)
    }
  # col names
  if (sum(!(names(dts[[i]]) %in% Attr_names_dt$colsNames) != 0)) { 
    print("Not the same column names") # flagging the need of attention
  }
}
# 
```

\quad Now I have clean data sets. The code will flag anything that needs attention.
\section{\large{`r paste0("Part", 2, ":", " Student-level data product")`} }
```{r label = "Student level file", message=FALSE, warning=FALSE, results='hide'}

# MAKING ONE SINGLE DATA SET - COMBINING ALL DATA SETS WITH GRADES AND FINAL MAJOR

names(Enrollment) # 18214
names(Grades) # 27520
names(Program) # 70

## Enrollment + grade
Enrollment_grade <- left_join(Enrollment, Grades[,c(1,3)], by = "id") #56318
Enrollment_grade <- distinct(Enrollment_grade) # 45820

sum(is.na(Enrollment_grade$final_course_grade)) # 1278 - students who have not stayed enrolled do not have a grade
summary(Enrollment_grade) # checking for other possible NAs produced

## Enrollment_grade + Program
## to do this, to ensure a valid join, I join the Enrollment_grade and program data on three shared columns
Enrollment_full <- left_join(Enrollment_grade, Program, by = c("college" = "college", "degr" = "degree", "majr" = "major")) #45820

```
\quad Since the data sets' volume is not large, consolidating all of them is plausible. In the process, I ended up with a single data set with 2.5 times the volume of the initial enrollment data. This is mainly due to the grades of various courses that are presented in the Grades data set. A few simple sanity checks were also performed to ensure that the final product of the merges was reliable. The output is used as the input for the rest of this analysis, with `r nrow(Enrollment_full)` records and `r ncol(Enrollment_full)` attributes describing the students, their performance and degree information.
\section{\large{`r paste0("Part", 3, ":", " Questions")`} }
#### Q1: What is the _persistence rate_ from week three to the end of the term?\
\ 
\quad To calculate the persistence rate, I used this formula: \

$$\text{\% persistence rate} = \frac {\text {students who stayed enrolled till the end of the term}}{\text{number of enrolled students at the begining of the term} - \text{students who finished school}} \times 100$$
\ 
\ 
\quad Since the time period is just one term, I can safely drop the second statement in the denominator and move forward. I created a binary variable to flag the students that had both census status on their record (1 for staying enrolled, 0 for not staying enrolled), and calculated the percentage of those who stayed enrolled in class. I decided to keep this variable in the final data product, since it provides a straight forward way of recognizing the two categories.
```{r label = "Data Manipulation - persistence rate", message=FALSE, warning=FALSE, results='hide'}

# CREATING PERSISTENCE RATE

length(unique(Enrollment_full$id)) #9746
table(Enrollment_full$census)  #EOT  WK3 22271 23549 
table(Enrollment_full$term_code) #202170 45820

## exclude enrolled students at week 3 - keeping only student id and census
data_WK3 <- Enrollment_full %>%
  select(id, census, legal_sex_desc) %>% # keeping gender variable for the next question
  filter(census == "WK3") %>%
  distinct(.) # 9746

## exclude enrolled students at the end of the term and create a dummy - keeping only student id and census
data_EOT <- Enrollment_full %>%
  select(id, census) %>%
  filter(census == "EOT") %>%
  dplyr::select(id) %>%
  mutate(persist = 1) %>%
  distinct(.) # 8468

## Joining the two and calculating of persistence rate
Enrollment_merged <- data_WK3 %>%
  left_join(data_EOT, by = 'id') %>% #9746 
  mutate(Persistence_rate = sum(persist, na.rm = T)/n())

## adding the two persistence related new measures to the full student level data set
Enrollment_full <- left_join(Enrollment_full, Enrollment_merged[,c(1,4,5)], by = "id") #45820
```
\quad The persistence rate of the `r term` term is `r paste0((round(unique(Enrollment_merged$Persistence_rate),3)))`, according to the Enrollment information provided to us. Let's move on to the next question.

#### Q2: Is there a statistically significant difference in persistence rate between _males_ and _females_? 

\quad To investigate the possible difference in persistence rates between genders, I decided to perform a Chi-square test to give additional context for the observed results. Chi-square test is a helpful tool in investigating the relationship between two categorical variables. First, I constructed a categorical attribute to flag the students that have stayed enrolled. Next, I created a contingency table and ran the Chi-Square test of independence. My null hypothesis is as followed:
$\begin{aligned}
{H_o} = \text{ The persistence rate is independent of gender in this cohort.} \ 
\end{aligned}$
The results are as follows:
```{r label = "Statistical test", message=FALSE, warning=FALSE, fig.align='center'}

# CHI-SQUARE TEST

## let's create a categorical value of persistence
### using the data set from last section to avoid repetition of id frequencies caused by joining
Enrollment_merged$persist_cat <- ifelse(is.na(Enrollment_merged$persist), "Attrition", "Persistence")
Enrollment_test <- Enrollment_merged[, c("legal_sex_desc", "persist_cat")]

## creating a contingency table 
Enrollment_test <- table(Enrollment_test$legal_sex_desc, Enrollment_test$persist_cat)
test <- chisq.test(Enrollment_test)
pander(test)
chi <- round(test$statistic, 3) 
pval <- round(test$p.value, 3)
#test$expected
critical_val <- round(qchisq(0.05, df = 1, lower.tail=FALSE), 3)

# if statement for p value 
if (pval > 0.05){
  sig <- "not significant"
  result_p <- "the test found no"
} else {
  sig <- "significant"
  result_p <- "the test found"
}

# if statement for chi-square value 
if (chi > critical_val){
  result_c <- "larger"
} else {
  result_c <- "smaller"
}

```

\quad The chi-square value is `r  chi`, `r result_c` than the critical value of the degree of freedom of 1 (`r critical_val` for p = 0.05; based on the chi-square table). In addition, the p-value is `r sig`. As a result, `r result_p` evidence of a statistically significant difference in persistence rates between genders. \ 

#### Q3: Describe the makeup of the class in terms of race/ethnicity and gender. \

\quad As a first step to answering this question, I constructed one unified measure of race/ethnicity using the three attributes of visa types, ethnicity, and race, following the guide provided in the question. In this process, I created a binary variable of internationality as well. All visa types will be flagged as international except for ‘PR’, 'RF', and ‘AS’, and of course, the undisclosed statuses (showing blank in the data).
```{r label = "Composition display prep", message=FALSE, warning=FALSE}

# CONSTRUCTING A RACE/ETHNICITY VARIABLE

## replacing the blank values of visa_desc with NAs
Enrollment_full$visa_desc <- ifelse(Enrollment_full$visa_desc == "", NA, Enrollment_full$visa_desc)

## internationals
Enrollment_full$internationality <- ifelse(!(Enrollment_full$visa_desc %in% c("PR", "RF", "AS")) & !is.na(Enrollment_full$visa_desc), 1, 0)

## filling in NAs of internationality with zeros
Enrollment_full$internationality <- ifelse(is.na(Enrollment_full$internationality), 0, Enrollment_full$internationality)

## combining all into race/ethnicity variable
Enrollment_full$Race_Ethnicity <- ifelse(Enrollment_full$internationality == 1, "International", ifelse(Enrollment_full$ethn_desc == "Hipanic or Latino", "Hipanic or Latino", Enrollment_full$race_desc))

female <- round(sum(Enrollment_full$legal_sex_desc == "Female")/nrow(Enrollment_full), 3)*100
male <- round(sum(Enrollment_full$legal_sex_desc == "Male")/nrow(Enrollment_full), 3)*100

# gender distribution results
if (female == male) {
  gender_stat <- "The proportion of legally male and female stuents are the same"
} else {
  if (female > 50) {
    gender_stat <- "More than half of our class"
  } else {
    gender_stat <- "Less than half of our class"
  }
}
```
\quad Having access to the gender attribute of the enrollment data set, I can demonstrate gender composition in the cohort as presented in figure 1. `r gender_stat` (`r paste0(female, "%")`) are legally female students while `r paste0(male, "%")` are male.

```{r label = "Composition display pies", message=FALSE, warning=FALSE, fig.align='center', out.width="82%"}

# PIE CHART 1 FOR GENDER

pie(c((sum(Enrollment_full$legal_sex_desc == "Female"))/nrow(Enrollment_full),(sum(Enrollment_full$legal_sex_desc == "Male"))/nrow(Enrollment_full)), labels = c('57%','43%'),
    col = c('red','lightgreen'), border="white", init.angle = 90, cex = .7)
legend(-1.75,1,legend =c('Female','Male'),
       col = c('red','lightgreen'),
       pch = 22, pt.bg = c('red','lightgreen'),
       cex = .7)
title(main = 'Figure 1: Breakdown of Gender', cex.main = 0.7)
```
```{r label = "Composition display pie two" , message=FALSE, warning=FALSE, fig.align='center', out.width="82%"}

# PIE CHART 2 FOR RACE/ETHNICITY

## data prep for race plot
race_graph <- Enrollment_full %>%
  select(id, Race_Ethnicity) %>%
  distinct(.) %>% # dropping duplicates of students since census and grade create many 
  group_by(Race_Ethnicity) %>%
  summarise(race = n()) %>%
  ungroup()

race_graph$prop <- (race_graph$race)/sum(race_graph$race)*100

## pie chart 2 for race
### ordering the data
race_graph_descend <- race_graph[order(-race_graph$prop), ]

labl <- c("White", "Hispanic", "International", "Black","Asian", "Multiple", "Native American", "Unknown", "Pacific Islander")

pie(c((race_graph_descend$prop)), labels = (paste0(c(round(race_graph_descend$prop)),"%")), col = hcl.colors(length(labl), "Spectral"),
    border="white", init.angle = 90, cex = .7)
legend(-2,1,legend =labl,
       col = hcl.colors(length(labl), "Spectral"),
       pch = 22, pt.bg = hcl.colors(length(labl), "Spectral"),
       cex = .75)
title(main = 'Figure 2: Breakdown of Race', cex.main = 0.7)

## automation variables 
if (race_graph_descend$Race_Ethnicity[race_graph_descend$prop == max(race_graph_descend$prop)] 
    %in% c("White", "white", "WHITE")){
  race_sentence1 <- "Considering the demographics of the state of Colorado, it is not surprising to see the percentage of white students in the lead"
} else {
  race_sentence1 <- paste0("The percentage of", race_graph_descend$Race_Ethnicity[race_graph_descend$prop == max(race_graph_descend$prop)], "students is the highest in this cohort")
}

###  having internationals in top 3 deserves a sentence to itself
if (race_graph_descend$Race_Ethnicity[1] %in% c("International", "international", "INTERNATIONAL")){
  race_sentence2 <- paste0("Having international as the highest demographic in class followed by ", race_graph_descend$Race_Ethnicity[2], " and ", race_graph_descend$Race_Ethnicity[3], "proportions", " is an intriguing emerging pattern!")
} else {
  if (race_graph_descend$Race_Ethnicity[2] %in% c("International", "international", "INTERNATIONAL")){
    race_sentence2 <- paste0("Having international as the second highest demographic in class followed by ", race_graph_descend$Race_Ethnicity[3], "proportions", " is an intriguing emerging pattern!")
  } else {
    if (race_graph_descend$Race_Ethnicity[3] %in% c("International", "international", "INTERNATIONAL")){
      race_sentence2 <- paste0("The high proportion of internationals in third place, closely following ", race_graph_descend$Race_Ethnicity[1], " and ", race_graph_descend$Race_Ethnicity[2], "proportions", " is an intriguing emerging pattern!")
    }
  }
}

race_sentence3 <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN")],2), "%")

race_sentence4 <- race_graph_descend$Race_Ethnicity[race_graph_descend$prop == min(race_graph_descend$prop) & !(race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN"))]

race_sentence4_prop <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity == race_sentence4], 2), "%")

race_sentence5 <- race_graph_descend$Race_Ethnicity[race_graph_descend$prop == min(race_graph_descend$prop[!(race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN", race_sentence4))])]

race_sentence5_prop <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity == race_sentence5], 2), "%")

race_sentence6 <- race_graph_descend$Race_Ethnicity[race_graph_descend$prop == min(race_graph_descend$prop[!(race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN", race_sentence4, race_sentence5))])]

race_sentence6_prop <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity == race_sentence6], 2), "%")

race_sentence7 <- race_graph_descend$Race_Ethnicity[race_graph_descend$prop == min(race_graph_descend$prop[!(race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN", race_sentence4, race_sentence5, race_sentence6))])]

race_sentence7_prop <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity == race_sentence7], 2), "%")

race_sentence8 <- race_graph_descend$Race_Ethnicity[race_graph_descend$prop == min(race_graph_descend$prop[!(race_graph_descend$Race_Ethnicity %in% c("Unknown", "Unknown", "UNKNOWN", race_sentence4, race_sentence5, race_sentence6, race_sentence7))])]

race_sentence8_prop <- paste0(round(race_graph_descend$prop[race_graph_descend$Race_Ethnicity == race_sentence8], 2), "%")
```

\quad Leveraging the constructed race/ethnicity measure of the enrollment data set, the race/ethnicity make up of the cohort is presented as figure 2. `r race_sentence1`. `r race_sentence2` Setting aside the unknown group (`r race_sentence3`), the smallest racial minorities in the class are the `r race_sentence4` (`r race_sentence4_prop`), `r race_sentence5` (`r race_sentence5_prop`), and `r race_sentence6` (`r race_sentence6_prop`) students, with the `r race_sentence7` (`r race_sentence7_prop`) and `r race_sentence8` (`r race_sentence8_prop`) races hovering in the middle.\ 

\quad One other interesting piece of information can be what the composition of the two major demographics (gender and race/ethnicity) looks like in relation to each other. Figure 3 shows both the race/ethnicity and gender of DU's students in Fall 2021.\ 

```{r label = "Composition display bar plot", message=FALSE, warning=FALSE, fig.align='center'}

# RACE/GENDER PYRAMID

## data prep for plot
Enrollment_bar <- Enrollment_full %>%
  select(id, Race_Ethnicity, legal_sex_desc) %>%
  distinct(.) %>% # dropping duplicates of students since census and grade create many 
  group_by(Race_Ethnicity, legal_sex_desc) %>%
  tally() %>%
  mutate( n = ifelse(legal_sex_desc=="Female", n*(-1), n*1)) # manipulating data - negative values for one category

ggplot(Enrollment_bar, aes(y = n, x=reorder(Race_Ethnicity, -n))) +
  geom_col(aes(fill=legal_sex_desc)) +
  scale_fill_manual("", values = c("#66FFFF", "#993300")) +
  scale_y_continuous(limits = c(-1800, 1800), breaks = scales::pretty_breaks(n = 6),
  labels =  function(br) ifelse(abs(br)>=1000,paste0(abs(br)/1000, "k"), abs(br))) +
  labs(caption = "Figure 3: proportion of female and male students in race/ethnicity groups") +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(text = element_text(size=9, vjust = 0.5, hjust=1, face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), plot.title = element_text(size=9, face = "bold", hjust = 0.5), legend.position = 'right')

## automation variables

Enrollment_automation <- Enrollment_full %>%
  select(id, Race_Ethnicity, legal_sex_desc) %>%
  distinct(.) %>% # dropping duplicates of students since census and grade create many 
  group_by(Race_Ethnicity, legal_sex_desc) %>%
  tally() %>%
  ungroup() %>%
  spread(legal_sex_desc, n) %>%
  group_by(Race_Ethnicity) %>%
  summarise(female_race_rate = round((Female/(Female+Male)*100),2), 
            male_race_rate = round((Male/(Female+Male)*100),2),
            closest = Female - Male,
         gender_race_flag = ifelse((Female - Male) > 0, 1, 0))

## a gender is dominant on all races
if (sum(Enrollment_automation$gender_race_flag) == 9){
    sentence1 <- "Female students are the leading gender in this cohort across all races"
  } else {
    if (sum(Enrollment_automation$gender_race_flag) == 0){
      sentence1 <- "Male students are the leading gender in this cohort across all races"
  }
}

## what races has the closest difference
sentence2 <- Enrollment_automation$Race_Ethnicity[Enrollment_automation$closest == min(abs(Enrollment_automation$closest))]
sentence2_rate <- paste0(Enrollment_automation$male_race_rate[Enrollment_automation$Race_Ethnicity == sentence2], "%")

Enrollment_automation <- arrange(Enrollment_automation, -Enrollment_automation$male_race_rate)

races <- unique(Enrollment_automation$Race_Ethnicity[!(Enrollment_automation$Race_Ethnicity %in% c(sentence2))])

sentence3_rate <- list()
sentence3 <- list()

for (i in 1:length(races)){
  sentence3_rate <- append(sentence3_rate, paste0(Enrollment_automation$male_race_rate[Enrollment_automation$Race_Ethnicity == races[i]], "%"))
  sentence3 <- append(sentence3, races[i])
}
``` 
\quad `r sentence1`. `r sentence2` are the race group that has the closest proportion of males and females, with `r sentence2_rate` of males. This percentage is `r sentence3_rate[1]` for `r sentence3[1]`, `r sentence3_rate[2]` for `r sentence3[2]`, `r sentence3[3]` for `r sentence3[3]`, `r sentence3_rate[4]` for `r sentence3[4]`, `r sentence3_rate[5]` for `r sentence3[5]`, `r sentence3_rate[6]` for `r sentence3[6]`, `r sentence3_rate[7]` for `r sentence3[7]` and `r sentence3_rate[8]` for `r sentence3[8]` populations.

```{r label = "Age distribution", message=FALSE, warning=FALSE, fig.align='center'}

# AGE DISTRIBUTION ILLUSTRATIONS

## convert the birth date column to date type
Enrollment_full$birth_date <- as.Date(Enrollment_full$birth_date, format = "%m/%d/%Y")

## calculate age from birth date 
Enrollment_full$age <- round(as.numeric(difftime(Sys.Date(), Enrollment_full$birth_date, units = "weeks"))/52.25)

## data prep for plotting age distribution
Enrollment_age <- Enrollment_full %>%
  select(id, age) %>%
  distinct(.) %>%
  group_by(age) %>%
  tally()

## numeric age plot
age <- ggplot(Enrollment_age, aes(x=age, y=n)) + 
  geom_line(color = "#BF87B3", size = 1.3) +
  geom_point(color = "#3F2D91") +
  theme_bw() +
  scale_x_continuous(limits = c(16, 70)) +
  xlab("") +
  ylab("") +
  theme(text = element_text(size=9, vjust = 0.5, hjust=1, face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), plot.title = element_text(size=8, face = "bold", hjust = 0.5))

## defining categorical age ranges
AgeUnder19 <- round((sum(Enrollment_age$n[Enrollment_age$age < 19])/sum(Enrollment_age$n))*100,1)
Age19 <- round((sum(Enrollment_age$n[Enrollment_age$age == 19])/sum(Enrollment_age$n))*100,1)
Age2022 <- round((sum(Enrollment_age$n[Enrollment_age$age %in% c(20,21,22)])/sum(Enrollment_age$n))*100,1)
AgeOver22 <- round((sum(Enrollment_age$n[Enrollment_age$age > 22 ])/sum(Enrollment_age$n))*100,1)

## creating a table of age ranges with their distributions
Percentage <- c(AgeUnder19, Age19, Age2022, AgeOver22)
Age <- c("AgeUnder19", "Age19", "Age2022", "AgeOver22")
table_age <- qplot(1:10, 1:10, geom = "blank") + 
  theme_void() +
  annotation_custom(grob = tableGrob(data.frame(Age, Percentage), rows = NULL), 
                    xmin = -5, xmax = 10, ymin = 1, ymax = 10)

## attaching two demonstrations together
title <- ggdraw() + 
  draw_label("Figure 4: Age Distribution: A) Barplot B) Percentage table", fontface='bold', size = 9)
fg4 <- plot_grid(age, table_age, align = "v", labels = c('A', 'B'))
plot_grid(title, fg4, ncol = 1, rel_heights=c(0.1, 1))

## automation variables
Age <- c("under 19", "19", "between 20 and 22", "over 22")
age_auto <- data.frame(Age, Percentage)
sentence1 <- age_auto$Age[age_auto$Percentage == max(age_auto$Percentage)]
sentence1_rate <- paste0(age_auto$Percentage[age_auto$Age == sentence1], "%")

sentence2 <- age_auto$Age[age_auto$Percentage == max(age_auto$Percentage[!(age_auto$Age %in% c(sentence1))])]
sentence2_rate <- paste0(age_auto$Percentage[age_auto$Age == sentence2], "%")

sentence3 <- age_auto$Age[age_auto$Percentage == max(age_auto$Percentage[!(age_auto$Age %in% c(sentence1, sentence2))])]
sentence3_rate <- paste0(age_auto$Percentage[age_auto$Age == sentence3], "%")

sentence4 <- age_auto$Age[age_auto$Percentage == max(age_auto$Percentage[!(age_auto$Age %in% c(sentence1, sentence2, sentence3))])]
sentence4_rate <- paste0(age_auto$Percentage[age_auto$Age == sentence4], "%")

sentence5 <- max(Enrollment_age$age)
sentence6 <- min(Enrollment_age$age)
```

#### Q4: How is age distributed in this class?\
\quad As presented in figure 4, the majority of the students are `r sentence1` (`r sentence1_rate`). `r sentence2_rate` of the class is `r sentence2`. The remainder belongs to ages of `r sentence4` (`r sentence4_rate`) and `r sentence3` (`r sentence3_rate`). The oldest reported age is `r sentence5`, and the youngest is `r sentence6`.
```{r label = "Grade distribution", message=FALSE, warning=FALSE, fig.align='center'}

#GRADE DISTRIBUTION

## choosing to use Grades data directly for this question
## function to convert letter grades to numeric version
grade_scores <- function(x) { 
  A <- factor(x, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F")) ## added score for D-
  values <- c(4, 3.7, 3.3, 3, 2.7, 2.3, 2, 1.7, 1.3, 1, 0.7, 0)
  values[A]
}

Grades$grade_scores <- lapply(Grades$final_course_grade, grade_scores)
Enrollment_full$grade_scores <- lapply(Enrollment_full$final_course_grade, grade_scores) # adding to student level data
Grades$grade_scores <- as.numeric(Grades$grade_scores)
Enrollment_full$grade_scores <- as.numeric(Enrollment_full$grade_scores)

Grades_byID <- Grades %>%
  group_by(id) %>%
  summarise(per_id = mean(grade_scores))

## plot

par(mfrow=c(1,2))
boxplot(Grades_byID$per_id, horizontal=TRUE, ylim=c(0,4), col=rgb(0.8,0.8,0,0.5), frame=T, main = "Figure 5:Final Exam Scores Distribution", cex.main = 0.7)
hist(Grades_byID$per_id,main = "Figure 6: Histogram of Final Exam Scores", cex.main = 0.7,
     breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4), xaxp = c(0,4,8), 
     xlab = "", ylab = "",xlim = c(0,4), col="darkmagenta")
abline(v = median(Grades_byID$per_id),col="darkslategray4", lwd = 5) # median line

sentence1 <- paste0(round(((sum(Grades_byID$per_id > mean(Grades_byID$per_id))/nrow(Grades_byID))*100),2), "%")
mediangrade <- round(median(Grades_byID$per_id),2)
min_score <- min(Grades_byID$per_id)

#sum(Grades_byID$per_id > 3 & Grades_byID$per_id < 3.5)/nrow(Grades_byID)
#sum(Grades_byID$per_id >= 3.5)
```
#### Q5: Describe or show the distribution of grades across all students.\
\quad After converting the letter grades to numbers, the distribution of grade scores was presented (figures 5 and 6). `r sentence1` of the class have scored higher than the average. Both figures 5 and 6 also present the median grade (`r mediangrade` or B+) as a vertical line. There are a couple outliers on the lower side of the grade distribution, demonstrated by the empty circles in figure 5. The minimum score in this class is `r min_score`. Both quarter one and quarter three are the sides of higher scores. According to figure 6, scores between 3 and 3.5 are the most frequent scores (38% of the class, in fact). The histogram is quite left-skewed. So, in summary, the students have done a great job on finals!\ 
```{r label = "Grades across programs", message=FALSE, warning=FALSE}

#GRADES ACROSS PROGRAMS

## Let's aggregate the grade data on program level
GPA_program <- Enrollment_full %>% 
  select(id, program, grade_scores) %>%
  distinct(.) 

GPA_program <- GPA_program %>% #17 rows
  group_by(program) %>%
  summarise(GPA = mean(grade_scores, na.rm = T)) 
 
## cross tab
GPA_program %>%
  arrange(.,desc(GPA)) %>% # order
  rename(Program = program) %>%
  kable(caption = "Average GPA across Programs") %>%
  kable_styling(latex_options = "HOLD_position", font_size = 7,
                bootstrap_options = c("condensed", "striped", "bordered")) %>%
  row_spec(0, font_size=10)
 
sentence1 <- GPA_program$program[GPA_program$GPA == max(GPA_program$GPA)]
```
#### Q6: Present a cross tab of average GPA by program.\
\quad I already have the information about GPA and the programs in one merged data set. One question that came to my mind was whether there is a conceptual difference between GPA and final scores, but since I did not have access to course credit information, I decided to assume that they represent the same thing. As demonstrated in table 4, `r sentence1`, or "Business Management", has the highest average scores across all 17 programs.

#### Q7: Present a visual representation of course grade distributions broken out by broad degree level (BA, BS, BM, BFA).\
\quad I assigned a tag to each record indicating which category of degree it belongs to in order to investigate the distribution of grades across the broad degree level. Assisted by figure 7, we can see that BA is the degree with the best grades, followed by BS, BM, and finally BFA. 25% of all grades in BA are A, followed by a 20% rate of A- for the `r term`.\ 

```{r label = "Grades across degree levels", message=FALSE, warning=FALSE, fig.align='center'}

# GRADES ACROSS DEGREE

## constructing a measure for broad degree level
Enrollment_full$degree_level <- ifelse(str_detect(Enrollment_full$degr, "BA"), "BA", ifelse(str_detect(Enrollment_full$degr, "BS"), "BS", ifelse(str_detect(Enrollment_full$degr, "BM"), "BM", ifelse(str_detect(Enrollment_full$degr, "BFA"), "BFA", NA))))

## data prep
Grade_degree <- Enrollment_full %>% # filtering out for the broad degree level
  select(id, degree_level, final_course_grade) %>%
  distinct(.) %>%
  group_by(degree_level, final_course_grade) %>%
  tally() %>%
  filter(!is.na(final_course_grade)) # NAs happen because not everyone on th enrollment data has a grade
## 47 records

## plot
ggballoonplot(Grade_degree, fill = "value",  shape = 21) +
  gradient_fill(c("blue", "white", "red")) + ggtitle("Figure 7: Grade distribution across the broad degree levels") +
  theme(plot.title = element_text(size = 9, face = "bold"), axis.text.y = element_text(angle = -90, vjust = 0.5, hjust=1, face = "bold"), axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold"))


```
```{r label = "week three undeclared students who declared major" , message=FALSE, warning=FALSE}

# UNDECLARED STUDENTS WHO DECLARED MAJOR

## choosing to use the original enrollment data
## getting a data set of all undeclared  students, getting their ids and extracting major info of those ids
Enrollment_UN <- Enrollment[str_detect(Enrollment$majr, "UN"),] # 929
Enrollment_both_dec <- Enrollment[Enrollment$id %in% Enrollment_UN$id,] #1161

## filtering for those UN students who have two records under their names
Enrollment_both_dec <- Enrollment_both_dec %>%
  select(-census) %>%
  distinct(.) %>% # this drops whoever stayed undeclared
  group_by(id) %>%
  mutate(un_major = n()) %>%
  filter(un_major == 2) #488

prop_dec_per_cohort <-paste0(round((length(unique(Enrollment_both_dec$id))/length(unique(Enrollment$id)))*100,2), "%") #2.503591
prop_dec_per_UN <- paste0(round((length(unique(Enrollment_both_dec$id))/nrow(Enrollment_UN))*100,2), "%") #26.2648

```

#### Q8: What is the proportion of week three undeclared students with a declared major at the end of term?\
\quad I identified all the students with undeclared majors, and then flagged the ones that have declared their major at the end of the term. `r prop_dec_per_cohort` of the cohort ended up figuring out what major they want to continue. They are `r prop_dec_per_UN` of all undeclared students.
\section{\large{`r paste0("Part", 4, ":", " Student-level data product")`} }
\quad As the last step of this analysis, I produced a student level data set. Some of the new measures I constructed are included in the final product, and some of the original attributes were dropped. 

```{r label = "outputting final product" , message=FALSE, warning=FALSE, results='hide'}

# FINAL TOUCH UPS OF FINAL PRODUCT

names(Enrollment_full) #45820 records
## reorder columns
Enrollment_final <- Enrollment_full[,c(1:3,14,8:10,20,13,11,18,12,19,15,17)]
names(Enrollment_final) <- tolower(names(Enrollment_final))
summary(Enrollment_final)

Enrollment_final$final_course_grade <- ifelse(is.na(Enrollment_final$final_course_grade), "No Grade", Enrollment_final$final_course_grade)
Enrollment_final$grade_scores <- ifelse(is.na(Enrollment_final$grade_scores), 0, Enrollment_final$grade_scores)
Enrollment_final$persist <- ifelse(is.na(Enrollment_final$persist), 0, Enrollment_final$persist)

# OUTPUTTING 

write.csv(Enrollment_final, paste0(DU_path_output, Product_name), row.names = F)

```


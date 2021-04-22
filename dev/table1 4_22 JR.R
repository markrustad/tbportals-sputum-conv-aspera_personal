#Packages (mostly bc when I was working in markdown)
library(tidyverse)
library(tableone)
library(arsenal)
library(knitr)
library(kableExtra)

#For markdown, need to load the folder to knit to doc
#load(file = "C:/Users/rokhsarjl/git/tbportals-sputum-conv-aspera/df_pos_init.Rdata")

#Create df with variables of interest
tab1_vars <- df_pos_init %>%
  select(age_of_onset, bmi, gender, country, case_definition, registration_date, 
         type_of_resistance_2, lung_localization, x_ray_exists, ct_exists, genomic_data_exists) %>%
  distinct() 

#Create labels for table
labels(tab1_vars)  <- c(type_of_resistance_2 = 'Type of Resistance',  registration_date = "Registration Date", age_of_onset = "Age of onset (yrs)", bmi = "BMI", gender = "Gender", country = "Country", 
                        case_definitionv = "Case definition", lung_localization = "Lung localization", x_ray_exists = "Radiographs in portal", ct_exists = "CT in portal", genomic_data_exists = "Genomic data in portal")

#Set controlf for table
mycontrols  <- tableby.control(test=FALSE, total=TRUE,
                               numeric.test="kwt", cat.test="chisq", #I don't know why we need this line but doesnt work without
                               digits = 1, digits.count = 0, digits.pct = 1, digits.n = 0,
                               numeric.stats=c("N", "median", "q1q3"),
                               cat.stats=c("countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))


#Create the table using tableby
tab1 <- tableby(type_of_resistance_2 ~ registration_date + age_of_onset + bmi + gender + country + case_definition + lung_localization + x_ray_exists + 
                  ct_exists + genomic_data_exists, data=tab1_vars, control = mycontrols)

#Check table in console
summary(tab1, text=TRUE)

#The example code to export the table to word doc:
# See item 21 in this link: https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html#modifying-the-look-feel-in-word-documents
write2word(tab1, "~/trash.doc", title="Table One")


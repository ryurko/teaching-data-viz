# PURPOSE: Begin exploring survey results with just basic info like how many
#          universities have a data viz class, the total number of courses that
#          we find, etc.

library(tidyverse)

# Load the survey results -------------------------------------------------

clean_survey_file <- read_csv("data/processed/survey_results.csv")

# How many schools have courses? ------------------------------------------

table("Type" = clean_survey_file$type,
      "Include a class?" = clean_survey_file$did_it_include_any_class)
# Include a class?
#   Type           FALSE TRUE
#   liberal_arts    34   16
#   university      26   78

# Huh, interesting - majority of the liberal arts do NOT feature a data viz class


# Create class level dataset ----------------------------------------------

# Most annoying part of working with this sheet, need to pivot to make it longer
# so that each row is a class

course_level_info <- clean_survey_file %>%
  dplyr::select(university, type, course_1_name:course_15_software) %>%
  dplyr::select(-contains("notes")) %>%
  pivot_longer(cols = course_1_name:course_15_software,
               names_to = c("class", ".value"),
               names_pattern = "course_(\\d+)_(.+)")
nrow(course_level_info) == (nrow(clean_survey_file) * 15)
# [1] TRUE

# Successfully pivoted using the above code

# Remove missing rows based on name:
course_level_info <- course_level_info %>%
  filter(!is.na(name))
# Results in 270 courses in total (makes sense)


# Basic exploration of class level data -----------------------------------

# Number of classes per school (excluding schools without data viz)
course_level_info %>%
  group_by(university) %>%
  count() %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1, closed = "left") +
  theme_bw() +
  labs(x = "Number of data visualization classes",
       y = "Number of schools")

# Course level?
table(course_level_info$level)  
# Both          Grad      Graduate      Undegrad     Undergrad Undergraduate 
# 63            99             2             1           103             2 
# Ugh - neeed to combine these...

course_level_info <- course_level_info %>%
  mutate(level = ifelse(str_detect(tolower(level), "und"),
                        "undergrad",
                        ifelse(str_detect(tolower(level),
                                          "both"),
                               "both", "grad")))
table(course_level_info$level)
# both      grad undergrad 
# 63       101       106
# That's surprisingly balanced...

# Can imagine making some word clouds of the course names and topics...

# What about the department?
length(unique(course_level_info$dept))
# [1] 163

# Okay that's way too many to initially look at... what are the types of depts
# that contain statistics or data science in them?

course_level_info <- course_level_info %>%
  mutate(is_stat_ds = as.numeric(str_detect(tolower(dept), "stat|data")))
course_level_info %>%
  filter(is_stat_ds == 1) %>%
  pull(dept) %>%
  unique()
# [1] "Data Science Institute"                                          
# [2] "Statistical Science, Information Science + Studies"              
# [3] "Statistical Science"                                             
# [4] "Data Science"                                                    
# [5] "Statistics and Data Science"                                     
# [6] "Data Analytics"                                                  
# [7] "Mathematics and Statistics, Computers and Information Management"
# [8] "Data Sciece"                                                     
# [9] "Statistics"                                                      
# [10] "Data Science and Analytics"                                      
# [11] "Data Science (Public Policy)"                                    
# [12] "Data Analytics for Science"                                      
# [13] "Applied Data Analytics and Visualization"                        
# [14] "Applied Statistics, Social Science, and Humanities"              
# [15] "Statistics and Operations Research"                              
# [16] "Environmental Data Science"                                      
# [17] "Data Science and Engineering"                                    
# [18] "Applied Data Science"                                            
# [19] "Computer Science, Data Science"                                  
# [20] "Journalism and Media Management, Data Science"                   
# [21] "Information Technology, Statistics"                              
# [22] "Data Analytics & Visualization"                                  
# [23] "Data Science, Informatics"                                       
# [24] "Data Science, Information Systems"                               
# [25] "Data for Political Research"     

# Huh... interesting mix

table(course_level_info$is_stat_ds)
#   0   1 
# 220  50 

# Yup... majority do not come from stats or data science depts, and that's 
# probably being generous given the stat/data tag


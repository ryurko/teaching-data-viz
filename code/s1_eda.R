# PURPOSE: Begin exploring survey results with just basic info like how many
#          universities have a data viz class, the total number of courses that
#          we find, etc.

library(tidyverse)

# Load the survey results -------------------------------------------------

clean_survey_file <- read_csv("data/processed/data-viz-survey.csv")

table(clean_survey_file$type)
# liberal_arts   university 
#           50          104 

length(unique(clean_survey_file$university))
# [1] 154


# Only consider the courses with accessible course catalogs ---------------

main_survey_results <- clean_survey_file |>
  filter(course_catalog_accesible)

# How many were we able to access?
table(main_survey_results$type)
# liberal_arts   university 
#           46           89 

# How many schools have courses? ------------------------------------------

table(main_survey_results$did_it_include_any_class)
# FALSE  TRUE 
#    41    94 

table("Type" = main_survey_results$type,
      "Include a class?" = main_survey_results$did_it_include_any_class)
#           Include a class?
# Type           FALSE TRUE
#   liberal_arts    30   16
#   university      11   78

# Huh, interesting - majority of the liberal arts do NOT feature a data viz class

# Create class level dataset ----------------------------------------------

# Most annoying part of working with this sheet, need to pivot to make it longer
# so that each row is a class

course_level_info <- main_survey_results |>
  dplyr::select(university, type, course_1_name:course_15_software) |>
  dplyr::select(-contains("notes")) |>
  pivot_longer(cols = course_1_name:course_15_software,
               names_to = c("class", ".value"),
               names_pattern = "course_(\\d+)_(.+)")
nrow(course_level_info) == (nrow(main_survey_results) * 15)
# [1] TRUE

# Successfully pivoted using the above code

# Remove missing rows based on name:
course_level_info <- course_level_info |>
  filter(!is.na(name))
# Results in 270 courses in total (makes sense)

# How many schools with classes?
length(unique(course_level_info$university))
# [1] 94
94 / 154
# [1] 0.6103896

# Basic exploration of class level data -----------------------------------

# Number of classes per school (excluding schools without data viz)
course_level_info |>
  group_by(university) |>
  count() |>
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1, closed = "left",
                 breaks = seq(0, 16, by = 1),
                 color = "white", fill = "darkblue") +
  theme_light() +
  labs(x = "Number of identified data visualization classes taught within a university/college",
       y = "Number of universities/colleges")

course_level_info |>
  group_by(university) |>
  count() |>
  arrange(desc(n))
#     university                          n
#     <chr>                           <int>
#   1 Georgetown University              15
#   2 New York University                14
#   3 Northeastern University            13
#   4 Carnegie Mellon University          8
#   5 University of Notre Dame            8
#   6 Northwestern University             7
#   7 Duke University                     6
#   8 Purdue University                   6
#   9 Stevens Institute of Technology     6
#  10 University of Minnesota             6
# Okay - should mention these for the paper, which stand out in terms of the 
# sheer number that are there...
  
# Course level?
table(course_level_info$level)  
# Both          Grad      Graduate      Undegrad     Undergrad Undergraduate 
# 63            99             2             1           103             2 
# Ugh - neeed to combine these...

course_level_info <- course_level_info |>
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
# that contain statistics in them?

course_level_info <- course_level_info |>
  mutate(is_stats = as.numeric(str_detect(tolower(dept), "stat")))
course_level_info |>
  filter(is_stats == 1) |>
  pull(dept) |>
  unique()
# [1] "Statistical Science, Information Science + Studies"              
# [2] "Statistical Science"                                             
# [3] "Statistics and Data Science"                                     
# [4] "Mathematics and Statistics, Computers and Information Management"
# [5] "Statistics"                                                      
# [6] "Applied Statistics, Social Science, and Humanities"              
# [7] "Statistics and Operations Research"                              
# [8] "Information Technology, Statistics" 

# Repeat for Data Science
course_level_info <- course_level_info |>
  mutate(is_ds = as.numeric(str_detect(tolower(dept), "data sci")))
course_level_info |>
  filter(is_ds == 1) |>
  pull(dept) |>
  unique()
# [1] "Data Science Institute"                        "Data Science"                                 
# [3] "Statistics and Data Science"                   "Data Sciece"                                  
# [5] "Data Science and Analytics"                    "Data Science (Public Policy)"                 
# [7] "Environmental Data Science"                    "Data Science and Engineering"                 
# [9] "Applied Data Science"                          "Computer Science, Data Science"               
# [11] "Journalism and Media Management, Data Science" "Data Science, Informatics"                    
# [13] "Data Science, Information Systems" 

# Spot a mispelling there...

# Okay - so how many data viz classes are from stats depts?
table(course_level_info$is_stats)
#   0   1 
# 252  18
table(course_level_info$is_stats) / nrow(course_level_info)
#          0          1 
# 0.93333333 0.06666667 

# For data science?
table(course_level_info$is_ds)
#   0   1 
# 243  27 
table(course_level_info$is_ds) / nrow(course_level_info)
#   0   1 
# 0.9 0.1 

# Split by level:
course_level_info |>
  group_by(level, is_stats) |>
  summarize(n_classes = n(),
            .groups = "drop") |>
  group_by(level) |>
  mutate(total_classes = sum(n_classes)) |>
  ungroup() |>
  mutate(frac_classes = n_classes / total_classes)
#     level     is_stats n_classes total_classes frac_classes
#     <chr>        <dbl>     <int>         <int>        <dbl>
#   1 both             0        61            63       0.968 
#   2 both             1         2            63       0.0317
#   3 grad             0        96           101       0.950 
#   4 grad             1         5           101       0.0495
#   5 undergrad        0        95           106       0.896 
#   6 undergrad        1        11           106       0.104 

# Okay nice - pretty consistent here...

course_level_info |>
  group_by(level, is_stats) |>
  summarize(n_classes = n(),
            .groups = "drop") |>
  mutate(level = fct_relevel(level, "undergrad", "grad", "both"),
         level = fct_recode(level, Both = "both", 
                            `Undergraduate-only` = "undergrad",
                            `Graduate-only` = "grad")) |>
  ggplot(aes(x = level, y = n_classes, fill = as.factor(is_stats))) +
  geom_bar(stat = "identity", position = "stack") +
  ggthemes::scale_fill_colorblind(labels = c("No", "Yes")) +
  labs(x = "Student-level", y = "Number of classes",
       fill = "Taught by Statistics department?") +
  theme_light() +
  theme(legend.position = "bottom")


course_level_info |>
  group_by(level, is_ds) |>
  summarize(n_classes = n(),
            .groups = "drop") |>
  group_by(level) |>
  mutate(total_classes = sum(n_classes)) |>
  ungroup() |>
  mutate(frac_classes = n_classes / total_classes)
# # A tibble: 6 × 5
#     level     is_ds n_classes total_classes frac_classes
#     <chr>     <dbl>     <int>         <int>        <dbl>
#   1 both          0        57            63       0.905 
#   2 both          1         6            63       0.0952
#   3 grad          0        90           101       0.891 
#   4 grad          1        11           101       0.109 
#   5 undergrad     0        96           106       0.906 
#   6 undergrad     1        10           106       0.0943

course_level_info |>
  group_by(level, is_ds) |>
  summarize(n_classes = n(),
            .groups = "drop") |>
  mutate(level = fct_relevel(level, "undergrad", "grad", "both"),
         level = fct_recode(level, Both = "both", 
                            `Undergraduate-only` = "undergrad",
                            `Graduate-only` = "grad")) |>
  ggplot(aes(x = level, y = n_classes, fill = as.factor(is_ds))) +
  geom_bar(stat = "identity", position = "stack") +
  ggthemes::scale_fill_colorblind(labels = c("No", "Yes")) +
  labs(x = "Student-level", y = "Number of classes",
       fill = "Taught by Data Science department?") +
  theme_light() +
  theme(legend.position = "bottom")

# Yup... majority do not come from stats or data science depts, and that's 
# probably being generous given the stat/data tag

# Frequency of topics we discuss ------------------------------------------

# How many classes had topics?
course_level_info |>
  filter(topic_list != "None", !is.na(topic_list)) |>
  nrow()
# [1] 142 - okay so a good number


# Set-up a vector of topics 
stat_topics <- c("hypothesis testing", "confidence intervals",
                 "statistical modeling")

# How many courses cover these topics?
courses_by_topics <- map(stat_topics,
                         function(topic) {
                           
                           # Just return the courses with this topic:
                           course_level_info |>
                             mutate(topic_list = tolower(topic_list)) |>
                             filter(str_detect(topic_list, str_c(topic)))
                           
                         })
names(courses_by_topics) <- stat_topics
sapply(courses_by_topics, function(x) x$university)
# $`hypothesis testing`
# [1] "Duke University"            "Columbia University"        "Carnegie Mellon University"
# [4] "Brigham Young University"  
# 
# $`confidence intervals`
# [1] "Carnegie Mellon University" "Colby College"             
# 
# $`statistical modeling`
# [1] "Duke University"            "Brown University"           "Georgetown University"     
# [4] "Georgetown University"      "Carnegie Mellon University" "University of Minnesota"   
# [7] "Colby College"        
# Okay - so no visualization needed here... thsi is really really limited then...
# Colby is Jerzy's class so that would explain the similarity there, and Duke 
# is not surprising at all

# Print out the depts:
lapply(courses_by_topics, function(x) dplyr::select(x, university, dept))
# $`hypothesis testing`
# # A tibble: 4 × 2
#     university                 dept                                              
#     <chr>                      <chr>                                             
#   1 Duke University            Statistical Science, Information Science + Studies
#   2 Columbia University        Computer Science                                  
#   3 Carnegie Mellon University Statistics and Data Science                       
#   4 Brigham Young University   Political Science                                 
# 
# $`confidence intervals`
# # A tibble: 2 × 2
#     university                 dept                       
#     <chr>                      <chr>                      
#   1 Carnegie Mellon University Statistics and Data Science
#   2 Colby College              Statistics                 
# 
# $`statistical modeling`
# # A tibble: 7 × 2
# university                 dept                                              
# <chr>                      <chr>                                             
#   1 Duke University            Statistical Science, Information Science + Studies
#   2 Brown University           Computer Science                                  
#   3 Georgetown University      Data Science and Analytics                        
#   4 Georgetown University      Learning, Design, and Technology                  
#   5 Carnegie Mellon University Statistics and Data Science                       
#   6 University of Minnesota    Information and Decision Science                  
#   7 Colby College              Statistics  

# huh - not what I expected, would've thought that this would be entirely stats


# Create word clouds of departments, class names, and topics --------------

# Start with departments
library(tidytext)
data(stop_words)

course_dept_text <- course_level_info |>
  dplyr::select(university, class, dept) |>
  unnest_tokens(word, dept) |>
  filter(!(word %in% stop_words$word)) |>
  group_by(word) |>
  summarize(freq = n(), .groups = "drop")

library(wordcloud)
wordcloud(words = course_dept_text$word,
          freq = course_dept_text$freq, 
          random.order = FALSE,
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"))
# That makes sense.... and is a good one to potentially use

# Repeat for course names:
course_name_text <- course_level_info |>
  dplyr::select(university, class, name) |>
  unnest_tokens(word, name) |>
  filter(!(word %in% stop_words$word)) |>
  group_by(word) |>
  summarize(freq = n(), .groups = "drop")

wordcloud(words = course_name_text$word,
          freq = course_name_text$freq, 
          random.order = FALSE,
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"))
# Well that's not interesting...

# And for topics:
course_topics_text <- course_level_info |>
  filter(topic_list != "None", # note - none is a stop word apparently..
         !is.na(topic_list)) |>
  dplyr::select(university, class, topic_list) |>
  unnest_tokens(word, topic_list) |>
  filter(!(word %in% stop_words$word)) |>
  group_by(word) |>
  summarize(freq = n(), .groups = "drop")

wordcloud(words = course_topics_text$word,
          freq = course_topics_text$freq, 
          random.order = FALSE,
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"))
# Okay so interactive graphics is really popular...


# Pretty sure I have what I need to fill out the rest of that section, hitting on 
# Zach's main points...

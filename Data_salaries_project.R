#import tidyverse
install.packages("tidyverse")
library(tidyverse)



#load data into dataframe
data_jobs <- read.csv("C:\\Users\\demet\\OneDrive - Georgia State University\\Documents\\R_programming\\Google_data_analytics_cert\\Capstone_project\\ds_salaries.csv")

# use rm(variable name) to delete variable



############################clean up data###########################################


install.packages("here")
library(here)

#skimr summarizes data easily and makes skimmming it easier
install.packages("skimr")
library(skimr)

# the janitor package has functions for cleaning data
install.packages("janitor")
library(janitor)


#look at a preview of data frame and get its meta 
str(data_jobs)
head(data_jobs)
skim_without_charts(data_jobs)
glimpse(data_jobs)
#clean names of columns
clean_names(data_jobs)

#select distinct values for certain columns
select(data_jobs,work_year) %>% 
  distinct()

select(data_jobs,job_title) %>% 
  distinct()

select(data_jobs,experience_level) %>% 
  distinct()

select(data_jobs,employee_residence) %>% 
  distinct()

select(data_jobs,remote_ratio) %>% 
  distinct()

select(data_jobs,company_location) %>% 
  distinct()

select(data_jobs,company_size) %>% 
  distinct()

#change values in columns company size and experience level
  df_jobs_clean <- mutate(data_jobs,
                   experience_level = str_replace_all(experience_level,c('SE' = 'senior', 'EX' = 'executive', 'MI' = 'middle', 'EN' = "entry")), 
                   company_size = str_replace_all(company_size, c('L' = 'large', 'S' = 'small','M' = 'middle')))
  
  
  # mutate(data_jobs,
  #       experience_level = recode(experience_level,'SE' = 'senior', 'EX' = 'executive', 'MI' = 'middle', 'EN' = "entry"), 
  #        company_size = recode(company_size, 'L' = 'large', 'S' = 'small','M' = 'middle'))


#filter for data where the company is located in the U.S.
df_jobs_clean_us <- filter(df_jobs_clean,company_location == "US")

#create job categories based on job_title 

df_jobs_organized_us <- df_jobs_clean_us %>% 
  mutate(job_type = case_when(
        #jobs in data engineering category
       str_like(job_title,"%data engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%business intelligence engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%data quality analyst%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%ETL%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Architect%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Operations Engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data modeler%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Strateg%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Manage%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Head of Data%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Database Engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Infrastructure Engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Lead%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data DevOps Engineer%",ignore_case = TRUE) ~ "Data Engineer",
       str_like(job_title,"%Data Specialist%",ignore_case = TRUE) ~ "Data Engineer",
        #jobs in data science category
       str_like(job_title,"%Data Scien%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Computer%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Machine Learn%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%AI%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Artificial%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Scien%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%ML%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Data Scien%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Data Scien%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Autonomous%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%NLP%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Research%",ignore_case = TRUE) ~ "Data Scientist",
       str_like(job_title,"%Learn%",ignore_case = TRUE) ~ "Data Scientist",
        #jobs in data analyst category
      str_like(job_title,"%Data Analyst%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%Data Analyst%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%Data Analyst%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%Analytics%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%BI%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%Insight%",ignore_case = TRUE) ~ "Data Analyst",
      str_like(job_title,"%Data Operations Analyst%",ignore_case = TRUE) ~ "Data Analyst"
         
  )) 


view(select(filter(df_jobs_organized_us,job_type == "")))

distinct(df_jobs_organized_us,job_type)
filter(df_jobs_organized_us,is.na(job_type))
#############################Explore data and Visualize########################################


#find the average salary based on job_type and work_year
df_group_salary_job_year <- df_jobs_organized_us %>% 
  select(-job_title,-salary_currency) %>% 
    group_by(job_type,work_year) %>% 
    summarise(avg_salary = mean(salary_in_usd))%>%
    arrange(work_year)

#visualize it
options(scipen = 999)
ggplot(data = df_group_salary_job_year) +
  geom_col(mapping = aes(x = job_type,y = avg_salary,fill = job_type))+
  geom_text(aes(x = job_type,y = avg_salary,label = as.integer(avg_salary)), vjust = 0.2)+
  labs(title = "Average Salary of data professional jobs by job category",caption = "Data collected from Kaggle dataset") +
  facet_wrap(~work_year)


# #didnt actually need this
# #find all the jobs salary, job_type, and work year
# df_salary_job_year <- df_jobs_organized_us %>% 
#   select(job_type, work_year, salary_in_usd) %>%
#   arrange(work_year)
# #make visual for it
# options(scipen = 999)
# ggplot(data = df_salary_job_year) +
#   geom_col(mapping = aes(x = job_type,y = salary_in_usd))+
#   facet_wrap(~work_year)
  
  
# #dont need this either
# #find salary for each experience level and work_year
# df_salary_experience_ <- df_jobs_organized_us %>% 
#   select(experience_level,work_year, salary_in_usd) %>% 
#   arrange(work_year)

#find avg salary per year and experience level
df_group_experience_salary_yearly <- df_jobs_organized_us %>% 
  select(experience_level,work_year,job_type,salary_in_usd) %>% 
  group_by(experience_level,work_year) %>% 
  summarise(avg_salary = mean(salary_in_usd))%>%
  arrange(work_year)
  
options(scipen = 999)
ggplot(data = df_group_experience_salary_yearly) +
  geom_col(mapping = aes(x = experience_level,y = avg_salary,fill = experience_level))+
  geom_text(aes(x = experience_level,y = avg_salary,label = as.integer(avg_salary)), vjust = -0.2)+
  labs(title = "Average Salary of data professional jobs by experience level",caption = "Data collected from Kaggle dataset") +
  facet_wrap(~work_year)     


#find the number of jobs per job_type and year
df_group_jobs_year_count <- group_by(select(df_jobs_organized_us,job_type,work_year),job_type,work_year) %>% 
  summarise(number_of_jobs = n())%>%
  arrange(work_year)

#visualize it
options(scipen = 999)
ggplot(data = df_group_jobs_year_count) +
  geom_col(mapping = aes(x = work_year,y = number_of_jobs,fill = work_year))+
  geom_text(aes(x = work_year,y = number_of_jobs,label = number_of_jobs), vjust = -0.2)+
  facet_wrap(~job_type)     


#find the number of jobs per experience level,job_type, and year
df_group_experience_jobs_count <- df_jobs_organized_us %>% 
  select(work_year,job_type,experience_level)%>%
  group_by(job_type,experience_level,work_year) %>% 
  summarise(number_of_jobs = n())%>%
  arrange(work_year)

#visualize it
options(scipen = 999)
ggplot(data = df_group_experience_jobs_count) +
  geom_col(mapping = aes(x = experience_level,y = number_of_jobs,fill = job_type))+
  labs(title = "Number of data professional jobs by experience level and job category",caption = "Data collected from Kaggle dataset") +
  facet_wrap(~work_year)

#find the number of jobs per experience level and year
df_group_experience_year_count <- df_jobs_organized_us %>% 
  select(work_year,experience_level)%>%
  group_by(experience_level,work_year) %>% 
  summarise(number_of_jobs = n())%>%
  arrange(work_year)

#visualize it
options(scipen = 999)
ggplot(data = df_group_experience_year_count) +
  geom_col(mapping = aes(x = experience_level,y = number_of_jobs,fill = experience_level))+
  geom_text(aes(x = experience_level,y = number_of_jobs,label = number_of_jobs), vjust = 0.1)+
  labs(title = "Number of Data professional jobs by experience level",caption = "Data collected from Kaggle dataset") +
  facet_wrap(~work_year)

#didnt need this
#find the number of jobs per experience level(only entry level), year, and job_type
# df_group_entry_jobs_count <- df_jobs_organized_us %>% 
#   select(work_year,job_type,experience_level)%>%
#   filter(experience_level == "entry") %>% 
#   group_by(job_type,experience_level,work_year) %>% 
#   summarise(number_of_jobs = n())%>%
#   arrange(work_year)
# 

#find the number of jobs by company size and year
df_jobs_company_size_count <- df_jobs_organized_us %>% 
  select(work_year,job_type,company_size)%>%
  group_by(work_year,company_size) %>% 
  summarise(number_of_jobs = n())%>%
  arrange(work_year)

options(scipen = 999)
ggplot(data = df_jobs_company_size_count) +
  geom_col(mapping = aes(x = company_size,y = number_of_jobs,fill = company_size))+
  geom_text(title="Number of Jobs by Company Size",aes(x = company_size,y = number_of_jobs,label = number_of_jobs), vjust = 0.1)+
  labs(title = "Number of jobs by company size",caption = "Data collected from Kaggle dataset") +
  facet_wrap(~work_year) 

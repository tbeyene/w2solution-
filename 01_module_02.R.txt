# BUAN 5112 - Module 2 assignment

library(tidyverse)
library(here)
library(haven)
library(forcats)

# Load data
nfhs <- read_dta(here("raw_data", "IAHR52FL.dta"))


# Q1: For which level analyses are this data tidy?
# As long as we only do household level analysis.
# If we want to do individual level analysis, the data are not tidy.

# Text:
# For ease, we are going to split the data into three different parts.
# We can then manipulate each part and put them back together again.

# Q2: Create a new data frame that selects all variables between the household id and hv208 (both included) plus the wealth index.
# How many variables do you have in your new data frame?

hh <- select(nfhs, hhid:hv208, hv270)

# Q3: Next create a data frame that holds the household id, the individuals' line numbers, and their education in single years.
  
educ <- select(nfhs, hhid, 
               starts_with("hvidx"),
               contains("hv108"))

# Q4: We also need some of the anthropometric data for females (and the household id, of course).
# The variables we need follow a pattern like "haX_YZ", where X, Y, and Z are single digits, 
# and X can take the value from 0 to 6 (both included).
# There are a number of different ways to do this. 

female <- select(nfhs, hhid,
                 matches("ha\\d_\\d\\d")
)


# Q5: Male anthropometric data

# Finally, the anthropometric data for males (and the household id, of course).
# The variables we need all follow a pattern like "hbX_YZ", where X, Y, and Z
# are single digits and X can take the value from 0 to 6 (both included).
#
# Try a different method than for females.
#
# How many variables do you have in the male data frame?

male <- select(nfhs, hhid,
               num_range("hb0_", 0:20, width = 2),
               num_range("hb1_", 0:20, width = 2),
               num_range("hb2_", 0:20, width = 2),
               num_range("hb3_", 0:20, width = 2),
               num_range("hb4_", 0:20, width = 2),
               num_range("hb5_", 0:20, width = 2),
               num_range("hb6_", 0:20, width = 2)
)

male <- select(nfhs, hhid,
               matches("hb[0-6]_\\d\\d")
               )

# Q6 ----
# The problem with the three data frames we have created is that they are
# still not tidy and we, therefore, cannot merge them. We will start with the
# education data frame. The problem here is that what we need both the "hhid"
# variable and the roster number to merge on, but the roster numbers are in the
# cells. Furthermore, there are lots of cells with NAs in them that we need to
# get rid of. All, in all, a fun logic puzzle!
# 
# The end goal here is to get the roster line number and the years of education
# of each individual on their own separate lines. To achieve this you need use
# the number that the original variables include (for example, hvidx_04 and
# hv108_04 are the same person). There are likely multiple different ways of
# doing this, but the one I used involved first making one long list (using
# gather), splitting the variable (using separate), and finally getting the
# roster number and education on the same observation line (using spread). My
# recommendation is that you do this in steps, rather than try to pipe
# everything together from the beginning.
# 
# Once you have done, filter out the lines that have missing in them. You should
# also drop any variable(s) that you will not need anymore and rename the
# variables to make it easier to understand what they contain. When all is done,
# you should have a new data frame with 3 variables (hhid, the roster number,
# and the year of education). How many observations are in this data frame?

educ <- educ %>%
  gather(variable_name, var_value, -hhid) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  filter(!is.na(hvidx)) %>% 
  select(-number) %>% 
  rename(roster_id = hvidx, educ = hv108) # Something to merge on!

educ %>%
  mutate(
    educ = case_when(
      educ <= 90 ~ educ,
      educ  > 90 ~ NA_real_
    ) 
  ) %>% 
  filter(!is.na(educ)) %>% 
  write_rds(here("data", "educ.rds"))

# Q7 ----

# Next we need to do exactly the same thing for the female sample. The main
# difference is that we will end up with three variables (age, weight, and
# height). In addition, you should create a factor variable for female, which
# obviously is true for all observations in this data set. How many female
# observations do we have?

female <- nfhs %>%
  select(hhid, matches("ha\\d_\\d\\d")) %>% 
  gather(variable_name, var_value, -hhid) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  filter(!is.na(ha0)) %>% 
  select(-number, -ha4, -ha5, -ha6) %>% 
  rename(roster_id = ha0, age = ha1, weight = ha2, height = ha3) %>% 
  mutate(female = TRUE)

# Q8 ----

# And, finally, the same for the male sample. Remember to add the female
# dummy/factor variable, but make it false. How many observations do you have?

male <- nfhs %>%
  select(hhid, matches("hb[0-6]_\\d\\d")) %>% 
  gather(variable_name, var_value, -hhid) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  filter(!is.na(hb0)) %>% 
  select(-number, -hb4, -hb5, -hb6) %>% 
  rename(roster_id = hb0, age = hb1, weight = hb2, height = hb3) %>% 
  mutate(female = FALSE)

# Q9 ----

# Now that we have prepared all the different data frames, it is time to combine
# them. You will first need to stack the female and male observations (hint:
# look at "bind_rows"). Then merge in the education and household data frames
# that you made first. We are only interested in the respondents for whom we
# have information on age, weight, and height (in other words, those in the male
# and female data frames). Once you are all done you should have 228,426
# observations and 54 variables. In that data frame, the median age for females
# is [28] and the median age for males is [30].

base <- bind_rows(female, male) %>%  # Combine male and female
  inner_join(educ) %>%  # could also use left here
  inner_join(hh) # note R figures out what to merge on

base %>% group_by(female) %>% summarize(median(age))








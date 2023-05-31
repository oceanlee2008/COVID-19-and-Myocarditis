library(ggplot2)
library (readxl) 
library(tidyverse)
# ip2 <- read_excel("lung_cancer.xlsx")
ip <- read_excel("new_sublung.xlsx")
# dat <- read_excel("sublung.xlsx",sheet="xian")


library(gtsummary)
# make dataset with a few variables to summarize
# summarize the data with our package
trial2 <- ip %>% select(SEX,	AGE,	TP,	ALB,	hs-CRP,	CRP,	LHD,	CK,	HB,	MB,	hs-cTn,	PLT,	LYMP,	label)
tbl_summary(
  trial2,
  by = PATIENTSEX, # split table by group
  missing = "no" # don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  
  
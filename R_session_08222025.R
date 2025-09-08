# Libraries
if(!require('pacman')) install.packages('pacman')
pacman::p_load('haven',
               'readr',
               'tidyverse',
               'usethis',
               'lubridate',
               'xportr',
               'here',
               'xlsx',
               'data.table')

# data loading
## xpt files

ae <- read_xpt("data/ae.xpt")
dm <- read_xpt("data/dm.xpt")
vs <- read_xpt("data/vs.xpt")

xportr_write(ae, "ae_xport.xpt")

# Checking  classes
ch <-c("A", 1, "B")
ch
class(ch)

# subsetting data
head(dm, 10) #----------first 10 rows


dm[dm$AGE>50,] #-------subsetting based on the condition
dm[seq(from = 1, to = 19, by = 2),c("STUDYID", "DOMAIN", "AGE", "RFSTDTC") ]


# formatting
sev <- c('MILD', 'MODERATE', 'SEVERE')
# nested if else
result <- if_else(sev == 'MILD', 'Low severity',
                  if_else(sev == 'MODERATE', 'Medium severity',
                          if_else(sev == 'SEVERE', 'High severity', 'Unknown')))

print(result)


# glimpse function
dm %>% glimpse()

# dplyr functions
l1_demo <- dm %>% 
  select(USUBJID,AGE, SEX) %>% 
  filter(SEX == 'F') %>% 
  filter(AGE > 50) %>% 
  mutate(AGE2 = AGE*2) %>%
  head(5)
  

# dplyr case when
exl_case <- dm %>% mutate(
  AGECAT = case_when(
    is.na(AGE) ~ "Missing",
    AGE < 40 ~ "<40",
    AGE < 65 ~ "40-64",
    TRUE ~ "65+"
  ),
  IS_MALE = SEX == 'M'
) %>% 
  select(USUBJID, AGE, AGECAT, IS_MALE)

exl_case

# dplyr summarise and group by
exl_summary <- dm %>% 
  group_by(SEX, ETHNIC) %>% 
  summarise(mean_age=mean(AGE, na.rm=TRUE), n=n(), .groups = "drop")
exl_summary

# dplyr transmute
exl_transmute <- dm %>% transmute(AGE3 = AGE * 3)  

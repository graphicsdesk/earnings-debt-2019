library(tidyverse)
library(jsonlite)

raw_data <- readxl::read_excel("Most-Recent-Field-Data-Elements.xlsx")
data <- raw_data %>% 
  filter(
    MD_EARN_WNE != "PrivacySuppressed" &
      DEBTMEDIAN != "PrivacySuppressed"
  ) %>% 
  mutate(
    COUNT = as.numeric(COUNT),
    MD_EARN_WNE = as.numeric(MD_EARN_WNE),
    DEBTMEDIAN = as.numeric(DEBTMEDIAN)
  )

BADG <- "Bachelor\u0092s Degree"
MADG <- "Master's Degree"

get_cred_data <- function(cred) {  
  fields <- data %>% 
    filter(INSTNM == "Columbia University in the City of New York") %>% 
    filter(CREDDESC == cred) %>% 
    pull(CIPCODE) %>% 
    unique()
  data %>% 
    filter(CREDDESC == cred) %>%
    filter(CIPCODE %in% fields)
    # mutate(color = ifelse(INSTNM %in% peers, INSTNM, "other")) color setting
}

rbind(get_cred_data(BADG), get_cred_data(MADG)) %>% 
  select(INSTNM, CIPDESC, CREDDESC, DEBTMEDIAN, MD_EARN_WNE) %>%
  mutate(CREDDESC = if_else(CREDDESC == BADG, "Bachelor", "Master")) %>% 
  mutate(CIPDESC = stringr::str_remove(CIPDESC, "\\.$")) %>% 
  rename(institution = INSTNM, field = CIPDESC, cred = CREDDESC, debt = DEBTMEDIAN, earnings = MD_EARN_WNE) %>% 
  write_json('../../earnings/data/data.json')

# Investigate the green-deal-orb lists that were scraped from the website

library(tidyverse)

assessor <- read_tsv("./lists/green-deal-orb/assessor.tsv")
provider <- read_tsv("./lists/green-deal-orb/provider.tsv")
installer <- read_tsv("./lists/green-deal-orb/installer.tsv")

participant <- bind_rows(assessor, provider, installer)

# Only `Provider/Cert. ID` is unique
map(participant, ~ length(unique(.x)))

# 354 websites are associated with more than one ID
participant %>%
  distinct(Website, `Provider/Cert. ID`) %>%
  count(Website) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# The shared webites show up a lot of varieties of spelling of company names.
participant %>%
  filter(!is.na(Website)) %>%
  distinct(Website, `Provider/Cert. ID`) %>%
  count(Website) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  select(-n) %>%
  left_join(participant, by = "Website") %>%
  select(Website, Company, `Company Type`, `Provider/Cert. ID`) %>%
  as.data.frame

# 374 phone numbers are associated with more than one ID
participant %>%
  distinct(`Phone Number`, `Provider/Cert. ID`) %>%
  count(`Phone Number`) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# The shared phone numbers show up a lot of varieties of spelling of company names.
participant %>%
  filter(!is.na(`Phone Number`)) %>%
  distinct(`Phone Number`, `Provider/Cert. ID`) %>%
  count(`Phone Number`) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  select(-n) %>%
  left_join(participant, by = "Phone Number") %>%
  select(`Phone Number`, Company, `Company Type`, `Provider/Cert. ID`) %>%
  as.data.frame

# TODO:
# * How many websites return with 404?
# * Validate phoone numbers with
# https://github.com/daviddrysdale/python-phonenumbers or
# https://github.com/googlei18n/libphonenumber (no R port yet).

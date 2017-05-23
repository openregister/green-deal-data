# Extract unique lists of qualifying improvements from the exports of
# asessors/installers/providers, for mapping to the qualifying improvements
# given in legislation.

library(tidyverse)
library(stringr)

assessor <- read_csv("./lists/green-deal-orb/Assessors_2017_05_12.csv") %>%
  rename(ID = `Certification ID`) %>%
  mutate(type = "assessor")
provider <- read_csv("./lists/green-deal-orb/Providers_2017_05_12.csv") %>%
  rename(ID = `Provider ID`) %>%
  mutate(type = "provider")
installer <- read_csv("./lists/green-deal-orb/Installers_2017_05_12.csv") %>%
  rename(ID = `Certification ID`) %>%
  mutate(type = "provider")

# Combine, and unnest the `Measures offered` column
bind_rows(assessor, provider, installer) %>%
  mutate(`Measures offered` = str_split(`Measures offered`, ",(?! )")) %>%
  unnest %>%
  distinct(`Measures offered`) %>%
  arrange(`Measures offered`) %>%
  rename(`improvement` = `Measures offered`) %>%
  write_tsv("lists/green-deal-orb/improvements.tsv")

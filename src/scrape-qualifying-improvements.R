# Scrape legislationg for the lists of qualifying improvements that
# installers/assessors/etc. are certified for.

library(tidyverse)
library(rvest)
library(stringr)

qualifying_energy_improvements_url <- "http://www.legislation.gov.uk/uksi/2012/2105/schedule/made"
specified_energy_efficiency_improvements_url <- "http://www.legislation.gov.uk/uksi/2012/2106/schedule/made"

qualifying_energy_improvements_url %>%
  read_html() %>%
  html_nodes(".LegP3Text") %>%
  html_text() %>%
  str_sub(end = -2L) %>%
  data_frame(`qualifying-energy-improvement` = .) %>%
  # Add an item from the amendment http://www.legislation.gov.uk/uksi/2014/2020/article/2/made
  bind_rows(data_frame(`qualifying-energy-improvement` = "circulator pumps")) %>%
  arrange(`qualifying-energy-improvement`) %>%
  write_tsv("./lists/legislation/qualifying-energy-improvement.tsv")

specified_energy_efficiency_improvements_url %>%
  read_html() %>%
  html_nodes(".LegP3Text") %>%
  html_text() %>%
  str_sub(end = -2L) %>%
  data_frame(`specified-energy-efficiency-improvement` = .) %>%
  arrange(`specified-energy-efficiency-improvement`) %>%
  write_tsv("./lists/legislation/specified-energy-efficiency-improvement.tsv")

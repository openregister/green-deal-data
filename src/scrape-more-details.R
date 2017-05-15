# Scrape the 'more details' pages of the Green Deal Oversight & Registration
# Body
#
# The initial results tables are scraped by /src/scrape-green-deal-orb.R.  This
# script picks up where that one left off.

library(tidyverse)
library(rvest)
library(xml2)

assessor <- read_tsv("./lists/green-deal-orb/assessor.tsv")
provider <- read_tsv("./lists/green-deal-orb/provider.tsv")
installer <- read_tsv("./lists/green-deal-orb/installer.tsv")

participant <- bind_rows(assessor, provider, installer)

base_url <- "http://gdorb.decc.gov.uk"
participant$more_info <- paste0(base_url, participant$more_info)

session <- html_session(base_url)

detail <-
  session %>%
  jump_to(url = participant$more_info[2]) %>%
  read_html()
detail_nodes <-
  detail %>%
  html_node(".mcsColumnsTwoOne") %>%
  xml_children()
status_change <-
  detail %>%
  html_table()
details <-
  data_frame(level = map_chr(detail_nodes, xml_name),
             text = map_chr(detail_nodes, xml_text)) %>%
  as.data.frame()
details[1, ]
details[2, ]
details[3, ]
details[4, ]
details[5, ]
details[6, ]
details[7, ]
details[8, ]
details[9, ]
details[10, ]
details[11, ]
details[12, ]
details[13, ]
details[14, ]
details[15, ]

details %>%
  filter(level != "hr")

# TODO:
# * extract Status History table

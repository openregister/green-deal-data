# Green Deal Oversight & Registration Body
#
# There is an 'export' function on the website, controlled by terms and
# conditions.
# http://gdorb.decc.gov.uk/green-deal-participant-register/participant-export
#
# Scrape http://gdorb.decc.gov.uk/green-deal-participant-register for:
# * assessors
# * providers
# * installers

# Scrape http://gdorb.decc.gov.uk/certification-body-search for:
# * certified-bodies

# This script only scrapes the initial results table.  The 'more details' pages
# are scraped by /src/scrape-more-details.R

library(tidyverse)
library(rvest)
library(stringr)

# Function to click the 'next' button in the results table, and scrape the
# result.  This wouldn't be necessary if the server responded correctly to
# 'results per page = 5000', but it only returns ~65 results that way (it works
# interactively in the browser though).
# Parameter n_urls is the number of urls in each row -- we only want one from
# each row, so we skip from 1 to 1+n_urls to 1+n_urls+n_urls etc.
# Warning!  This function modifies 'results'
next_results <- function(css, n_urls) {
  # Get the current results
  out <- html_table(results)[[1]]
  # Get the 'More info' url
  out$more_info <-
    results %>%
    html_nodes(css) %>%
    html_attr("href") %>%
    .[seq(1, length(.), by = n_urls)]
  # Advance to the next page
  result <- NULL
  attempt <- 0
  while(is.null(result) && attempt < 10) {
    attempt <- attempt + 1
    try(result <- follow_link(results, css = 'a[title="Next page"]'))
    Sys.sleep(.25)
  }
  if (!is.null(result)) {results <<- result}
  # Return the result set
  out
}

################################################################################

# assessors, providers, installers

# We need an rvest session so that the website remembers who we are, and we can
# use submit_form().
session <- html_session("http://gdorb.decc.gov.uk/find-a-green-deal-supplier")

# Submit the form with no constraints, to receive all values, including
# suspended/withdrawn
results <-
  session %>%
  html_form() %>%
  keep(~ .x$name == "search_form") %>%
  .[[1]] %>%
  submit_form(session, .)
html_table(results)

# We should have 4195 results as of 2017-05-11
# We should have 4198 results as of 2017-05-12
results_n <- results %>%
  html_node(".squiloople-pagination-information") %>%
  html_text() %>%
  str_extract("[0-9]+$") %>%
  parse_number
results_n

# The next step is complicated by the fact that the form refuses to return more
# than 65 results, even if the url is hacked.

# Create a list for each page of results
pages <- ceiling(results_n / 10)
all_results <- vector("list", pages)

# Get all pages (takes a few minutes, expect errors on the final page)
for(i in seq_len(pages)) {
  cat("page: ", i, "\n")
  all_results[[i]] <- next_results(css = ".mcsResultsTable a", n_urls = 3)
}

combined <- all_results %>%
  bind_rows() %>%
  select(-6) %>%
  as_tibble %>%
  distinct # The last few rows might have been duplicated by the website

assessor <- filter(combined, `Company Type` == "Assessor")
provider <- filter(combined, `Company Type` == "Provider")
installer <- filter(combined, `Company Type` == "Installer")

write_tsv(assessor, "./lists/green-deal-orb/assessor.tsv")
write_tsv(provider, "./lists/green-deal-orb/provider.tsv")
write_tsv(installer, "./lists/green-deal-orb/installer.tsv")

################################################################################

# Certified bodies
session <- html_session("http://gdorb.decc.gov.uk/certification-body-search")

# Submit the form with no constraints, to receive all values
results <-
  session %>%
  html_form() %>%
  keep(~ .x$name == "search_form") %>%
  .[[1]] %>%
  submit_form(session, .)

# We should have 21 results as of 2017-05-11
results_n <- results %>%
  html_node(".squiloople-pagination-information") %>%
  html_text() %>%
  str_extract("[0-9]+$") %>%
  parse_number
results_n

# Create a list for each page of results
pages <- ceiling(results_n / 10)
all_results <- vector("list", pages)

# Get all pages (takes a few minutes, expect errors on the final page)
for(i in seq_len(pages)) {
  cat("page: ", i, "\n")
  all_results[[i]] <- next_results(css = "td a", n_urls = 2)
}

combined <- all_results %>%
  bind_rows() %>%
  select(-4) %>%
  as_tibble %>%
  distinct # The last few rows might have been duplicated by the website

write_tsv(combined, "./lists/green-deal-orb/certified-bodies.tsv")

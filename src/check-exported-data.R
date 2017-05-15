# Investigate the green-deal-orb lists that were exported from the website

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

participant <- bind_rows(assessor, provider, installer)

# It is the same size as data scraped on the same date
nrow(participant)

# IDs are unique
participant %>%
  map(~ length(unique(.x)))

# Date Authorised can be missing, so can Measures offerred
participant %>%
  map(~ anyNA(.x))

# When Date Authorised is missing, then Measures offerred is not missing
participant %>%
  filter(is.na(`Date Authorised`)) %>%
  map(~ anyNA(.x))

# When Measures offered is missing, then Date Authorised is not missing
participant %>%
  filter(is.na(`Measures offered`)) %>%
  map(~ anyNA(.x))

# Only 173 are missing Date Authorised, all of which are in 'initial
# authorisation' or 'voluntary withdrawal'
# Note spellings of 'initial authorisation' and 'Initial authorisation'
participant %>%
  filter(is.na(`Date Authorised`)) %>%
  count(`Reason for status change`)

# Other misspellings of Reasoon for status change are initial/Initial
# authorisation, and voluntary withdrawal vs withdrawn
participant %>%
  count(`Reason for status change`)

# No misspellings of Status
participant %>%
  count(Status)

# Measures offered meets a likely maximum character length limit
# There are 10 of these
participant %>%
  map_if(is.character, ~ max(nchar(.x), na.rm = TRUE))

# Yes, at least twice at least one whole field has been omitted (following a
# comma)
participant %>%
  filter(str_detect(`Measures offered`, ",$")) %>%
  select(ID, `Measures offered`) %>%
  glimpse

# Company GDPA253 is missing several whole measures due to the 1024-character
# limit
participant %>%
  filter(nchar(`Measures offered`) == 1024,
         ID == "GDPA253") %>%
  select(ID, type, `Measures offered`) %>%
  .$`Measures offered`
# * Air source heat pumps
# * Photovoltaics
# * Pipe-work insulation
# * Radiant heating
# * Replacement glazing
# * Roof insulation
# * Room in roof insulation
# * Sealing improvements (including duct sealing)
# * Secondary glazing
# * Solar blinds, shutters and shading devices
# * Solar water heating
# * Transpired solar collectors
# * Under-floor heating
# * Under-floor insulation
# * Variable speed drives for fans and pumps
# * Warm-air units
# * Waste water heat recovery devices attached to showers
# * Water source heat pumps
# It has:
# "Domestic,Non-domestic,Green Deal Cashback Scheme,Biomass boilers,Biomass room heaters (including with radiators),Cavity wall insulation,Chillers,Cylinder thermostats,Draught proofing,Duct insulation,External wall insulation systems,Fan-assisted replacement storage heaters,Flue gas heat recovery devices,Gas-fired condensing boilers,Ground source heat pumps,Heating controls (for wet central heating system and warm air system),Heating ventilation and air-conditioning controls (including zoning controls),High performance external doors,Hot water controls (including timers and temperature control),Hot water cylinder insulation,Hot water showers (efficient),Hot water systems (efficient),Hot water taps (efficient),Internal wall insulation (of external walls) systems,Lighting systems, fittings and controls (including rooflights, lamps and luminaires),Loft or rafter insulation (including loft hatch insulation),Mechanical ventilation with heat recovery,Micro combined heat and power,Micro wind generation,Oil-fired conde"
# str_length("Domestic,Non-domestic,Green Deal Cashback Scheme,Biomass boilers,Biomass room heaters (including with radiators),Cavity wall insulation,Chillers,Cylinder thermostats,Draught proofing,Duct insulation,External wall insulation systems,Fan-assisted replacement storage heaters,Flue gas heat recovery devices,Gas-fired condensing boilers,Ground source heat pumps,Heating controls (for wet central heating system and warm air system),Heating ventilation and air-conditioning controls (including zoning controls),High performance external doors,Hot water controls (including timers and temperature control),Hot water cylinder insulation,Hot water showers (efficient),Hot water systems (efficient),Hot water taps (efficient),Internal wall insulation (of external walls) systems,Lighting systems, fittings and controls (including rooflights, lamps and luminaires),Loft or rafter insulation (including loft hatch insulation),Mechanical ventilation with heat recovery,Micro combined heat and power,Micro wind generation,Oil-fired conde")
# 1024-characters long

# Filter for Measures offered not at the 1024-character-limit, unnest the column
# and count the number of each measure.  There are still some spammy-looking
# ones of length 64 (though the max is 84)
participant %>%
  filter(nchar(`Measures offered`) < 1024) %>%
  mutate(`Measures offered` = str_split(`Measures offered`, ",(?! )")) %>%
  select(`Measures offered`) %>%
  unnest %>%
  count(`Measures offered`) %>%
  mutate(nchar = nchar(`Measures offered`)) %>%
  filter(nchar == 64) %>%
  select(nchar, n, `Measures offered`)
# These three have been truncated:
# * Heating controls (for wet central heating system and warm air sy
# * Heating ventilation and air-conditioning controls (including zon
# * Lighting systems, fittings and controls (including rooflights, l
# They ought to be
# * Heating controls (for wet central heating system and warm air system)
# * Heating ventilation and air-conditioning controls (including zoning controls)
# * Lighting systems, fittings and controls (including rooflights, lamps and luminaires)

participant %>%
  filter(nchar(`Measures offered`) < 1024) %>%
  mutate(`Measures offered` = str_split(`Measures offered`, ",(?! )")) %>%
  unnest %>%
  mutate(nchar = nchar(`Measures offered`)) %>%
  filter(nchar == 64) %>%
  count(`Measures offered`)

# Such trunctaion doesn't occur in the rendered web pages 
# http://gdorb.decc.gov.uk/index.php?option=com_content&view=article&id=131&installer_id=9549
participant %>%
  filter(nchar(`Measures offered`) < 1024) %>%
  mutate(`Measures offered` = str_split(`Measures offered`, ",(?! )")) %>%
  unnest %>%
  mutate(nchar = nchar(`Measures offered`)) %>%
  filter(nchar == 64) %>%
  select(ID, `Measures offered`)

participant %>%
  filter(str_detect(`Measures offered`, "\\(including zon,")) %>%
  .$`Measures offered` %>%
  nchar

# Permanently unnest the `Measures offered` column
participant <- bind_rows(assessor, provider, installer) %>%
  mutate(`Measures offered` = str_split(`Measures offered`, ",(?! )"))

# Eyeball all the different measures that exist
participant %>%
  unnest %>%
  count(`Measures offered`) %>%
  arrange(desc(n)) %>%
  select(n, everything()) %>%
  write_csv("temp.csv")

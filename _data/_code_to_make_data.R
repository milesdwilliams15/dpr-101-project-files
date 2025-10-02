#########################
# Code to Make Datasets #
#########################



# election.csv ------------------------------------------------------------

library(tidyverse)
library(qss)
library(tidycensus)
data("elections")
elections |>
  filter(year %in% c(2008, 2012)) |>
  mutate(
    turnout = rep + dem + other
  ) -> election

v17 <- load_variables(2010, "acs5", cache = TRUE)

View(v17)
get_acs(
  "county",
  variables = c(
    pop = "B01003_001"
  ),
  year = 2010
) -> fl
fl |>
  select(-moe) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) -> fl
fl |>
  separate(NAME, c("county", "state"), ", ") |>
  transmute(
    county = str_to_lower(county) |>
      str_remove_all(" county") |>
      str_remove_all(" parish") |>
      str_replace_all("jeff jefferson", "jefferson") |>
      str_replace_all("jeff ", "jefferson ") |>
      str_replace_all(" city city", " city") |>
      str_replace_all("st[.] ", "st ") |>
      str_replace_all("ste ", "st ") |>
      str_replace_all("saint ", "st ") |>
      str_replace_all(" ", "") |>
      str_replace_all("'", "") |>
      str_replace_all("[.]", ""),
    state = str_to_lower(state),
    pop
  ) -> fl
election |>
  mutate(
    county = county |>
      str_remove_all(" county") |>
      str_remove_all(" parish") |>
      str_replace_all("jeff jefferson", "jefferson") |>
      str_replace_all("jeff ", "jefferson ") |>
      str_replace_all(" city city", " city") |>
      str_replace_all("st[.] ", "st ") |>
      str_replace_all("ste[.] ", "st ") |>
      str_replace_all("saint ", "st ") |>
      str_replace_all(" ", "") |>
      str_replace_all("'", "")
  ) -> election

fl$county[fl$state == "virginia" & 
            fl$county == "manassasparkcity"] <- "manassascitypark"
fl$county[fl$state == "new mexico" &
            fl$county == "do?aana"] <- "doaana"
election$county[election$state == "virginia" & 
            election$county == "manassasparkcity"] <- "manassascitypark"

election |>
  pivot_longer(
    rep:turnout
  ) |>
  mutate(
    variable = paste0(name, year)
  ) |>
  select(
    -year, -name
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) -> election

left_join(
  election, fl
) -> election

election |>
  mutate(
    dem_margin08 = (dem2008 - rep2008) / (dem2008 + rep2008),
    dem_margin12 = (dem2012 - rep2012) / (dem2012 + rep2012),
    winner08 = ifelse(dem_margin08 > 0, "Dem (Obama)", "Rep (McCain)"),
    winner12 = ifelse(dem_margin12 > 0, "Dem (Obama)", "Rep (Romney)"),
    pop_decile = ntile(pop, n = 10)
  ) -> election

write_csv(
  election,
  here::here("_data", "election.csv")
)
rm(list = ls()) # clean workspace when done

# florida.csv and florida_map.csv -----------------------------------------

library(tidyverse)
library(qss)
data("florida")
#?florida
florida |>
  transmute(
    county = str_replace_all(
      county,
      "([[:upper:]])",
      " \\1"
    ) |>
      str_replace(
        "- ",
        "-"
      ) |>
      str_replace(
        " ",
        ""
      ) |>
      # str_replace(
      #   "St. ",
      #   "St "
      # ) |>
      str_replace(
        "Desoto",
        "DeSoto"
      ),
    Clinton96,
    Dole96,
    Perot96,
    Gore00,
    Bush00,
    Buchanan00,
    across(
      Clinton96:Perot96,
      ~ .x / (Clinton96 + Dole96 + Perot96),
      .names = "prop_{.col}"
    ),
    across(
      Gore00:Buchanan00,
      ~ .x / (Gore00 + Bush00 + Buchanan00),
      .names = "prop_{.col}"
    )
  ) |>
  mutate(
    county = paste(county, "County"),
    palmbeach = ifelse(
      county == "Palm Beach County",
      "Palm Beach",
      "Other Counties"
    )
  ) |>
  write_csv(
  here::here(
    "_data",
    "florida.csv"
  )
)

map_data("county") |>
  filter(region == "florida") |>
  mutate(
    county = str_to_title(subregion)
  ) |>
  write_csv(
    here::here(
      "_data",
      "florida_map.csv"
    )
  )
rm(list = ls()) # clean workspace when done



# datasets for unit 2 -----------------------------------------------------

library(tidyverse)
library(peacesciencer)
read_csv(
  here::here(
    "_data", "mie-1.0.csv"
  )
) -> mie

# aggregate the date to the event level

# mie |> # is it directed?
#   group_by(ccode1, ccode2) |>
#   count() |>
#   view() ## yes

# are events unique?
# mie |>
#   group_by(micnum, eventnum) |>
#   count() |>
#   view() ## yes
  
mie |>
  group_by(styear, stmon) |>
  summarize(
    events = n(),
    warevents = sum(hostlev==5),
    fatalmin = sum(fatalmin1 + fatalmin2),
    fatalmax = sum(fatalmax1 + fatalmax2),
    warfatalmin = sum(fatalmin1[hostlev == 5] +
                      fatalmin2[hostlev == 5]),
    warfatalmax = sum(fatalmax1[hostlev == 5] +
                        fatalmax2[hostlev == 5])
  ) -> event_count_series

write_csv(
  event_count_series,
  here::here(
    "_data", "intl_conflict.csv"
  )
)

# make a dataset of countries involved in events

mie |>
  select(styear, stmon, ccode1, ccode2) |>
  pivot_longer(
    ccode1:ccode2
  ) |>
  select(-name) |>
  distinct() |>
  mutate(
    involved_in_conflict = 1  
  )-> conflict_countries


create_stateyears(
  subset_years = 1816:2014
) -> cyd

cyd |>
  full_join(
    conflict_countries, by = c("ccode" = "value",
                               "year" = "styear")
  ) |>
  drop_na()->conflict_countries

write_csv(
  conflict_countries,
  here::here(
    "_data",
    "conflict_countries.csv"
  )
)


cyd |>
  add_sdp_gdp() |>
  transmute(
    statenme,
    year,
    pop = exp(wbpopest),
    gdp = exp(wbgdp2011est)
  ) -> cpop

write_csv(
  cpop,
  here::here(
    "_data",
    "country_pop_gdp.csv"
  )
)

cyd |>
  add_democracy() -> dem

write_csv(
  dem,
  here::here(
    "_data",
    "democracy.csv"
  )
)
rm(list = ls()) ## clean up


# elections_2000-2020.csv -------------------------------------------------

library(tidyverse)
read_csv(
  here::here(
    "_data",
    "mit_countypres_2000-2020.csv"
  )
) -> dt

## the data isn't tidy, so let's fix that
dt |>
  ## keep only the columns I need
  select( # drop these
    - state_po,
    - candidate,
    - office,
    - version,
    - mode
  ) |>
  ## pivot wider on candidates and votes
  #group_by(state, county_name) |>
  mutate(row = row_number()) |>
  pivot_wider(
    names_from = party,
    values_from = candidatevotes
  ) |>
  select(-row) |>
  group_by(year, state, county_name) |>
  summarize(
    across(county_fips:LIBERTARIAN,
           ~ median(.x, na.rm = T)),
    .groups = "drop"
  ) -> tidydt

## make stuff lowercase
colnames(tidydt) <- tidydt |>
  colnames() |>
  str_to_lower()
tidydt <- tidydt |>
  mutate(
    across(
      state:county_name,
      str_to_lower
    )
  )

## save the data as .rds to make
## more efficient
write_rds(
  tidydt,
  here::here(
    "_data",
    "countypres_2000-2020.rds"
  )
)
rm(list = ls())



# fraud.csv ---------------------------------------------------------------

## The Heritage Foundation has a database of fraud
## claims. But they didn't store it in a sensible way.
## I need to write some code to scrape their database
## then save it in a tidy format to share with students.

## open {tidyverse} and {rvest}
library(tidyverse)
library(rvest) # "rvest" instead of "harvest"

## extract the data on fraud cases
read_html(
  "https://www.heritage.org/voterfraud-print/search"
) |>
  html_node(
    "#voterfraud-print-view"
  ) |>
  html_elements("span") |>
  html_text2() -> out

## I now have a character vector that I need to convert to a data frame
out[
  out != "Source:"
][
  seq(13, length(out), by = 3)
] |> 
  na.omit() -> cln_out 

tibble(
  cln_out = cln_out,
  name = rep(
    c("state", "year", "name", "case", "fraud"),
    len = length(cln_out)
  )
) |>
  pivot_wider(
    values_from = cln_out,
    names_from = name
  ) |>
  unnest() -> Data

## Fix one missing year
Data$year <- Data$year |>
  as.numeric() |>
  replace_na(2018)

## now read in the data on election integrity
## score cards --- thankfully this one is in a 
## table!

the_code <- minimal_html('
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">1</td><td class="nonSelectedColumn"><a href="states/tn.html">Tennessee</a></td><td class="nonSelectedColumn">90</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">26</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">2-t</td><td class="nonSelectedColumn"><a href="states/al.html">Alabama</a></td><td class="nonSelectedColumn">83</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">2-t</td><td class="nonSelectedColumn"><a href="states/fl.html">Florida</a></td><td class="nonSelectedColumn">83</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">2-t</td><td class="nonSelectedColumn"><a href="states/ga.html">Georgia</a></td><td class="nonSelectedColumn">83</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">5</td><td class="nonSelectedColumn"><a href="states/ok.html">Oklahoma</a></td><td class="nonSelectedColumn">82</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">6</td><td class="nonSelectedColumn"><a href="states/sc.html">South Carolina</a></td><td class="nonSelectedColumn">81</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">7-t</td><td class="nonSelectedColumn"><a href="states/ar.html">Arkansas</a></td><td class="nonSelectedColumn">80</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">7-t</td><td class="nonSelectedColumn"><a href="states/mo.html">Missouri</a></td><td class="nonSelectedColumn">80</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">9-t</td><td class="nonSelectedColumn"><a href="states/in.html">Indiana</a></td><td class="nonSelectedColumn">79</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(0, 130, 66); color: white;">9-t</td><td class="nonSelectedColumn"><a href="states/la.html">Louisiana</a></td><td class="nonSelectedColumn">79</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/oh.html">Ohio</a></td><td class="nonSelectedColumn">76</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/wi.html">Wisconsin</a></td><td class="nonSelectedColumn">76</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">13</td><td class="nonSelectedColumn"><a href="states/tx.html">Texas</a></td><td class="nonSelectedColumn">75</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">14</td><td class="nonSelectedColumn"><a href="states/ky.html">Kentucky</a></td><td class="nonSelectedColumn">74</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">15</td><td class="nonSelectedColumn"><a href="states/ia.html">Iowa</a></td><td class="nonSelectedColumn">72</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">16-t</td><td class="nonSelectedColumn"><a href="states/ks.html">Kansas</a></td><td class="nonSelectedColumn">71</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">16-t</td><td class="nonSelectedColumn"><a href="states/ms.html">Mississippi</a></td><td class="nonSelectedColumn">71</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">18</td><td class="nonSelectedColumn"><a href="states/nc.html">North Carolina</a></td><td class="nonSelectedColumn">70</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">19</td><td class="nonSelectedColumn"><a href="states/ne.html">Nebraska</a></td><td class="nonSelectedColumn">68</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(193, 217, 75); color: black;">20</td><td class="nonSelectedColumn"><a href="states/sd.html">South Dakota</a></td><td class="nonSelectedColumn">66</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">21-t</td><td class="nonSelectedColumn"><a href="states/mt.html">Montana</a></td><td class="nonSelectedColumn">65</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">21-t</td><td class="nonSelectedColumn"><a href="states/nh.html">New Hampshire</a></td><td class="nonSelectedColumn">65</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">23</td><td class="nonSelectedColumn"><a href="states/va.html">Virginia</a></td><td class="nonSelectedColumn">63</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/az.html">Arizona</a></td><td class="nonSelectedColumn">62</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/pa.html">Pennsylvania</a></td><td class="nonSelectedColumn">62</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/ri.html">Rhode Island</a></td><td class="nonSelectedColumn">62</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">27</td><td class="nonSelectedColumn"><a href="states/mi.html">Michigan</a></td><td class="nonSelectedColumn">57</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">28-t</td><td class="nonSelectedColumn"><a href="states/ak.html">Alaska</a></td><td class="nonSelectedColumn">56</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">28-t</td><td class="nonSelectedColumn"><a href="states/dc.html">District of Columbia</a></td><td class="nonSelectedColumn">56</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(255, 242, 0); color: black;">28-t</td><td class="nonSelectedColumn"><a href="states/id.html">Idaho</a></td><td class="nonSelectedColumn">56</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">31-t</td><td class="nonSelectedColumn"><a href="states/md.html">Maryland</a></td><td class="nonSelectedColumn">54</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">25</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">31-t</td><td class="nonSelectedColumn"><a href="states/wv.html">West Virginia</a></td><td class="nonSelectedColumn">54</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">33-t</td><td class="nonSelectedColumn"><a href="states/ut.html">Utah</a></td><td class="nonSelectedColumn">53</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">24</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">33-t</td><td class="nonSelectedColumn"><a href="states/wy.html">Wyoming</a></td><td class="nonSelectedColumn">53</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">35</td><td class="nonSelectedColumn"><a href="states/de.html">Delaware</a></td><td class="nonSelectedColumn">52</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">36</td><td class="nonSelectedColumn"><a href="states/nm.html">New Mexico</a></td><td class="nonSelectedColumn">51</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">37-t</td><td class="nonSelectedColumn"><a href="states/co.html">Colorado</a></td><td class="nonSelectedColumn">50</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">24</td><td class="nonSelectedColumn">8</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">37-t</td><td class="nonSelectedColumn"><a href="states/ct.html">Connecticut</a></td><td class="nonSelectedColumn">50</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">37-t</td><td class="nonSelectedColumn"><a href="states/me.html">Maine</a></td><td class="nonSelectedColumn">50</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(247, 148, 29); color: black;">37-t</td><td class="nonSelectedColumn"><a href="states/nd.html">North Dakota</a></td><td class="nonSelectedColumn">50</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/mn.html">Minnesota</a></td><td class="nonSelectedColumn">45</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/nj.html">New Jersey</a></td><td class="nonSelectedColumn">45</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/ny.html">New York</a></td><td class="nonSelectedColumn">45</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">11</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">45</td><td class="nonSelectedColumn"><a href="states/ma.html">Massachusetts</a></td><td class="nonSelectedColumn">44</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">46</td><td class="nonSelectedColumn"><a href="states/wa.html">Washington</a></td><td class="nonSelectedColumn">40</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">24</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">47-t</td><td class="nonSelectedColumn"><a href="states/or.html">Oregon</a></td><td class="nonSelectedColumn">38</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">47-t</td><td class="nonSelectedColumn"><a href="states/vt.html">Vermont</a></td><td class="nonSelectedColumn">38</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">11</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">49</td><td class="nonSelectedColumn"><a href="states/ca.html">California</a></td><td class="nonSelectedColumn">30</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">50</td><td class="nonSelectedColumn"><a href="states/nv.html">Nevada</a></td><td class="nonSelectedColumn">28</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">51</td><td class="nonSelectedColumn"><a href="states/hi.html">Hawaii</a></td><td class="nonSelectedColumn">27</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
  <tr><td class="selectedColumn" style="background-color: rgb(196, 22, 28); color: white;">41</td><td class="nonSelectedColumn"><a href="states/il.html">Illinois</a></td><td class="nonSelectedColumn">47</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
')

the_code |>
  html_elements("tr") |>
  html_text2() |>
  str_replace_all("\\t", ", ") |>
  as_tibble() |>
  separate(
    value,
    into = c(
      "fraud_rank",
      "state",
      "fraud_score",
      "voter_id",
      "voter_reg",
      "absentee_man",
      "vote_harvesting",
      "election_obs",
      "citizen_verify",
      "voter_asst_id",
      "vote_counting",
      "litigation",
      "same_day_reg",
      "auto_reg",
      "private_funding",
      "audits",
      "rank_choice"
    ),
    sep = ", "
  ) |>
  mutate(
    across(fraud_score:rank_choice, as.numeric),
    fraud_rank = 52 - rank(fraud_score, ties.method = "max")
  ) -> score_data

## Clean up the data for students

Data |>
  group_by(state) |>
  count() -> fraud_counts

full_join(
  fraud_counts,
  score_data |> select(state, fraud_rank, fraud_score)
) |>
  mutate(
    cum_fraud_82_to_24 = replace_na(n, 0)
  ) |>
  select(-n) -> fraud_data

## okay let's bring in election returns for 2020 and 2016 by state
read_csv(
  here::here(
    "_data",
    "mit_countypres_2000-2020.csv"
  )
) -> elec_data

elec_data |>
  filter(year >= 2016) |>
  group_by(year, state, state_po, party) |>
  summarize(
    votes = sum(candidatevotes, na.rm = T)
  ) |>
  ungroup() |>
  pivot_wider(
    values_from = votes,
    names_from = party
  ) |>
  transmute(
    year, state, state_po,
    dem = DEMOCRAT,
    rep = REPUBLICAN,
    other = rowSums(
      cbind(DEMOCRAT, OTHER, REPUBLICAN, GREEN, LIBERTARIAN),
      na.rm = T
    ) - rep - dem,
    total = other + rep + dem
  ) |>
  pivot_wider(
    values_from = c(dem, rep, total, other),
    names_from = year
  ) -> clean_elec_data

## merge data for MA1
full_join(
  clean_elec_data |> mutate(state = str_to_lower(state)),
  fraud_data |> mutate(state = str_to_lower(state)),
  by = "state"
) -> ma1_data

## Save the datasets
write_csv(
  Data,
  here::here("_data", "heritage_fraud_data.csv")
)
write_csv(
  fraud_counts,
  here::here("_data", "heritage_fraud_scores.csv")
)
write_csv(
  clean_elec_data,
  here::here("_data", "mit_pres_returns_2016-2020.csv")
)
write_csv(
  ma1_data,
  here::here("_data", "ma1_data.csv")
)

rm(list = ls())



# county_data_2024 --------------------------------------------------------

library(tidyverse)

d24 <- read_csv(
  "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/refs/heads/master/2024_US_County_Level_Presidential_Results.csv"
)

d20 <- read_csv(
  "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/refs/heads/master/2020_US_County_Level_Presidential_Results.csv"
)

full_join(
  x = d24 |>
    transmute(
      state_name, county_name,
      rep24 = votes_gop,
      dem24 = votes_dem,
      tot24 = total_votes,
      rep_share24 = rep24 / tot24,
      dem_share24 = dem24 / tot24,
      rep_margin24 = rep_share24 - dem_share24
    ),
  y = d20 |>
    transmute(
      state_name, county_name,
      rep20 = votes_gop,
      dem20 = votes_dem,
      tot20 = total_votes,
      rep_share20 = rep20 / tot20,
      dem_share20 = dem20 / tot20,
      rep_margin20 = rep_share20 - dem_share20
    )
) -> dt

dt |>
  mutate(
    rep_shift = rep_margin24 - rep_margin20
  ) -> dt

write_csv(
  dt,
  here::here("_data", "county_data_2024.csv")
)

# make a version at the state level

dt |>
  select(-county_name) |>
  group_by(state_name) |>
  summarize(
    across(c(rep20:tot20, rep24:tot24), ~ sum(.x, na.rm = T))
  ) |>
  ungroup() |>
  mutate(
    rep_share20 = rep20 / tot20,
    dem_share20 = dem20 / tot20,
    rep_margin20 = rep_share20 - dem_share20,
    rep_share24 = rep24 / tot24,
    dem_share24 = dem24 / tot24,
    rep_margin24 = rep_share24 - dem_share24,
    rep_shift = rep_margin24 - rep_margin20
  ) |>
  write_csv(
    here::here("_data", "state_data_2024.csv")
  )

rm(list = ls())

# new code for fraud.csv dataset ------------------------------------------

library(tidyverse)
library(here)

## heritage fraud data (as of 10.02.2025)
read_csv(
  here("_data", "heritage-voter-fraud.csv")
) -> fraud


## integrity data is still shitty
library(rvest)

the_code <- minimal_html('
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">1-t</td><td class="nonSelectedColumn"><a href="states/ar.html">Arkansas</a></td><td class="selectedColumn">91</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">26</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">1-t</td><td class="nonSelectedColumn"><a href="states/tn.html">Tennessee</a></td><td class="selectedColumn">91</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">27</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">3</td><td class="nonSelectedColumn"><a href="states/al.html">Alabama</a></td><td class="selectedColumn">89</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">25</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">4</td><td class="nonSelectedColumn"><a href="states/la.html">Louisiana</a></td><td class="selectedColumn">85</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">5-t</td><td class="nonSelectedColumn"><a href="states/fl.html">Florida</a></td><td class="selectedColumn">83</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">5-t</td><td class="nonSelectedColumn"><a href="states/ga.html">Georgia</a></td><td class="selectedColumn">83</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">7-t</td><td class="nonSelectedColumn"><a href="states/in.html">Indiana</a></td><td class="selectedColumn">82</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">7-t</td><td class="nonSelectedColumn"><a href="states/ok.html">Oklahoma</a></td><td class="selectedColumn">82</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">9</td><td class="nonSelectedColumn"><a href="states/sc.html">South Carolina</a></td><td class="selectedColumn">81</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(0, 130, 66); color: white;">10</td><td class="nonSelectedColumn"><a href="states/mo.html">Missouri</a></td><td class="selectedColumn">80</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/ms.html">Mississippi</a></td><td class="selectedColumn">76</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/oh.html">Ohio</a></td><td class="selectedColumn">76</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/tx.html">Texas</a></td><td class="selectedColumn">76</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">11-t</td><td class="nonSelectedColumn"><a href="states/wi.html">Wisconsin</a></td><td class="selectedColumn">76</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">15</td><td class="nonSelectedColumn"><a href="states/ky.html">Kentucky</a></td><td class="selectedColumn">74</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">16</td><td class="nonSelectedColumn"><a href="states/ia.html">Iowa</a></td><td class="selectedColumn">72</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">17</td><td class="nonSelectedColumn"><a href="states/ks.html">Kansas</a></td><td class="selectedColumn">71</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">18</td><td class="nonSelectedColumn"><a href="states/nc.html">North Carolina</a></td><td class="selectedColumn">70</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">19</td><td class="nonSelectedColumn"><a href="states/ne.html">Nebraska</a></td><td class="selectedColumn">68</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">20-t</td><td class="nonSelectedColumn"><a href="states/nh.html">New Hampshire</a></td><td class="selectedColumn">66</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(193, 217, 75); color: black;">20-t</td><td class="nonSelectedColumn"><a href="states/sd.html">South Dakota</a></td><td class="selectedColumn">66</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">22</td><td class="nonSelectedColumn"><a href="states/mt.html">Montana</a></td><td class="selectedColumn">65</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">23</td><td class="nonSelectedColumn"><a href="states/va.html">Virginia</a></td><td class="selectedColumn">63</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/az.html">Arizona</a></td><td class="selectedColumn">62</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/id.html">Idaho</a></td><td class="selectedColumn">62</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/pa.html">Pennsylvania</a></td><td class="selectedColumn">62</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">24-t</td><td class="nonSelectedColumn"><a href="states/ri.html">Rhode Island</a></td><td class="selectedColumn">62</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">28</td><td class="nonSelectedColumn"><a href="states/ut.html">Utah</a></td><td class="selectedColumn">61</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">25</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">29</td><td class="nonSelectedColumn"><a href="states/wv.html">West Virginia</a></td><td class="selectedColumn">59</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">20</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(255, 242, 0); color: black;">30</td><td class="nonSelectedColumn"><a href="states/wy.html">Wyoming</a></td><td class="selectedColumn">58</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">31</td><td class="nonSelectedColumn"><a href="states/mi.html">Michigan</a></td><td class="selectedColumn">57</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">32</td><td class="nonSelectedColumn"><a href="states/ak.html">Alaska</a></td><td class="selectedColumn">56</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">33</td><td class="nonSelectedColumn"><a href="states/dc.html">District of Columbia</a></td><td class="selectedColumn">55</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">34</td><td class="nonSelectedColumn"><a href="states/md.html">Maryland</a></td><td class="selectedColumn">54</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">25</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">35-t</td><td class="nonSelectedColumn"><a href="states/de.html">Delaware</a></td><td class="selectedColumn">52</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">35-t</td><td class="nonSelectedColumn"><a href="states/nd.html">North Dakota</a></td><td class="selectedColumn">52</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">9</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">37</td><td class="nonSelectedColumn"><a href="states/nm.html">New Mexico</a></td><td class="selectedColumn">51</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">38-t</td><td class="nonSelectedColumn"><a href="states/co.html">Colorado</a></td><td class="selectedColumn">50</td><td class="nonSelectedColumn">6</td><td class="nonSelectedColumn">24</td><td class="nonSelectedColumn">8</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">38-t</td><td class="nonSelectedColumn"><a href="states/ct.html">Connecticut</a></td><td class="selectedColumn">50</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">14</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(247, 148, 29); color: black;">38-t</td><td class="nonSelectedColumn"><a href="states/me.html">Maine</a></td><td class="selectedColumn">50</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">23</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">41</td><td class="nonSelectedColumn"><a href="states/il.html">Illinois</a></td><td class="selectedColumn">47</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">22</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/mn.html">Minnesota</a></td><td class="selectedColumn">45</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">17</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/nj.html">New Jersey</a></td><td class="selectedColumn">45</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">10</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">42-t</td><td class="nonSelectedColumn"><a href="states/ny.html">New York</a></td><td class="selectedColumn">45</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">19</td><td class="nonSelectedColumn">11</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">45</td><td class="nonSelectedColumn"><a href="states/ma.html">Massachusetts</a></td><td class="selectedColumn">44</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">16</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">46</td><td class="nonSelectedColumn"><a href="states/wa.html">Washington</a></td><td class="selectedColumn">40</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">24</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">47</td><td class="nonSelectedColumn"><a href="states/nv.html">Nevada</a></td><td class="selectedColumn">39</td><td class="nonSelectedColumn">12</td><td class="nonSelectedColumn">15</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">48-t</td><td class="nonSelectedColumn"><a href="states/or.html">Oregon</a></td><td class="selectedColumn">38</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">1</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">48-t</td><td class="nonSelectedColumn"><a href="states/vt.html">Vermont</a></td><td class="selectedColumn">38</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">21</td><td class="nonSelectedColumn">11</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">50</td><td class="nonSelectedColumn"><a href="states/ca.html">California</a></td><td class="selectedColumn">30</td><td class="nonSelectedColumn">4</td><td class="nonSelectedColumn">18</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
<tr><td class="nonSelectedColumn" style="background-color: rgb(196, 22, 28); color: white;">51</td><td class="nonSelectedColumn"><a href="states/hi.html">Hawaii</a></td><td class="selectedColumn">27</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">13</td><td class="nonSelectedColumn">7</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">3</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">2</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">0</td><td class="nonSelectedColumn">1</td><td class="nonSelectedColumn">0</td></tr>
')

the_code |>
  html_elements("tr") |>
  html_text2() |>
  str_replace_all("\\t", ", ") |>
  as_tibble() |>
  separate(
    value,
    into = c(
      "fraud_rank",
      "state_name",
      "integrity_score",
      "voter_id",
      "voter_reg",
      "absentee_man",
      "vote_harvesting",
      "election_obs",
      "citizen_verify",
      "voter_asst_id",
      "vote_counting",
      "litigation",
      "same_day_reg",
      "auto_reg",
      "private_funding",
      "audits",
      "rank_choice"
    ),
    sep = ", "
  ) |>
  mutate(
    across(integrity_score:rank_choice, as.numeric)
  ) -> score

score |>
  select(state_name, integrity_score) |>
  arrange(-integrity_score) |>
  mutate(
    integrity_cat = c(rep(
      c("Ranked 1-10", "Ranked 11-20",
        "Ranked 21-20", "Ranked 31-40"),
      each = 10
    ), rep("Ranked 41-51", len = 11))
  ) -> score

# county data
read_csv(
  here("_data", "county_data_2024.csv")
) -> county


## Clean up

state_dt <- bind_cols(state_abbr = state.abb, state_name = state.name) |>
  bind_rows(tibble(state_abbr = "DC", state_name = "District of Columbia"))

fraud |>
  mutate(
    state_abbr = State
  ) |>
  count(state_abbr) |>
  rename(fraud_cases = n) -> fraud

state_dt |>
  left_join(fraud) |>
  mutate(
    fraud_cases = replace_na(fraud_cases, 0)
  ) -> fraud

fraud |>
  left_join(score) -> hold_this

county |>
  left_join(hold_this) -> hold_this

# now fix AK and CT counties

hold_this |>
  filter(
    state_abbr %in% c("AK", "CT")
  ) |>
  group_by(state_name) |>
  summarize(
    state_abbr = unique(state_abbr),
    across(rep24:rep_shift, ~ sum(.x, na.rm = T)),
    across(fraud_cases:integrity_cat, unique)
  ) |>
  mutate(
    rep_share24 = rep24 / tot24,
    dem_share24 = dem24 / tot24,
    rep_margin24 = rep_share24 - dem_share24,
    rep_share20 = rep20 / tot20,
    dem_share20 = dem20 / tot20,
    rep_margin20 = rep_share20 - dem_share20,
    rep_shift = rep_margin24 - rep_margin20
  ) -> akct

library(usmap)

akct_states <- us_map("counties") |>
  filter(abbr %in% c("AK", "CT"))

tibble(
  state_name = akct_states$full,
  state_abbr = akct_states$abbr,
  county_name = akct_states$county
) |>
  left_join(akct) -> akct

hold_this |>
  filter(
    !(state_abbr %in% c("AK", "CT"))
  ) |>
  bind_rows(akct) -> final_data

write_csv(
  final_data,
  here("_data", "ma1_dataset.csv")
)

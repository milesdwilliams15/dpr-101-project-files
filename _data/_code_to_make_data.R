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
      str_replace(
        "St. ",
        "St "
      ) |>
      str_replace(
        "Desoto",
        "De Soto"
      ),
    dem_96 = Clinton96,
    rep_96 = Dole96,
    lib_96 = Perot96,
    dem_00 = Gore00,
    rep_00 = Bush00,
    lib_00 = Buchanan00,
    across(
      dem_96:lib_96,
      ~ .x / (dem_96 + rep_96 + lib_96),
      .names = "p{.col}"
    ),
    across(
      dem_00:lib_00,
      ~ .x / (dem_00 + rep_00 + lib_00),
      .names = "p{.col}"
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

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
rm(list = ls())

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
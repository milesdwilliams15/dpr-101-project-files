#########################
# Code to Make Datasets #
#########################


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
    ),
    dlib_96 = lib_96 / sum(lib_96),
    dlib_00 = lib_00 / sum(lib_00)
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
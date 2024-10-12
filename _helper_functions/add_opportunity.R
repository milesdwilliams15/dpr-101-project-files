################################################
# Function to add conflict opportunity measure #
################################################

add_opportunity <- function(data) {
  create_dyadyears() |>
    add_cow_majors() |>
    add_contiguity() |>
    add_capital_distance() -> ddy
  
  logit <- function(x) 1 / (1 + exp(-x))
  
  ddy |>
    mutate(
      contig = ifelse(conttype >= 1, 1, 0),
      majdyad = pmax(cowmaj1, cowmaj2),
      prd = ifelse(contig == 1 | majdyad == 1, 1, 0),
      opportunity = logit(
        4.801 + 4.50*contig - 1.051*log(capdist) + 2.901*majdyad)
    ) -> ddy
  
  ddy |>
    group_by(ccode1, year) |>
    summarize(
      prd = mean(prd, na.rm = T),
      opportunity = mean(opportunity, na.rm = T),
      .groups = "drop"
    ) |>
    rename(
      ccode = ccode1
    ) -> cy
  
  left_join(
    data, cy, by = c("ccode", "year")
  )
}
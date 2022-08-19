library(tidyverse)

ppr_code <- c(
  "NN" = "C",
  "TN" = "A",
  "ND" = "T",
  "ND" = "U",
  "TD" = "G"
)

rna_code <- c(
  "C" = "NN",
  "A" = "TN",
  "U" = "ND",
  "T" = "ND",
  "G" = "TD"
)

readLines("inst/extdata/Exported.fa") |>
  as_tibble() |>
  mutate(
    name = str_detect(value, ">"),
    seq = cumsum(name),
  ) |>
  group_by(seq) |>
  summarise(value = paste0(value, collapse = "")) |>
  mutate(
    name = str_extract(value, "(?<=>)[^\\(]+") |> str_trim(),
    sequence = str_extract(value, "(?<=\\))\\w+"),
    bp = nchar(sequence),
    aa = str_extract(name, "[A-Z]{1,2}$"),
    group = str_extract(name, "^\\w{1,2}")
  ) |>
  select(name, group, bp, aa, sequence) -> dat


dat |>
  mutate(
    pre = str_extract(aa, "^\\w"),
    nex = str_extract(aa, "\\w$"),
    number = str_extract(group, "\\d+")
  ) |>
  print(n = 100)

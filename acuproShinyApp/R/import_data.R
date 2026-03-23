#' Getting species list of possible species in area
#' @description
#' To represent a realistic species composition, data from artskart has been
#' downloaded for names to be sampled for the distribution on gradients.
#' @returns a list of species from area to be surveyed in my masters
#' @export
#'

library(tidyverse)
library(readxl)

raw <- read_xlsx(
  'data/Aculeata_Samnanger, Kvam, Voss, Ulvik, Eidfjord_not Absent_2010+_Polygon.xlsx'
  )|>
  janitor::clean_names()

spp <- raw |>
  select(vitenskapelig_navn) |>
  distinct() |>
  as.list() |>
  unlist()



#token: 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
library(tidyverse)
library(readxl)

raw <- read_xlsx(
  'data/Aculeata_Samnanger, Kvam, Voss, Ulvik, Eidfjord_not Absent_2010+_Polygon.xlsx'
  )|>
  janitor::clean_names()


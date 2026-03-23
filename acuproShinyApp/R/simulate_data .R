#' Creates dataframe usefull for acuproApp
#' @description
#' This function creates a dataframe with two gradient columns, one species column
#' , a genus column and three trait columns. The dataframe randomly combines
#' gradient values, species and traits. This means that any species can be found
#' anywhere on both gradients, and with any trait combination. The column genus
#' is made from the the species column
#' @param gradient1_values a list of values for the first gradient
#' @param gradient2_values a list of values for the second gradient
#' @param species_list a list of scientific species names, separated by space or ' '
#' @param trait1 name of the first trait
#' @param trait2 name of the second trait
#' @param trait3 name of the third trait
#' @returns a `data.frame`
#' @export
#'

gradient_data <- function(gradient1_values, gradient2_values, species_list, trait1, trait2, trait3){
  data.frame(
    gradient1 = sample(gradient1_values, 500, TRUE),
    gradient2 = sample(gradient2_values, 500, TRUE),
    species = sample(species_list, 500, TRUE),
    genus = sub(' .*', '', df$species),
    trait1 = sample(0:1, 500, TRUE),
    trait2 = sample(0:1, 500, TRUE),
    trait3 = sample(0:1, 500, TRUE)
  )
}

#plan from tomorrow -> finish dataframe function. Write read me. Change plots in app
#to be regression plots. Maybe change checkboxes to radiobuttons. Upload all to github
rain <- c(4000, 2750, 2125, 1900, 1650)
masl <- c(75, 425, 810)
spp <- raw |>
  select(vitenskapelig_navn) |>
  distinct() |>
  as.list() |>
  unlist()

df <- data.frame(
  precipetation = sample(rain, 200, TRUE),
  elevation = sample(masl, 200, TRUE),
  species = sample(spp, 200, TRUE),
  genus = sub(' .*', '', df$species),
  trait1 = sample(0:1, 200, TRUE),
  trait2 = sample(0:1, 200, TRUE),
  trait3 = sample(0:1, 200, TRUE)
)

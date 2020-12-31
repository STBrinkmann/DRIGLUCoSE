## code to prepare `Erlangen_isodistances` dataset goes here
erlangen.isodistances <- sf::read_sf("docs/erlangen_isodistances.gpkg")

usethis::use_data(erlangen.isodistances, overwrite = TRUE, compress = "xz")

## code to prepare `Erlangen` dataset goes here
Erlangen <- dplyr::tibble(
  tag = c(1, 2),
  age = c(26, 34),
  sex = c("Male", "Female"),
  X = c(35199.46, 36281.59),
  Y = c(-159433.5, -159243.2)
) %>%
  # https://ij-healthgeographics.biomedcentral.com/articles/10.1186/1476-072X-11-43
  dplyr::mutate(Speed = ifelse(sex == "Female",
                               # Female
                               ifelse(age %in% 18:30, 4.77,
                               ifelse(age %in% 31:40, 4.79,
                               ifelse(age %in% 41:50, 4.71,
                               ifelse(age %in% 51:60, 4.72,
                               ifelse(age %in% 61:70, 4.37, 4.28))))),

                               # Male
                               ifelse(age %in% 18:30, 4.71,
                               ifelse(age %in% 31:40, 4.95,
                               ifelse(age %in% 41:50, 4.96,
                               ifelse(age %in% 51:60, 4.71,
                               ifelse(age %in% 61:70, 4.59, 4.49)))))
  )) %>%
  dplyr::mutate(Speed = Speed / 3.6 * 60) %>%
  dplyr::select(tag, Speed, X, Y) %>%
  sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(4839)) %>%
  dplyr::rename(geom = geometry)



usethis::use_data(Erlangen, overwrite = TRUE, compress = "xz")

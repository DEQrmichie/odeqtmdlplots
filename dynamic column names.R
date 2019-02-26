dplyr::mutate(difference = UQ(rlang::sym(first_station_name)) - UQ(rlang::sym(second_station_name)))

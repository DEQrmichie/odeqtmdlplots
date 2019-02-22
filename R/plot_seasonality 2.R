library(AWQMSdata)
library(tidyverse)

test_dataset <- AWQMS_Data(station = '14046778',
                           char = 'Temperature, water',
                           stat_base = '7DADM',
                           crit_codes = TRUE)

test_dataset2 <- test_dataset %>%
  select(MLocID, SampleStartDate, Result_Numeric)



graph_data <- test_dataset2
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::group_by(MLocID) %>%
  tidyr::complete(SampleStartDate = seq.Date(min(SampleStartDate), max(SampleStartDate), by="day")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = lubridate::year(SampleStartDate),
                month = lubridate::month(SampleStartDate)) %>%
  dplyr::mutate(doy = strftime(SampleStartDate, format = "%m-%d"))



season <- graph_data %>%
  dplyr::filter(Result_Numeric > (buffer * temp_criteria))


if(nrow(season) > 0){

first_date <- as.Date((min(season$doy, na.rm = TRUE)),
                      format = "%m-%d")

last_date <- as.Date((max(season$doy, na.rm = TRUE)),
                     format = "%m-%d")

} else {
  first_date <- as.Date("12-31",
                        format = "%m-%d")
  last_date <- as.Date("12-31",
                       format = "%m-%d")

}


# spawn -------------------------------------------------------------------


spawn_season <- graph_data %>%
  mutate(Spawnstart = as.Date(spawn_start, format = "%m-%d"),
         spawnend = as.Date(spawn_end, format = "%m-%d")) %>%
  mutate(inspawn = ifelse(Spawnstart < spawnend &
                            as.Date(graph_data$doy, format = "%m-%d") >= Spawnstart &
                            as.Date(graph_data$doy, format = "%m-%d") <= spawnend, 1,
                          ifelse(Spawnstart > spawnend &
                                   as.Date(graph_data$doy, format = "%m-%d") >= Spawnstart |
                                   as.Date(graph_data$doy, format = "%m-%d") <= spawnend , 1, 0 ))) %>%
  dplyr::filter(Result_Numeric > (buffer * spawn_crit) &
                  inspawn == 1)


if(nrow(spawn_season) > 0){
first_date_spawn <- as.Date((min(spawn_season$doy, na.rm = TRUE)),
                      format = "%m-%d")
last_date_spawn <- as.Date((max(spawn_season$doy, na.rm = TRUE)),
                           format = "%m-%d")

} else {
  first_date_spawn <- as.Date(spawn_end,
                        format = "%m-%d")
  last_date_spawn <- as.Date(spawn_end,
                       format = "%m-%d")

}


g <- ggplot2::ggplot() +
  ggplot2::geom_line(data = graph_data,
                     ggplot2::aes(x = as.Date(graph_data$doy, format = "%m-%d"),
                                  y = Result_Numeric,
                                  group = interaction(year,MLocID),
                                  linetype = MLocID),
                     color = "grey47")+
  ggplot2::theme_classic()+
  ggplot2::scale_x_date(breaks = seq(as.Date("01-01", format = "%m-%d"),
                                     as.Date("12-31", format = "%m-%d"),
                                     by = "1 months"),
                        date_labels = "%b",
                        expand = ggplot2::expand_scale())+
  ggplot2::scale_y_continuous(breaks=seq(0,100,5),
                              limits = c(0,NA),
                              expand = c(0,0))
 #break here

#spawn in middle of period
if(as.Date(spawn_start, format = "%m-%d") < as.Date(spawn_end, format = "%m-%d") &
   spawn_start != "01-01"){

  g <- g +
    ggplot2::geom_segment(aes(x = as.Date("01-01", format = "%m-%d"),
                              xend = as.Date(spawn_start, format = "%m-%d"),
                              y = temp_criteria,
                              yend = temp_criteria),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_segment(aes(x = as.Date(spawn_end, format = "%m-%d"),
                              xend = as.Date("12-31", format = "%m-%d"),
                              y = temp_criteria,
                              yend = temp_criteria),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_segment(aes(x = as.Date(spawn_start, format = "%m-%d"),
                              xend = as.Date(spawn_end, format = "%m-%d"),
                              y = spawn_crit,
                              yend = spawn_crit),
                          linetype = "longdash",
                          size = 0.7)+
    ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("01-01", format = "%m-%d"),
                                    xmax = first_date,
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2') +
    ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                    xmax = as.Date("12-31", format = "%m-%d"),
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2') +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_start, format = "%m-%d"),
                                    xmax = first_date_spawn,
                                    ymin = 0,
                                    ymax = spawn_crit),
                       alpha = 0.3, fill = 'steelblue2')

#spawn starting on January 1st
} else if(spawn_start == "01-01"){

  g <- g +
    ggplot2::geom_segment(aes(x = as.Date(spawn_start, format = "%m-%d"),
                              xend = as.Date(spawn_end, format = "%m-%d"),
                              y = spawn_crit,
                              yend = spawn_crit),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_segment(aes(x = as.Date(spawn_end, format = "%m-%d"),
                              xend = as.Date("12-31", format = "%m-%d"),
                              y = temp_criteria,
                              yend = temp_criteria),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_start, format = "%m-%d"),
                                    xmax = min(first_date_spawn, as.Date(spawn_end, format = "%m-%d")),
                                    ymin = 0,
                                    ymax = spawn_crit),
                       alpha = 0.3, fill = 'steelblue2') +
    ggplot2::geom_rect(ggplot2::aes(xmin = last_date_spawn,
                                    xmax = as.Date(spawn_end, format = "%m-%d"),
                                    ymin = 0,
                                    ymax = spawn_crit),
                       alpha = 0.3, fill = 'steelblue2') +
    ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                    xmax = as.Date("12-31", format = "%m-%d"),
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2')

    if(first_date > as.Date(spawn_end, format = "%m-%d")){


    g <- g +
      ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_end, format = "%m-%d"),
                                    xmax = first_date ,
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2')

}


} else if (as.Date(spawn_start, format = "%m-%d") > as.Date(spawn_end, format = "%m-%d") &
  spawn_start != "01-01") {

  g <- g +
    ggplot2::geom_segment(aes(x = as.Date('01-01', format = "%m-%d"),
                              xend = as.Date(spawn_end, format = "%m-%d"),
                              y = spawn_crit,
                              yend = spawn_crit),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_segment(aes(x = as.Date(spawn_start, format = "%m-%d"),
                              xend = as.Date('12-31', format = "%m-%d"),
                              y = spawn_crit,
                              yend = spawn_crit),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_segment(aes(x = as.Date(spawn_end, format = "%m-%d"),
                              xend = as.Date(spawn_start, format = "%m-%d"),
                              y = temp_criteria,
                              yend = temp_criteria),
                          linetype = "longdash",
                          size = 0.7) +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.Date('01-01', format = "%m-%d"),
                                    xmax = min(first_date_spawn, as.Date(spawn_end, format = "%m-%d")),
                                    ymin = 0,
                                    ymax = spawn_crit),
                       alpha = 0.3, fill = 'steelblue2')+
    ggplot2::geom_rect(ggplot2::aes(xmin = max(last_date_spawn, as.Date(spawn_start, format = "%m-%d")),
                                    xmax = as.Date('12-31', format = "%m-%d"),
                                    ymin = 0,
                                    ymax = spawn_crit),
                       alpha = 0.3, fill = 'steelblue2')


  if(first_date > as.Date(spawn_end, format = "%m-%d")){

    g <-  g+
      ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_end, format = "%m-%d"),
                                    xmax = first_date ,
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2')

  }


  if(last_date < as.Date(spawn_start, format = "%m-%d")){

    g <- g+
      ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                    xmax =as.Date(spawn_start, format = "%m-%d") ,
                                    ymin = 0,
                                    ymax = temp_criteria),
                       alpha = 0.3, fill = 'steelblue2')

  }

}























   ggplot2::geom_segment(aes(x = as.Date("01-01", format = "%m-%d"),
                             xend = as.Date(spawn_start, format = "%m-%d"),
                             y = temp_criteria,
                             yend = temp_criteria),
                         linetype = "longdash",
                         size = 0.7) +
  ggplot2::geom_segment(aes(x = as.Date(spawn_end, format = "%m-%d"),
                            xend = as.Date("12-31", format = "%m-%d"),
                            y = temp_criteria,
                            yend = temp_criteria),
                        linetype = "longdash",
                        size = 0.7) +
  ggplot2::geom_segment(aes(x = as.Date(spawn_start, format = "%m-%d"),
                            xend = as.Date(spawn_end, format = "%m-%d"),
                            y = 11,
                            yend = 11),
                        linetype = "longdash",
                        size = 0.7) +
  #rectangles
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date('01-01', format = "%m-%d"),
                                  xmax = min(first_date, as.Date(spawn_start, format = "%m-%d")),
                                  ymin = 0,
                                  ymax = temp_criteria),
                     alpha = 0.3, fill = 'steelblue2') +
  ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                  xmax = as.Date('12-31', format = "%m-%d"),
                                  ymin = 0,
                                  ymax = temp_criteria),
                     alpha = 0.3, fill = 'steelblue2')







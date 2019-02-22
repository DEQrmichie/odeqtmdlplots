#' plot_seasonality
#'
#' This function produces a graph pf seven day average daily maximum river
#' temperatures for a single monitoring location. These graphs are intended
#' to be placed in the thrid section of the temperture TMDL. Choose a monitoring location with
#' sufficient continuous temperature (many years worth of continuous data).
#' Criteria line is drawn based of fish use station desgination.
#' Blue boxed represent times of unlikely impairments. They are drawn at the
#' first and last day in dataset with a value of buffer * criteria.
#'
#' @param df Dataframe of data to be graphes
#' @param station_col Name of column containting station info. Date should be formatted in y-m-d
#' @param result_col Name of column containing result information
#' @param temp_criteria Temperature criteria. Numeric
#' @param spawn_criteria Spawning criteria
#' @param spawn_start Start of spawning season. fprmat as Character- "mm-dd"
#' @param spawn_end End of spawning season. fprmat as Character- "mm-dd"
#' @param buffer Buffer value
#' @param highlight_year Optional year to hightlight on graph
#' @param rm_labels if TRUE, remove labels from graph
#' @param lab_impair_y_nudge Move the Impairement Unlikely label up or down
#' @param lab_spawn_y_nudge Move the spawning criteria label up or down
#' @param lab_spawn_x_nudge Move the spawning criteria label left or right
#' @param lab_crit_y_nudge Move criteria label up or down
#' @param lab_crit_x_nudge Move criteria left or right
#' @param rm_legend If TRUE, remove the legend
#' @examples plot_seasonality(station = '14034470', buffer = 0.8,
#'   label_y = 12, highlight_year = 2014, include_station_legend = FALSE,
#'   include_highlight_legend = FALSE)
#' @return plot of all 7DADM data, with criteria and identified unlikely impariment seasons
#'  \if{html}{\figure{seasonality.png}{Plot}}
#'  \if{latex}{\figure{seasonality.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export
#'


plot_seasonality2 <- function(df,
                              station_col = 'MLocID',
                              date_col = 'SampleStartDate',
                              result_col = 'Result_Numeric',
                              temp_criteria = NULL,
                              spawn_criteria = 13,
                              spawn_start = NULL,
                              spawn_end = NULL,
                              fish_use = NULL,
                              buffer = 0.8,
                              highlight_year = NULL,
                              rm_labels = FALSE,
                              lab_impair_y_nudge = 0,
                              lab_spawn_y_nudge = 0,
                              lab_spawn_x_nudge = 0,
                              lab_crit_y_nudge = 0,
                              lab_crit_x_nudge = 0,
                              rm_legend = FALSE){

  if(is.null(temp_criteria)){
    stop("Include crieria value for temp_criteria")
  }


  df <- graph_data <- df[,c(station_col, date_col,result_col)]

  colnames(df) <- c("MLocID", "SampleStartDate", "Result_Numeric")



  graph_data <- df %>%
    dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
    dplyr::group_by(MLocID) %>%
    tidyr::complete(SampleStartDate = seq.Date(min(SampleStartDate), max(SampleStartDate), by="day")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = lubridate::year(SampleStartDate),
                  month = lubridate::month(SampleStartDate)) %>%
    dplyr::mutate(doy = strftime(SampleStartDate, format = "%m-%d"))



  if(!is.null(highlight_year)){

    highlight <- graph_data %>%
      dplyr::filter(year %in% highlight_year)

  }



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


  if(!is.null(spawn_end)){
    # spawn -------------------------------------------------------------------


    spawn_season <- graph_data %>%
      dplyr::mutate(Spawnstart = as.Date(spawn_start, format = "%m-%d"),
                    spawnend = as.Date(spawn_end, format = "%m-%d")) %>%
      dplyr::mutate(inspawn = ifelse(Spawnstart < spawnend &
                                       as.Date(graph_data$doy, format = "%m-%d") >= Spawnstart &
                                       as.Date(graph_data$doy, format = "%m-%d") <= spawnend, 1,
                                     ifelse(Spawnstart > spawnend &
                                              (as.Date(graph_data$doy, format = "%m-%d") >= Spawnstart |
                                                 as.Date(graph_data$doy, format = "%m-%d") <= spawnend) , 1, 0 ))) %>%
      dplyr::filter(Result_Numeric > (buffer * spawn_criteria) &
                      inspawn == 1)




    if(nrow(spawn_season) > 0){
      first_date_spawn <- as.Date((min(spawn_season$doy, na.rm = TRUE)),
                                  format = "%m-%d")
      last_date_spawn <- as.Date((max(spawn_season$doy, na.rm = TRUE)),
                                 format = "%m-%d")

    } else {
      first_date_spawn <- as.Date(spawn_start,
                                  format = "%m-%d")
      last_date_spawn <- as.Date(spawn_end,
                                 format = "%m-%d")

    }

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
                                expand = c(0,0))+
    ggplot2::labs(x = "Date",
                  y = "7DADM Temperature (°C)") +
    #ggplot2::guides(linetype=FALSE) +
    ggplot2::guides(color=ggplot2::guide_legend(title="Year"),
                    linetype=ggplot2::guide_legend(title="Monitoring Location"))+
    ggplot2::theme(legend.position="bottom")
  #break here

  # No spawn

  if(is.null(spawn_start)){
    g <- g +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date("01-01", format = "%m-%d"),
                                         xend = as.Date("12-31", format = "%m-%d"),
                                         y = temp_criteria,
                                         yend = temp_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("01-01", format = "%m-%d"),
                                      xmax = first_date,
                                      ymin = 0,
                                      ymax = temp_criteria),
                         alpha = 0.3, fill = 'steelblue2') +
      ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                      xmax = as.Date("12-31", format = "%m-%d"),
                                      ymin = 0,
                                      ymax = temp_criteria),
                         alpha = 0.3, fill = 'steelblue2')

    #spawn in middle of period
  } else if(as.Date(spawn_start, format = "%m-%d") < as.Date(spawn_end, format = "%m-%d") &
            spawn_start != "01-01"){

    g <- g +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date("01-01", format = "%m-%d"),
                                         xend = as.Date(spawn_start, format = "%m-%d"),
                                         y = temp_criteria,
                                         yend = temp_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_end, format = "%m-%d"),
                                         xend = as.Date("12-31", format = "%m-%d"),
                                         y = temp_criteria,
                                         yend = temp_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_start, format = "%m-%d"),
                                         xend = as.Date(spawn_end, format = "%m-%d"),
                                         y = spawn_criteria,
                                         yend = spawn_criteria),
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
                         alpha = 0.3, fill = 'steelblue2')

    if(as.Date(spawn_start, format = "%m-%d") == first_date){
      g <- g +
        ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_start, format = "%m-%d"),
                                      xmax = first_date_spawn,
                                      ymin = 0,
                                      ymax = spawn_criteria),
                         alpha = 0.3, fill = 'steelblue2')

    }



    if(!rm_labels){
      g <- g +
        ggplot2::annotate("text", label = "Impairment \n unlikely",
                          x = as.Date('01-10', format = "%m-%d"),
                          y = 10 + lab_impair_y_nudge,
                          size = 3,
                          hjust = 0) +
        ggplot2::annotate("text", label = "Spawning Criterion",
                          x = as.Date(spawn_start, format = "%m-%d") + lab_spawn_x_nudge,
                          y = spawn_criteria  + 0.5  + lab_spawn_y_nudge,
                          size = 3,
                          hjust = 0) +
        ggplot2::annotate("text", label = paste(fish_use, "Biological Criterion"),
                          x = as.Date('01-10', format = "%m-%d") + lab_crit_x_nudge,
                          y = temp_criteria  + 0.5  + lab_crit_y_nudge,
                          size = 3,
                          hjust = 0)
    }

    #spawn starting on January 1st
  } else if(spawn_start == "01-01"){

    g <- g +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_start, format = "%m-%d"),
                                         xend = as.Date(spawn_end, format = "%m-%d"),
                                         y = spawn_criteria,
                                         yend = spawn_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_end, format = "%m-%d"),
                                         xend = as.Date("12-31", format = "%m-%d"),
                                         y = temp_criteria,
                                         yend = temp_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_start, format = "%m-%d"),
                                      xmax = min(first_date_spawn, as.Date(spawn_end, format = "%m-%d")),
                                      ymin = 0,
                                      ymax = spawn_criteria),
                         alpha = 0.3, fill = 'steelblue2') +
      ggplot2::geom_rect(ggplot2::aes(xmin = last_date_spawn,
                                      xmax = as.Date(spawn_end, format = "%m-%d"),
                                      ymin = 0,
                                      ymax = spawn_criteria),
                         alpha = 0.3, fill = 'steelblue2') +
      ggplot2::geom_rect(ggplot2::aes(xmin = last_date,
                                      xmax = as.Date("12-31", format = "%m-%d"),
                                      ymin = 0,
                                      ymax = temp_criteria),
                         alpha = 0.3, fill = 'steelblue2')

    if(first_date > as.Date(spawn_end, format = "%m-%d") &
       min(first_date_spawn, as.Date(spawn_end, format = "%m-%d")) == as.Date(spawn_end, format = "%m-%d")){


      g <- g +
        ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(spawn_end, format = "%m-%d"),
                                        xmax = first_date ,
                                        ymin = 0,
                                        ymax = temp_criteria),
                           alpha = 0.3, fill = 'steelblue2')

    }


    if(!rm_labels){
      g <- g +
        ggplot2::annotate("text", label = "Impairment \n unlikely",
                          x = as.Date('01-10', format = "%m-%d"),
                          y = 10 + lab_impair_y_nudge,
                          size = 3,
                          hjust = 0) +
        ggplot2::annotate("text", label = "Spawning Criterion",
                          x = as.Date('01-10', format = "%m-%d") + lab_spawn_x_nudge,
                          y = spawn_criteria  + 0.5  + lab_spawn_y_nudge,
                          size = 3,
                          hjust = 0) +
        ggplot2::annotate("text", label = paste(fish_use, "Biological Criterion"),
                          x = as.Date(spawn_end, format = "%m-%d") + lab_crit_x_nudge,
                          y = temp_criteria  + 0.5  + lab_crit_y_nudge,
                          size = 3,
                          hjust = 0)
    }


  } else if (as.Date(spawn_start, format = "%m-%d") > as.Date(spawn_end, format = "%m-%d") &
             spawn_start != "01-01") {

    g <- g +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date('01-01', format = "%m-%d"),
                                         xend = as.Date(spawn_end, format = "%m-%d"),
                                         y = spawn_criteria,
                                         yend = spawn_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_start, format = "%m-%d"),
                                         xend = as.Date('12-31', format = "%m-%d"),
                                         y = spawn_criteria,
                                         yend = spawn_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(spawn_end, format = "%m-%d"),
                                         xend = as.Date(spawn_start, format = "%m-%d"),
                                         y = temp_criteria,
                                         yend = temp_criteria),
                            linetype = "longdash",
                            size = 0.7) +
      ggplot2::geom_rect(ggplot2::aes(xmin = as.Date('01-01', format = "%m-%d"),
                                      xmax = min(first_date_spawn, as.Date(spawn_end, format = "%m-%d")),
                                      ymin = 0,
                                      ymax = spawn_criteria),
                         alpha = 0.3, fill = 'steelblue2')+
      ggplot2::geom_rect(ggplot2::aes(xmin = max(last_date_spawn, as.Date(spawn_start, format = "%m-%d")),
                                      xmax = as.Date('12-31', format = "%m-%d"),
                                      ymin = 0,
                                      ymax = spawn_criteria),
                         alpha = 0.3, fill = 'steelblue2')


    if(!rm_labels){
      g <- g +
        ggplot2::annotate("text", label = "Impairment \n unlikely",
                        x = as.Date('01-10', format = "%m-%d"),
                        y = 10 + lab_impair_y_nudge,
                        size = 3,
                        hjust = 0) +
         ggplot2::annotate("text", label = "Spawning Criterion",
                        x = as.Date('01-10', format = "%m-%d") + lab_spawn_x_nudge,
                        y = spawn_criteria  + 0.5  + lab_spawn_y_nudge,
                        size = 3,
                        hjust = 0) +
         ggplot2::annotate("text", label = paste(fish_use, "Biological Criterion"),
                        x = as.Date(spawn_end, format = "%m-%d") + lab_crit_x_nudge,
                        y = temp_criteria  + 0.5  + lab_crit_y_nudge,
                        size = 3,
                        hjust = 0)
    }


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

  } else {

    g <- g
  }





  if(!is.null(highlight_year)){

    g <- g +
      ggplot2::geom_line(data = highlight,
                         ggplot2::aes(x = as.Date(highlight$doy, format = "%m-%d"),
                                      y = Result_Numeric,
                                      group = interaction(year,MLocID),
                                      color = as.character(highlight_year),
                                      linetype = MLocID),
                         size = 1.2) +
      ggplot2::scale_color_manual(name = '', values = "black")

  }

  if(rm_legend){

    g <- g +
      ggplot2::guides(linetype=FALSE,
                      color = FALSE)
  }

  return(g)

}



#' plot_minmax_compare
#'
#' Creates a plot showing the daily minimum and maximum water temperature
#' for 2 monitoring locations. Intended to be used to show the difference
#' between upstream and downstream monitoring locations. Source datafram
#' must have at least 4 columns: Date, station, minimum, and maximum.
#' If only 1 monitoring location is desired, only include the upstream
#' location. Dataframe must be in 'long' format, but with seperate
#' columns for daily minimum and maximum.
#'
#'
#' @param df Dataframe of data to be graphed
#' @param date_col Name of date column
#' @param min_col Name of daily minimum column
#' @param max_col name of daily maximum column
#' @param station_name_col name of station column
#' @param grey_station Station that will be graphed in grey
#' @param red_station Station that will be graphed in red
#' @param spawnstart Optional start of spawning season
#' @param spawnend Optional end of spawning season
#' @param crit Optional non-spwaning crietria
#' @param spawn_crit = Optional spawning season criteria
#' @examples
#'  \dontrun{
#'   plot_minmax_compare(df = minmax, date_col = "SampleStartDate",
#'    min_col = "Minimum", max_col = "Maximum", station_name_col = "Station",
#'    grey_station = "Downstream", red_station = "Upstream", crit = 20)
#'  }
#' @return plot showing daily min and max 7DADM temperatures, comparied upstream with downstream
#'   \if{html}{\figure{minmax_compare.png}{Plot}}
#'   \if{latex}{\figure{minmax_compare.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export





plot_minmax_compare <- function(df, date_col, min_col, max_col,
                                station_name_col, grey_station, red_station = NULL, spawnstart = NULL, spawnend = NULL,
                                crit = NULL, spawn_crit= NULL) {


  graph_data <- df[,c(date_col, min_col,max_col, station_name_col)]

  colnames(graph_data) <- c("Date", "Minimum", "Maximum", "name")

  x <- c("slategray4","firebrick2" )
  names(x) <- c(grey_station, red_station)

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = graph_data, ggplot2::aes(x = Date, ymin = Minimum, ymax = Maximum, fill = name), alpha = 0.5) +
    ggplot2::scale_fill_manual(values = x) +
    ggplot2::scale_y_continuous(breaks=seq(0,100,5),
                                limits = c(0,NA),
                                expand = c(0,0)) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Date", y = "Temperature (C)", fill = NULL) +
    ggplot2::scale_x_date(breaks = seq(min(graph_data$Date),
                                       max(graph_data$Date),
                                       by = "14 days"),
                          date_labels = "%m/%d",
                          expand = ggplot2::expand_scale())






  # Add criteria lines ------------------------------------------------------


  if ((is.null(spawn_crit) & is.null(crit)) |
      ((is.null(spawnstart) | is.null(spawnend)) & is.null(crit))) {

    # If no criteria
    g  <-  g
  } else if (!is.null(crit)  & is.null(spawn_crit)) {

    #If only yearround critieria
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = min(graph_data$Date),
          xend = max(graph_data$Date),
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      )

  } else if (spawnstart < min(graph_data$Date) &
             spawnend >= max(graph_data$Date)) {

    # If data window is only during spawning period
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = min(graph_data$Date),
          xend = max(graph_data$Date),
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      )
  } else if (spawnstart > min(graph_data$Date) &
             spawnend > max(graph_data$Date) &
             spawnend - lubridate::years(1) < min(graph_data$Date)) {

    # If Spawning period goes beyond end of data window
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = min(graph_data$Date),
          xend = spawnstart,
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = max(graph_data$Date),
          xend = spawnstart,
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      )

  } else if (spawnstart > min(graph_data$Date) &
             spawnend < max(graph_data$Date) &
             spawnend - lubridate::years(1) < min(graph_data$Date)) {

    # If spawning in middle of datawindow, and yearround on both sides
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = min(graph_data$Date),
          xend = spawnstart,
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = spawnend,
          xend = max(graph_data$Date),
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = spawnstart,
          xend = spawnend,
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      )
  } else if (spawnstart < min(graph_data$Date) &
             spawnend < max(graph_data$Date)) {

    # If spawning starts before data window, and ends in middle of window
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x =  min(graph_data$Date),
          xend = spawnend,
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = spawnend,
          xend = max(graph_data$Date),
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      )
  } else if (spawnstart > min(graph_data$Date) &
             spawnstart < max(graph_data$Date) &
             spawnend - lubridate::years(1) > min(graph_data$Date)) {

    # If spawning period spans a year
    g <- g +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x =  min(graph_data$Date),
          xend =  spawnend - lubridate::years(1),
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = spawnend - lubridate::years(1),
          xend = spawnstart,
          y = crit,
          yend = crit
        ),
        linetype = "longdash",
        size = 0.7
      ) +
      ggplot2::geom_segment(
        data = graph_data,
        ggplot2::aes(
          x = spawnstart,
          xend = max(graph_data$Date),
          y = spawn_crit,
          yend = spawn_crit
        ),
        linetype = "longdash",
        size = 0.7
      )


  } else {

    #If everything fails, just treat like no cirteria line
    g <- g
  }

  return(g)

}

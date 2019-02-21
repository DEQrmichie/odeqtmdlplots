#' plot_diff_box
#'
#' Crates a monthly boxplot distribution of the daily difference between
#' upsteam and downstream temperatures (station1 - station2). Allows for
#' filtering based on upstream temperature >=  temp_filter. Data source is
#' AWQMS.
#'
#' @param df datafram of data to compare
#' @param station_1 Name of first station (Upstream?)
#' @param station_2 Name of second station (Downstream?)
#' @param station_col Name of column containing the location data. Must be in quotes.
#' @param Date_col Name of column containing the date. Must be in quotes.
#' @param Result_col Name of column containing the results. Must be in quotes.
#' @param temp_filter optional filter for upstream temperature. Plot will only display station_1 temps >=  temp_filter.
#' @examples
#'  plot_diff_box(df = dataframe,
#'   station_1 = "14046778",
#'   station_2 =  "14034470",
#'   station_col = "MLocID",
#'   Date_col = "SampleStartDate",
#'   Result_col= "Result_Numeric",
#`   temp_filter = NULL)
#' @return boxplot showing difference between upstream and downstream temperatures
#'  \if{html}{\figure{diff_box.png}{Plot}}
#'  \if{latex}{\figure{diff_box.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export


plot_diff_box <- function(df,
                          station_1,
                          station_2,
                          station_col = "MLocID",
                          Date_col = "SampleStartDate",
                          Result_col= "Result_Numeric",
                          temp_filter = NULL){


  graph_data <- df[,c(station_col,Date_col, Result_col )]



  colnames(graph_data) <- c("station", "Date", "Result")




  graph_data <- graph_data %>%
    dplyr::mutate(Date = lubridate::ymd(Date)) %>%
    dplyr::mutate(station = ifelse(station == station_1, "Upstream",
                                   ifelse(station == station_2, "Downstream", "ERROR" ))) %>%
    tidyr::spread(key = station, value = Result) %>%
    dplyr::mutate(month = lubridate::month(Date)) %>%
    dplyr::mutate(difference = Upstream - Downstream) %>%
    dplyr::filter(!is.na(difference))




  if(!is.null(temp_filter)){

    graph_data <- graph_data %>%
      dplyr::filter(first_station_name >= temp_filter)
  }



  # Get axis labels ---------------------------------------------------------


  summary <- graph_data %>%
    dplyr::mutate(month = lubridate::month(graph_data$Date, label = TRUE)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(num = n())

  xlabs <- paste(summary$month,"\n(n=",(summary$num),")",sep="")



  # create plot -------------------------------------------------------------

  g <- ggplot2::ggplot(data = graph_data,ggplot2::aes(x = lubridate::month(Date, label = TRUE), y =difference, group = month ) ) +
    ggplot2::geom_boxplot(outlier.shape = 1)+
    ggplot2::geom_hline(yintercept = 0, color = 'darkblue') +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(y = "Change in 7DADA Temperature (C)",
                  x = NULL) +
    ggplot2::scale_x_discrete(labels=xlabs)


  return(g)



}




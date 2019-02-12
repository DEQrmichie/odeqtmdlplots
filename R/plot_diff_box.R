#' plot_diff_box
#'
#' Crates a monthly boxplot distribution of the daily difference between
#' upsteam and downstream temperatures (downstream - upstream). Allows for
#' filtering based on upstream temperature >=  temp_filter. Data source is
#' AWQMS.
#'
#' @param downstream downstream monitoring location
#' @param upstream upstream monitoring location
#' @param startdate Startdate: format = 'yyyy-mm-dd'
#' @param enddate Enddate: format = 'yyy-mm-dd'
#' @examples
#' plot_diff_box(startdate = '2014-01-01', enddate = '2014-12-31', upstream = '14046778', downstream = '14034470', temp_filter = 10)
#' @param temp_filter optional filter for upstream temperature. Plot will only display upstream temps >=  temp_filter.
#' @return boxplot showing difference between upstream and downstream temperatures
#'  \if{html}{\figure{diff_box.png}{Plot}}
#'  \if{latex}{\figure{diff_box.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export


plot_diff_box <- function(downstream, upstream, startdate, enddate, temp_filter = NULL){


station <- c(downstream, upstream)
AWQMS_dat <- AWQMSdata::AWQMS_Data(startdate = startdate,
                                   enddate = enddate,
                                   station = station,
                                   char = 'Temperature, water',
                                   stat_base = '7DADM')


graph_data <- AWQMS_dat %>%
  dplyr::select(MLocID, SampleStartDate, Result_Numeric) %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::mutate(MLocID = ifelse(MLocID == downstream, "Downstream",
                ifelse(MLocID == upstream, "Upstream", "ERROR" ))) %>%
  tidyr::spread(key = MLocID, value = Result_Numeric) %>%
  dplyr::mutate(month = lubridate::month(SampleStartDate),
         difference = Downstream - Upstream)

if(!is.null(temp_filter)){

  graph_data <- graph_data %>%
    dplyr::filter(Upstream >= temp_filter)
}


# Get axis labels ---------------------------------------------------------


summary <- graph_data %>%
  dplyr::mutate(month = lubridate::month(graph_data$SampleStartDate, label = TRUE)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(num = n())

xlabs <- paste(summary$month,"\n(n=",(summary$num),")",sep="")



# create plot -------------------------------------------------------------


g <- ggplot2::ggplot(data = graph_data,ggplot2::aes(x = lubridate::month(SampleStartDate, label = TRUE), y =difference, group = month ) ) +
  ggplot2::geom_boxplot(outlier.shape = 1)+
  ggplot2::geom_hline(yintercept = 0, color = 'darkblue') +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggplot2::labs(y = "Change in 7DADA Temperature (C)",
       x = NULL) +
  ggplot2::scale_x_discrete(labels=xlabs)


# Uncomment this if you want base style boxplots
#
# g <- ggplot2::ggplot(data = graph_data,ggplot2::aes(x = lubridate::month(SampleStartDate, label = TRUE), y =difference, group = month ) ) +
#   ggplot2::geom_boxplot(linetype = "dashed", outlier.shape = 1)+
#   ggplot2::stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
#   ggplot2::stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
#   ggplot2::stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
#   ggplot2::geom_hline(yintercept = 0, color = 'darkblue') +
#   ggplot2::ggplot2::theme_bw() +
#   ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   ggplot2::labs(y = "Change in 7DADA Temperature (C)",
#     x = NULL)+
#   ggplot2::scale_x_discrete(labels=xlabs)


return(g)

}

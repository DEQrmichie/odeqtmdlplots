#' plot_minmax_compare
#'
#' Creates a plot showing the daily minimum and maximum water temperature
#' for 2 monitoring locations. Intended to be used to show the difference
#' between upstream and downstream monitoring locations. If only 1 monitoring
#' location is desired, only include the upstream location. Plot includes
#' 7DADM criteria for upstream location. Data source is AWQMS
#'
#'
#' @param upstream Upstream monitoring location ID
#' @param downstream Downstream monitoring location ID
#' @param startdate Startdate: format = 'yyyy-mm-dd'
#' @param enddate Enddate: format = 'yyy-mm-dd'
#' @param include_legend if FALSE, do not include legend. defaults to TRUE
#' @examples plot_minmax_compare(upstream = '14046778', downstream = '14034470',
#'   startdate = '2014-06-01', enddate = '2014-10-31')
#' @return plot showing dailt min and max 7DADM temperatures, comparied upstream with downstream
#'   \if{html}{\figure{minmax_compare.png}{Plot}}
#'   \if{latex}{\figure{minmax_compare.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export



plot_minmax_compare <- function(upstream = NULL, downstream= NULL,startdate, enddate, include_legend = TRUE){

stations <- c(upstream, downstream)

minmax <- AWQMSdata::AWQMS_Data(startdate = startdate,
                     enddate = enddate,
                     station = stations,
                     char = c('Temperature, water') ,
                     stat_base = c('Minimum', 'Maximum') ,
                     crit_codes = TRUE)

minmax_upstream <- minmax %>%
  dplyr::filter(MLocID == upstream) %>%
  dplyr::left_join(AWQMSdata::Temp_crit, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::left_join(AWQMSdata::LU_spawn, by = "SpawnCode") %>%
  dplyr::left_join(AWQMSdata::LU_FishUse, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::select(OrganizationID, Org_Name,  HUC8, HUC8_Name, AU_ID, MLocID,
                Lat_DD, Long_DD, SampleStartDate,Result_Operator,
                Result_Numeric, Result_Unit, Statistical_Base,FishCode,
                Temp_Criteria, SpawnStart, SpawnEnd, FishUse) %>%
  tidyr::spread(key = Statistical_Base, value = Result_Numeric)


g <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = minmax_upstream, ggplot2::aes(x = SampleStartDate, ymin = Minimum, ymax = Maximum, fill = 'Upstream'), alpha = 0.5) +
  ggplot2::geom_line(data = minmax_upstream, ggplot2::aes(x = SampleStartDate, y = Minimum))+
  ggplot2::geom_line(data = minmax_upstream, ggplot2::aes(x = SampleStartDate, y = Maximum ))+
  ggplot2::geom_hline(yintercept =  dplyr::first(minmax_upstream$Temp_Criteria),
             linetype = "longdash",
             size = 0.7)+
  ggplot2::scale_fill_manual(values = c("Upstream" = "slategray4", "Downstream" = "firebrick2"))+
  ggplot2::scale_y_continuous(breaks=seq(0,100,5),
                              limits = c(0,NA),
                              expand = c(0,0)) +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "Date", y = "Temperature (C)", fill = NULL) +
  ggplot2::scale_x_date(breaks = seq(min(minmax_upstream$SampleStartDate),
                                     max(minmax_upstream$SampleStartDate),
                                     by = "14 days"),
                        date_labels = "%m/%d",
                        expand = ggplot2::expand_scale())


if(!is.null(downstream)){
minmax_downstream <- minmax %>%
  dplyr::filter(MLocID == downstream) %>%
  dplyr::left_join(AWQMSdata::Temp_crit, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::left_join(AWQMSdata::LU_spawn, by = "SpawnCode") %>%
  dplyr::left_join(AWQMSdata::LU_FishUse, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::select(OrganizationID, Org_Name,  HUC8, HUC8_Name, AU_ID, MLocID,
                Lat_DD, Long_DD, SampleStartDate,Result_Operator,
                Result_Numeric, Result_Unit, Statistical_Base,FishCode,
                Temp_Criteria, SpawnStart, SpawnEnd, FishUse) %>%
  tidyr::spread(key = Statistical_Base, value = Result_Numeric)


g <- g +
  ggplot2::geom_ribbon(data = minmax_downstream, ggplot2::aes(x = SampleStartDate, ymin = Minimum, ymax = Maximum, fill = 'Downstream'), alpha = 0.5) +
  ggplot2::geom_line(data = minmax_downstream, ggplot2::aes(x = SampleStartDate, y = Minimum), color = 'red4') +
  ggplot2::geom_line(data = minmax_downstream, ggplot2::aes(x = SampleStartDate, y = Maximum), color = 'red4')

}

if(!include_legend){

  g <- g +
    ggplot2::guides(fill=FALSE)

}

return(g)

}



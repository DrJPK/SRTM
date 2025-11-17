plot_SRTM <- function(
    data){
  data %>%
    tidyr::pivot_longer(
      cols = starts_with("y"),
      names_to = "time",
      values_to = "value"
    ) %>%
    dplyr::mutate(time = factor(time, levels = c("y0", "y1", "y2")))%>%
    ggplot(aes(x=time, y=value, group = ID))+
    geom_point(alpha = 0.4) +
    geom_line(alpha = 0.2) +
    theme_minimal()
}

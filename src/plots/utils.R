# Useful functions for plotting

# Plot fraction of location points at home,
# over time, for a twin, using plotly
at_home_plot <- function (twin, at_home_sum) {
  require(plotly)
  require(ggplot2)
  require(dplyr)
  
  p <-
    at_home_sum %>%
    filter(user_id == twin) %>%
    ggplot(aes(date, at_home_frac, text = paste("Date: ", as.Date(date), "<br>at_home_frac: ", at_home_frac, "<br>N: ", N))) +
    geom_point(aes(color = N), alpha = 0.5) +
    xlab("Date") +
    ylab("Fraction of points at home")
  
  ggplotly(p, tooltip = c("text"))
}

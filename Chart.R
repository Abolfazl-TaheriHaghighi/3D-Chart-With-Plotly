library(plotly)

data <- read.csv("Advertising.csv")


mod <- lm(sales ~ TV * radio, data = data)


tv_vals <- seq(min(data$TV), max(data$TV), length.out = 100)
rad_vals <- seq(min(data$radio), max(data$radio), length.out = 100)

grid <- expand.grid(TV = tv_vals, radio = rad_vals)
grid$sales <- predict(mod, grid)


z_matrix <- matrix(grid$sales, nrow = 100, ncol = 100)


plot_ly() %>%
  add_surface(x = ~tv_vals, y = ~rad_vals, z = ~z_matrix,
              colorscale = 'Greens', opacity = 0.5, showscale = TRUE) %>%
  add_markers(x = ~data$TV, y = ~data$radio, z = ~data$sales,
              marker = list(color = 'red', size = 2 , symbol = '' )) %>%
  layout(scene = list(
    xaxis = list(title = "TV" , range = c(0:300) , color = "black"),
    yaxis = list(title = "Radio" , range = c(0:50), color = "black"),
    zaxis = list(title = "Sales" , range = c(0:30), color = "black"),
    camera = list(eye = list(x = 1.1, y = 1.1, z = 0.8))
  ),
  title = "Sales ~ TV * Radio")




#Abolfazl Taheri Haghighi
#Bachelor's student of statistics at Fasa University
# v1.0.0


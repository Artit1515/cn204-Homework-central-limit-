library(dplyr)
# id 6410742552
mu_id <- 2
min_id <- mu_id - 5
max_id <- mu_id + 5

theoretical_mean <- (max_id + min_id)/2
theoretical_var <- (max_id - min_id)**2 /12
theoretical_sd <- sqrt(theoretical_var)

try <- 1000

small_xbar <- replicate(try,mean(runif(5,min_id,max_id)))
small_xbar

medium_xbar <- replicate(try,mean(runif(50,min_id,max_id)))
medium_xbar

large_xbar  <- replicate(try, mean(runif(500, min_id, max_id)))
large_xbar

compare_df <- data.frame(
  mean = c(mean(small_xbar), mean(medium_xbar), mean(large_xbar)),
  var  = c(var(small_xbar),  var(medium_xbar),  var(large_xbar)),
  sd   = c(sd(small_xbar),   sd(medium_xbar),   sd(large_xbar)),
  theoretical_mean = rep(theoretical_mean, 3),
  theoretical_var  = theoretical_var / c(5, 50, 500),
  theoretical_sd   = theoretical_sd / sqrt(c(5, 50, 500))
)

View(compare_df)



xbar_df <- tibble(
  xbar = c(small_xbar, medium_xbar, large_xbar),
  n = factor(rep(c(5, 50, 500), each = length(small_xbar)))
)


xmin <- min_id
xmax <- max_id
binwidth <- (xmax - xmin) / 40


sdX <- sqrt(((max_id - min_id)^2) / 12)


ymax <- 1.1 * (1 / ((sdX / sqrt(min(c(5, 50, 500)))) * sqrt(2 * pi)))


ggplot(xbar_df, aes(x = xbar)) +
  geom_histogram(aes(y = ..density..),
                 fill = "skyblue", color = "white",
                 binwidth = binwidth, boundary = xmin) +
  geom_vline(xintercept = theoretical_mean, linetype = "dashed", color = "red") +
  facet_wrap(~n, nrow = 1, scales = "fixed") +  
  coord_cartesian(xlim = c(xmin, xmax), ylim = c(0, ymax)) +
  labs(
    title = "Sampling Distributions of Sample Mean (X̄)",
    subtitle = paste0("Uniform(", min_id, ", ", max_id, ") — Dashed line = Theoretical mean = ", theoretical_mean),
    x = expression(bar(X)),
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


library(ggplot2)


xmin <- min_id
xmax <- max_id
binwidth <- (xmax - xmin) / 40
sdX <- sqrt(((max_id - min_id)^2) / 12)
ymax <- 1.1 * (1 / ((sdX / sqrt(5)) * sqrt(2 * pi)))  


plot_xbar <- function(xbar_data, n) {
  ggplot(data.frame(xbar = xbar_data), aes(x = xbar)) +
    geom_histogram(aes(y = ..density..),
                   fill = "skyblue", color = "white",
                   binwidth = binwidth, boundary = xmin) +
    geom_vline(xintercept = theoretical_mean, linetype = "dashed", color = "red") +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(0, ymax)) +
    labs(
      title = paste("Sampling Distribution of X̄ (n =", n, ")"),
      subtitle = paste0("Uniform(", min_id, ", ", max_id, ")"),
      x = expression(bar(X)),
      y = "Density"
    ) +
    theme_minimal(base_size = 14)
}


plot_xbar(small_xbar, 5)
plot_xbar(medium_xbar, 50)
plot_xbar(large_xbar, 500)

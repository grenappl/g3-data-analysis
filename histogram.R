library(ggplot2)

set.seed(123)

x <- rnorm(1000, mean = 50, sd = 10)

p <- ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),
                 bins = 30, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") 
  geom_density(color = "gold", linewidth = 1) +
  labs(title = "Probability Distribution")

print(p)

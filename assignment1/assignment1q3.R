library(MASS)
library(ggplot2)

#question 3.a
mean <- c(1.2, 3.1)
sigma <- matrix(c(1.2, 0.7, 0.7, 3.3), nrow = 2, ncol = 2)
mydata <- mvrnorm(1000, mean, sigma)
#question 3.b
df <- data.frame(mydata[,1], mydata[,2])
p <- ggplot(df, aes(mydata[,1], mydata[,2])) + geom_point(color = "#00bfff", shape = 4, size = 0.1) + 
  coord_fixed(ratio = 1, xlim = c(-2.5,9), ylim = c(-2.5,9))
print(p)
#question 3.c
detM <- det(sigma)
trace <- sum(diag(sigma))
#question 3.d
V_Lamda <- eigen(sigma)
eigenvalues <- V_Lamda$values
eigenvectors <- V_Lamda$vectors
p + stat_ellipse(level = 0.95)
#question 3.e
x <- seq(-6, 8, length = 1000)
y <- dnorm(x, 2.2, 0.8)
qplot(x,y)

y1 <- pnorm(x, 2.2, 0.8)
qplot(x, y1)
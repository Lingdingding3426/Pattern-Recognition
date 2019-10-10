library(ggplot2)
library(grid)


#Question 2.a
fruit_data <- read.table(file = "assigData2.tsv")
W_apl <- fruit_data[,1]
W_orng <- fruit_data[,2]
W_grp <- fruit_data[,3]
D_apl <- fruit_data[,4]
D_orng <- fruit_data[,5]
D_grp <- fruit_data[,6]

W_apl.mean <- mean(W_apl)
W_orng.mean <- mean(W_orng)
W_grp.mean <- mean(W_grp)
D_apl.mean <- mean(D_apl)
D_orng.mean <- mean(D_orng)
D_grp.mean <- mean(D_grp)

W_apl.var <- var(W_apl)
W_orng.var <- var(W_orng)
W_grp.var <- var(W_grp)
D_apl.var <- var(D_apl)
D_orng.var <- var(D_orng)
D_grp.var <- var(D_grp)

#question 2.b
df2 = data.frame(W_apl, W_orng, W_grp)
W <- ggplot(df2) +
  geom_histogram(aes(x = W_apl), col = "green", alpha = 0.3,binwidth = 2)+
  geom_histogram(aes(x = W_orng), col = "red",alpha = 0.3,binwidth = 2)+
  geom_histogram(aes(x = W_grp), col = "blue",alpha = 0.3,binwidth = 2)
print(W)
df3 = data.frame(D_apl, D_orng, D_grp)
D <- ggplot(df2) +
  geom_histogram(aes(x = D_apl), col = "green", alpha = 0.3,binwidth = 20)+
  geom_histogram(aes(x = D_orng), col = "red",alpha = 0.3,binwidth = 20)+
  geom_histogram(aes(x = D_grp), col = "blue",alpha = 0.3,binwidth = 20)
print(D)

#question 2.c
RW_apl <-round(fruit_data[,1])  
RW_orng <-round(fruit_data[,2])  
RW_grp <-round(fruit_data[,3])  
W_all = RW_apl + RW_orng + RW_grp

df <- data.frame(W_all)
p <- ggplot(df, aes(sample = W_all))
p + stat_qq() + stat_qq_line()



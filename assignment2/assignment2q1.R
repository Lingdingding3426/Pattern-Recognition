library(readxl)
library(ggpubr)

Psipred_data <- read_excel("assigData1.xls", sheet = 2)

Q3 <- as.numeric(unlist(Psipred_data[,4]))
CC_AVG <- as.numeric(unlist(Psipred_data[,3]))
#Classical statistical test
n <- length(Q3)
ggqqplot(Q3, ylab = "Q3") #see if Q3 is close to normal distribution
ggqqplot(CC_AVG, ylab = "CC_AVG") #see if CC_AVG is close to normal distribution
r <- cor(Q3, CC_AVG, method = "spearman")
print(r)
Sr <- sqrt((1-r^2)/(n-2))
t <- r/Sr
p_value <- dt(abs(t),n-2)
print(p_value)

#Permutation test
PT <- vector()
sum <- 0
for (i in 1:1000)
{
  CC_NEW <- sample(CC_AVG, length(CC_AVG))
  PT <- c(PT, cor(Q3, CC_NEW, method = "spearman"))
  if (PT[i] < r){
    sum  = sum + 1
  }
}
if (sum/1000 > 0.95){
  print("significant correlated")
  print(sum)
} else {
  print("not correlated")
  print(sum)
}

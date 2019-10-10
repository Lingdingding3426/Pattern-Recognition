library(readxl)
library(ggplot2)

# read data
Pci_Data <- read_excel("assigData1.xls", sheet = 1)
Psipred_Data <- read_excel("assigData1.xls", sheet = 2)

#question 1.a
Q3_1 <- as.numeric(unlist(Pci_Data[,4]))  
Q3_2 <- as.numeric(unlist(Psipred_Data[,4]))
pLength <- as.numeric(unlist(Pci_Data[,2]))

df1 <- data.frame(pLength, Q3_1, Q3_2)
p <- ggplot(df1) + 
  geom_point(df1, mapping = aes(x = pLength, y = Q3_1), color = "purple") +
  geom_point(df1, mapping = aes(x = pLength, y = Q3_2), color = "red")
print(p)

#question 1.b
#PCI Q3 and Length corelation
x1_mean <- mean(Q3_1)
y1_mean <- mean(pLength)
Mole1_x_y <- sum((Q3_1 - x1_mean)*(pLength - y1_mean))
Deno1_x_y <- sqrt(sum((Q3_1 - x1_mean)^2)*sum((pLength - y1_mean)^2))
cor1_x_y <- Mole1_x_y/Deno1_x_y 
#PSIPRED Q3 and Length corelation
x2_mean <- mean(Q3_2)
Mole2_x_y <- sum((Q3_2 - x2_mean)*(pLength - y1_mean))
Deno2_x_y <- sqrt(sum((Q3_2 - x2_mean)^2)*sum((pLength - y1_mean)^2))
cor2_x_y <- Mole2_x_y/Deno2_x_y 

#question 1.c
#PCI CC 
CC1 <- as.numeric(unlist(Pci_Data[,3]))
CC1.mean = mean(CC1)
CC1.median = median(CC1)
CC1.sd = sd(CC1)
#PSIPRED CC
CC2 <- as.numeric(unlist(Psipred_Data[,3]))
CC2.mean = mean(CC2)
CC2.median = median(CC2)
CC2.sd = sd(CC2)
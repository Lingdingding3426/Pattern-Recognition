library(ggplot2)

fruit_data <- read.table(file = "assigData2.tsv")
#Question 2.a

W_apl <- fruit_data[,1]
W_orng <- fruit_data[,2]
W_grp <- fruit_data[,3]
D_apl <- fruit_data[,4]

skew_test <- function(A,B){
  if(mean(A) > median(A)){
    cat(B,"is right skewed")
  } else if(mean(A) < median(A)){
    cat(B,"is left skewed")
  } else {
    print("it is symmertical")
  }
}
skew_test(W_apl, "Apple's weight")
skew_test(D_apl,"Apple's diameter")
a = data.frame(W_apl,D_apl)
W <- ggplot(a)+
  geom_histogram(aes(x = W_apl),col = "blue", alpha = 0.3,binwidth = 0.1)
print(W)
D <- ggplot(a)+
  geom_histogram(aes(x = D_apl),col = "green", alpha = 0.3,binwidth = 3)
print(D)

#Question 2.b
outlier_test <- function(A,B){
  test <- 0
  Q <- quantile(A, c(0.25, 0.75))
  IQR <- Q[2] - Q[1]
  for(i in length(A)){
    if(A[i] > Q[2] + IQR|| A[i] < Q[1]-IQR){
      test <- 1
      A[i] <- 0
    }
  }
  if(test == 1)
  {
    cat(B,"contains outliers")
  } else {
    cat(B, "does not have outliers")
  }
  A <- A[A!=0]
  return(A)
}

NEW_Wapl <- outlier_test(W_apl,"Apple's weight")
NEW_Worng <- outlier_test(W_orng,"orange's weight")
NEW_Wgrp <- outlier_test(W_grp,"grape's weight")

#Question 2.c
min(D_apl)
max(D_apl)
range(D_apl)
Q1 <- quantile(D_apl, c(0.25, 0.75))
IQR <- Q1[2]-Q1[1]
IQR



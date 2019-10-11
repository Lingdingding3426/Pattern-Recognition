library(readxl)

#question 3.a
My_data <- read_excel("assigData4.xlsx")

fix(My_data)

Frame_data <- data.frame(My_data)

as.matrix(Frame_data)

chisq.test(Frame_data$X0.5,Frame_data$X6.8,
           Frame_data$X9.10)

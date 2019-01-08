
#Market Mix Model
sales <- c(37, 89, 82, 58, 110, 77, 103, 78, 95, 106, 98, 96, 68, 96, 157, 198, 145, 132, 96, 135)
ad <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)

modFit.0 <- lm(sales~ad)
summary(modFit.0)

#Adding adstock tranformation
ad.adstock <- as.numeric(filter(x=ad, filter=.50, method="recursive"))

modFit.1 <- lm(sales~ad.adstock)
summary(modFit.1)

#With two marketing variables
sales <- c(37, 89, 82, 58, 110, 77, 103, 78, 95, 106, 98, 96, 68, 96, 157, 198, 145, 132, 96, 135)
ad1 <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)
ad2 <- c(3, 0, 4, 0, 5, 0, 0, 0, 8, 0, 0, 5, 0, 11, 16, 11, 5, 0, 0, 15)

ad1.adstock <- as.numeric(filter(x=ad1, filter=.3, method="recursive"))
ad2.adstock <- as.numeric(filter(x=ad2, filter=.3, method="recursive"))

modFit2 <- lm(sales~ad1.adstock+ad2.adstock)
summary(modFit2)

########
sales <- c(37, 89, 82, 58, 110, 77, 103, 78, 95, 106, 98, 96, 68, 96, 157, 198, 145, 132, 96, 135)
ad1 <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)
ad2 <- c(3, 0, 4, 0, 5, 0, 0, 0, 8, 0, 0, 5, 0, 11, 16, 11, 5, 0, 0, 15)
trend <- 1:20

ad1.adstock <- as.numeric(filter(x=ad1, filter=.3, method="recursive"))
ad2.adstock <- as.numeric(filter(x=ad2, filter=.3, method="recursive"))

modFit.3 <- lm(sales~trend+ad1.adstock+ad2.adstock)
summary(modFit.3)


setwd("~/")

library("xlsx")
library("lmtest")
library("ggplot2")


cn <- read.xlsx("Ctrl.xlsx", 1)
ck <- read.xlsx("cko.xlsx", 1)
View(cn) 
View(ck)

cn <- data.frame(cn = cn)
names(cn) <- c("1","2","3","4","5")
cn1 <- c(na.omit(cn[,1]), na.omit(cn[,2]), na.omit(cn[,3]),
         na.omit(cn[,4]), na.omit(cn[,5])) 

ck <- data.frame(ck = ck)
names(ck) <- c("1","2","3","4","5")
ck1 <- c(na.omit(ck[,1]), na.omit(ck[,2]), na.omit(ck[,3]),
         na.omit(ck[,4]), na.omit(ck[,5]))

a <- data.frame(y = cn1)
a$x <- "control"
b <- data.frame(y = ck1)
b$x <- "cko"
d <- rbind(a,b)

d$x <- as.factor(d$x)
levels(d$x)
d$x <- relevel(d$x, ref = "control")

View(d)


head(cn1)
length(cn1) 
mean(cn1) 
var(cn1) 
head(ck1)
length(ck1) 
mean(ck1)
var(ck1) 


t.test(d$y[d$x == "control"], d$y[d$x == "cko"])
t.test(d$y[d$x == "control"], d$y[d$x == "cko"], alternative = "less")

lm1 <- lm(d$y ~ d$x)
summary(lm1)

ks.test(d$y[d$x == "control"], d$y[d$x == "cko"])

ks.test(d$y[d$x == "control"], d$y[d$x == "cko"], 
        alternative = "less")
ks.test(d$y[d$x == "control"], d$y[d$x == "cko"], 
        alternative = "greater")


m <- as.matrix(summary(lm1)$coeff)
m1 <- as.data.frame(m)
write.xlsx(m1, "Name.xlsx")









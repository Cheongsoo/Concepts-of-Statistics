#binomial distribution
dbinom(3, 10, 0.5)
dbinom(3, 10, 0.4)

#hypergeometric
dhyper(3, 4, 6, 10)
dhyper(4, 4, 6, 10)
dhyper(4, 8, 12, 10)
dhyper(4, 24, 36, 10)


a=100
approx <- numeric(length=a)
for(i in 1:a){
    approx[i]=dhyper(3, 4*i, 6*i, 10)
}

approx

plot(approx[2:100])
abline(h=dbinom(3, 10, 0.4), col="red")

#hyper close to binom as # increase

approx-dbinom(3, 10, 0.4)


#pakage, summary, aggregate
install.packages("MASS")
library(MASS)
data("Aids2")
str(Aids2)
?Aids2
head(Aids2)
summary(Aids2)
which(Aids2$age==0)
Aids2[which(Aids2$age==0),]

Alive <- Aids2[which(Aids2$status=="A"),]
Dead <- Aids2[which(Aids2$status=="D"),]
aggregate(Alive$age, by=list(Alive$sex), mean)
aggregate(Dead$age, by=list(Dead$sex), mean)

aggregate(Aids2$age, by=list(Aids2$sex, Aids2$status), mean)


#plot option
x <- c(1:10)
y <- x^2-x+10


par(mfrow=c(2,4))
for(i in 1:8){
plot(x, y, type="p", col="blue", pch=i)
}

types=c("p", "l", "o", "b", "c", "s", "S", "h")
for(i in 1:8){
  plot(x, y, type=types[i], col="blue", pch=i)
}

#conditional statement
#if() {} else if {} else {}
mtcars
mean_by_cyl(4)

mean_by_cyl2 <- function(x) {
  mean(mtcars[which(mtcars$cyl==x),][,1])
}

mean_by_cyl2(7)

aggregate(mtcars$mpg, by=list(mtcars$cyl), mean)
mean_by_cyl <- function(x) {
 if(x==4) { a <- round(mean(mtcars[which(mtcars$cyl==4),][,1]),2)
          return(paste('The avg mile per gallon of',x,'cylinder car is', a))
aggregate(mtcars$mpg, by=list(mtcars$cyl), mean)[1,2]
} else if(x==6) { b <- round(mean(mtcars[which(mtcars$cyl==6),][,1]),2)
  return(paste('The avg mile per gallon of',x,'cylinder car is', c))
   
} else if(x==8) { c <- round(mean(mtcars[which(mtcars$cyl==8),][,1]),2)
  return(paste('The avg mile per gallon of',x,'cylinder car is', c))
} else{print('Wrong number!')}
}
#RMySQL, SQL
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='5553', dbname='sampdb')
mydb
result <- dbGetQuery(mydbm 'show tables;')
result

dbGetQuery(mydb, 'select count(*) from president;')
tbl <- dbGetQuery(mydb, 'select * from president;')
tbl
tbl$name <- paste(tabl$first_name, tbl$last_name, tbl$suffix)

newdf <- tbl[c(8,4:7)]
newdf$name <- gsub(' NA', '',newdf$name) #delete the suffix(NA)
str(newdf) #all chr
summary(newdf) # doesn't mean anything
newdf$state <- as.factor(newdf$state)
newdf$birth <- as.Date(newdf$birth)
newdf$death <- as.Date(newdf$death)
str(newdf)
summary(newdf)
dbDisconnect(mydb)
#newdf still exists

#R markdown&matrix
#matrix determinant
C <- matrix(c(1,-1,2,3), nrow=2)
C
def_f <- function(A){
  D <- A[1,1]*A[2,2]-A[1,2]*A[2,1]
  return(D)
}
def_f(C)
det(C)

inv_f <- function(A){
  A[1,1] <- A[2,2]
  A[2,2] <- A[1,1]
  A[1,2] <- -A[1,2]
  A[2,1] <- -A[2,1]
  return(A)
}

inv_f(C)
C%*%((1/def_f(C))*inv_f(C))

inv_f <- function(A){
  B <- matrix(nrow = 2, ncol = 2)
  B[1,1] <- A[2,2]
  B[2,2] <- A[1,1]
  B[1,2] <- -A[1,2]
  B[2,1] <- -A[2,1]
  return(B)
}


inv_f <- function(A){
  D <- A[1,1]*A[2,2]-A[1,2]*A[2,1]
  B <- matrix(nrow = 2, ncol = 2)
  B[1,1] <- A[2,2]
  B[2,2] <- A[1,1]
  B[1,2] <- -A[1,2]
  B[2,1] <- -A[2,1]
  I=1/D*B
  inv <- list()
  inv$det <- D
  inv$I <- I
  return(inv)
}

C%*%((1/inv_f(C)$det)*inv_f(C)$I)

D <- inv_f(C)$det
D
I <- inv_f(C)$I
I
C%*%I
solve(C)
round(C%*%solve(C),10)

#eigendecomposition A=eigenVector%*%eigenValueDiagmatrix%*%eigenVector_inverse
#only square matrix
A <- matrix(c(3,1,0,2,1,0,-2,-2,1), nrow=3)
A

ev <- eigen(A)$values
evec <- eigen(A)$vectors
A-lambda*E
diag(ev)
evec%*%ev%*%solve(evec)
evec%*%diag(ev)%*%solve(evec)

trans <- function(A){
    B <- matrix(nrow=nrow(A), ncol=ncol(A))
    for (i in 1:nrow(A)){
        for (j in 1:ncol(A)){
              B[j,i]<- A[i,j]
        }
    }
    return(B)
    
}
trans(A)
A
trans(trans(A))==A
t(A)
# symmetric matrix and eigen vectors orthogonal P%*%P_inverse=E
C <- matrix(c(3,2,-2,2,1,-2,-2,-2,1), nrow=3)
C==trans(C)
eigen(C)
eigen(C)$vectors%*%diag(eigen(C)$values)%*%solve(evec)


#ggplot, linear regression model
#1. Build Data frame
age <- c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37)
maxHR <- c(202, 186, 187, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178)
df <- data.frame(age, maxHR)
df

#2. Linear Regression model
lm_result <- lm(maxHR ~ age, data=df)
lm_result
paste(1, "hi")
print(paste())
#-0.7977age + 210.0485 = maxHR

#3.Visualization
library(ggplot2)
ggplot(df, aes(age, maxHR)) + geom_point() + xlab("AGE") + ylab("Maximum Heart Rate") +
  ggtitle("Relation between Maximum Heart Rate and Age") +
  stat_smooth(method=lm, level=0.95)

year <- c(2000, 2001, 2002, 2003, 2004, 2005)
value <- c(2.3, 3.2, 5.6, 5.4, 5.8, 100)
plot(year, value, ylim=c(0, 150))

fit <- lm(value ~ year)
abline(fit, col="red")

fit$coefficients[[1]]
fit$coefficients[[2]]

# value= 0.92year - 1837.38
fit$residuals
summary(fit)

plot(year, value, xlim=c(2000, 2020),ylim=c(0, 150))
fit$coefficients[[1]]
fit$coefficients[[2]]

# value= 0.92year - 1837.38
fit$residuals
summary(fit)

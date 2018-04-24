library(tidyr)
library(dplyr)

member <- data.frame(family=c(1,2,3), namef=c("a","b","c"),
                     agef=c(30,40,23), namem=c("d","e","f"),
                     agem=c(44,53,25))
member

member[1]

a <- gather(member, key, value, namef:agem)
b <- separate(a, key, c("variable", "type"), -1)
new <- spread(b, variable, value)
new

filter(new, type=="f")
filter(new, age >= 30)

member
select(member, family, namef, agef)

#pipeline
new_member <- member %>%
    gather(key, value, namef:agem) %>%
    separate(key, c("variable", "type"), -1) %>%
    spread(variable, value)
new
new_member


y <- character()
y2 <- ""

y <- character(10)
y
y[3] <- "third"
y[12] <- "twelveth"

as.character(c(1:4))

hi <- paste("Hi", "Jack")
hi
paste("jac","k", sep="")
paste(hi, "what's up?", sep=", ")

paste("1", 1:10, sep="-")
a <- paste("The value of 'pi' is", pi, ", endless!!!")
noquote(a)
print(a, quote=FALSE)


mtcars
cars <- rownames(mtcars)
cars

nchar(cars)
which(nchar(cars)==max(nchar(cars)))
cars[which(nchar(cars)==max(nchar(cars)))]


grep("z", cars, value=TRUE)
grep("[vV]", cars, value=T)
grep("t", tolower(cars), T)
grep("t", tolower(cars), value=T)
grep("TOYOTA", toupper(cars), value=T)
grep("toyota", tolower(cars), value=T)

library(stringr)
cars
str_count(cars, "t")
str_count(toupper(cars), "TOYOTA")
sum(str_count(toupper(cars), "TOYOTA"))



#normal distribution mean=80, sd=10
x <- seq(40, 120, length=300)
x
y <- dnorm(x, mean=80, sd=10)
plot(x,y, type="l", col="red")
lines(x, dnorm(x, mean=80, sd=5), col="blue")

#a.probability between 65~75
x2 <- seq(65, 75, length=200)
y2 <- dnorm(x2, mean=80, sd=10)
polygon(c(65,x2,75), c(0, y2, 0), col="gray")

pnorm(75, mean=80, sd=10) - pnorm(65, mean=80, sd=10)

#b.probability of over 92
1 - pnorm(92, mean=80, sd=10, lower.tail=TRUE)
pnorm(92, mean=80, sd=10, lower.tail=F)

#c.probability of less than 68
pnorm(68, mean=80, sd=10)

#d.cutoff that separtes the bottom 30%
qnorm(0.3, mean=80, sd=10)

#e.80th percentile
qnorm(0.8, mean=80, sd=10)

#f.cutoff that contain the middle 60%
qnorm(0.8, mean=80, sd=10)
qnorm(0.2, mean=80, sd=10)
80-qnorm(0.8, mean=80, sd=10)
80-qnorm(0.2, mean=80, sd=10)


strsplit("dfadfqwetqet", split="w")
a <- strsplit("how are you?", split="")
paste(a)
class(a)

paste(a[[1]], collapse="")

reversed <- a[[1]][12:1]
reversed
paste(reversed, collapse="")

reverse_myf <- function(string){
  a <- strsplit(string, split="")
  reversed <- a[[1]][nchar(string):1]
  paste(reversed, collapse="")
}

reverse_myf("how are you?, Jack, emily")


a <- strsplit("how are you?", split=" ")
a
rev_word <- function(string){
              a <- strsplit(string, split=" ")
              str_length <- length(a[[1]])
              reversed <- a[[1]][str_length:1]
              paste(reversed, collapse=" ")
}

rev_word("how are you?, what are you doing?")

#
dotchart
x <- 1:10
y <- x-1
plot(y~x)
plot(mtcars$mpg)
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=0.7)

carmpg <- mtcars[order(mtcars$mpg),]
carmpg
#data[order()]
carmpg$cyl <-factor(carmpg$cyl)
dotchart(carmpg$mpg, labels=row.names(carmpg), cex=0.7)

carmpg$color[carmpg$cyl==4] <- "blue"
carmpg$color[carmpg$cyl==6] <- "green"
carmpg$color[carmpg$cyl==8] <- "red"

dotchart(carmpg$mpg, labels=row.names(carmpg),
         cex=0.7, color = carmpg$color)
#divide by group
dotchart(carmpg$mpg, labels=row.names(carmpg),
         groups=carmpg$cyl, cex=0.7, 
         color = carmpg$color, xlab="Miles Per Gallon",
         main="Mileage depending on numbers of cylinder")

#Chi-square test
data <- matrix(c(42,30,50,87), nrow=2, byrow=FALSE)
data
#H0 Not much difference

chisq <- function(Obs){
        Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs)
        return(Expected)
        }

data
chisq(data)
rs <- rowSums(data)
cs <- colSums(data)

outer(rs,cs)
117*72

chisqr <- function(Obs){
  Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}

chisqr(data)

1-pchisq(9.132947,1)
#df (2-1)*(2-1)
#Reject H0

chisq.test(data)# different answer due to continuity correction since there's small #
chisq.test(data, correct=FALSE)

#t-apply ??????????????? ???????????? ?????????
mtcars
str(mtcars)
newdata <- mtcars[1:2]
newdata

newdata$cyl==4
newdata[2,]
#True??? ???????????? ????????????
mpg_4 <- newdata[which(newdata$cyl==4),]
mpg_6 <- newdata[which(newdata$cyl==6),]
mpg_8 <- newdata[which(newdata$cyl==8),]

mean(mpg_4$mpg)
mean(mpg_6$mpg)
mean(mpg_8$mpg)

cbind(mean(mpg_4$mpg),mean(mpg_6$mpg),mean(mpg_8$mpg))

#tapply(x, index, ?)
#?tapply

tapply(newdata$mpg, newdata$cyl, mean)
tapply(mtcars$mpg, mtcars$cyl, mean)
tapply(mtcars$mpg, mtcars$gear, mean)


#ggvis, graph package
install.packages("ggvis")
library(ggvis)

mtcars

plot(mtcars$mpg, mtcars$wt)

mtcars
attach(mtcars)
plot(mpg, wt)

mtcars %>% ggvis( ~mpg, ~wt) %>% layer_points()
mtcars %>% ggvis( ~mpg, ~wt) %>% layer_lines()
mtcars %>% ggvis( ~mpg, ~wt) %>% layer_bars()
mtcars %>% ggvis( ~mpg, ~wt) %>% layer_smooths()

#????????? ?????? ????????????
mtcars %>% ggvis( ~mpg, ~wt) %>% layer_points() %>% layer_smooths()

mtcars %>% ggvis( ~mpg, ~wt, fill:="red") %>% layer_points() %>% layer_smooths()

mtcars %>% ggvis( ~mpg, ~wt, fill="cyl") %>% layer_points() %>% layer_smooths()

mtcars %>% ggvis( ~mpg, ~wt, fill= ~cyl) %>% layer_points() %>% layer_smooths()
#???????????? continuous??? ???????????? ????????? ????????? ??????
str(mtcars)
#cyl=num ???????????? ????????? ??????

mtcars$cyl <- factor(mtcars$cyl)
str(mtcars)

mtcars %>% ggvis( ~mpg, ~wt, fill= ~cyl) %>% 
            layer_points() %>% layer_smooths() %>%
            add_axis("x", title = "MPG", values = c(0:35))

mtcars %>% ggvis( ~mpg, ~wt, fill= ~cyl) %>% 
  layer_points() %>% layer_smooths() %>%
  add_axis("x", title = "MPG", values = c(10:35)) %>%
  add_axis("y", title = "WT", subdivide = 4)




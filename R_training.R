setwd('/data/app/gcbcofflan/sg62708/training')
customer <- read.csv("newcustomers.csv", stringsAsFactors = TRUE)
colnames(customer)



a <- list('a','b','c')
b <- list('c', 'd', 'e')

union(a,b)
       
intersect(a,b)
setdiff(a,b)
       
setdiff(b,a)
       
is.element(1:10,2)
       
1:10 %in% 2

keycode1 <- c("A1","A3","AA1")
keycode2 <-c("AA2","AA6")





keycode <- union(keycode1, keycode2)



matches <- match(customer$Keycode,keycode, nomatch=0)
length(matches) 






matches[1:500]

table(matches)





customer.subset <- customer[matches!=0,]

nrow(customer.subset)


matches <- is.element(customer$Keycode,keycode)

matches[1:500]




matches <- is.element(customer$Keycode,keycode)

matches[1:500]

sum(matches)



customer.subset <- customer[matches,]

nrow(customer.subset)

#table(customer.subset$Keycode)






editvec <- function(x) {
  y <- (x+5)^2   # do our thing
  y  # the result of the last statement before exiting is returned by the function
}



class(editvec)

editvec(1:20)




newvec <- editvec(1:20)

newvec

class(newvec)






my.simple.imputation <- function(x, replacement=NULL) {
  if (!is.numeric(x)) stop("You must pass in a numeric vector")
  if (!is.numeric(replacement) & !is.null(replacement)) stop("You must pass in a numeric replacement value, or NULL to use the mean")
  if (is.null(replacement)) replacement <- mean(x, na.rm=TRUE)
  x <- ifelse(is.na(x), replacement, x)
  x
}


my.simple.imputation(c(1:10,NA,11:20))

my.simple.imputation(c(1:10,NA,11:20), replacement=0)





my.simple.imputation <- function(x, replacement=NULL) {
  if (!is.numeric(x)) stop("You must pass in a numeric vector")
  if (!is.numeric(replacement) & !is.null(replacement)) stop("You must pass in a numeric replacement value, or NULL to use the mean")
  if (is.null(replacement)) replacement <- mean(x, na.rm=TRUE)
  x <- ifelse(is.na(x), replacement, x)
  x
}


customer$TotalSpending2 <- my.simple.imputation(customer$TotalSpending)
head(data.frame(customer$TotalSpending, customer$TotalSpending2))

customer$TotalSpending2 <- my.simple.imputation(customer$TotalSpending, replacement=0)
head(data.frame(customer$TotalSpending, customer$TotalSpending2))


segments<-unique(customer$Segment)

customer$FutureValue <- round( (100-customer$Age)*100 +
                                  50 * match(customer$Segment, segments) +
                                  rnorm( nrow ( customer),0,350) 
)


plot(customer$Age,
     customer$FutureValue, 
     type="p", 
     pch=1, 
     col="black", 
     xlab="Age", 
     ylab="Estimated Future Value",
     main="My Plot")



my.colors <- c(A="red",B="blue",C="green",N="brown")
my.symbols <- c(A=21, B=22, C=23, N=24)
my.colors["A"]
my.symbols[c("A","A","B","C","N","B","B")]





with(customer,{
  plot(Age, 
       FutureValue, 
       type="p",
       bg=my.colors[Segment],
       pch=my.symbols[Segment], 
       main="Future Value versus Age")
  
  legend(80,9000,
         legend=segments,
         pt.bg=my.colors,
         pch=my.symbols,
         title="Segment",
         yjust=1,
         bty="n")
}
)


means <- tapply(customer$FutureValue, customer$Segment, mean, na.rm=TRUE)

bar.color <- rgb(red=17, blue=107, green=64, maxColorValue=255)


bar.xpositions <- barplot(means, names.arg=names(means), 
                          col=bar.color, 
                          border="dark blue",
                          ylim=c(0,1.25*max(means, na.rm = TRUE)),
                          main="Future Value Analysis",
                          xlab="Segment",
                          ylab="Mean Estimated Future Value"
                          )



axis(1,at=bar.xpositions,labels=unique(customer$Segment))


hist(customer$Age, 
     breaks=5,
     col=bar.color, 
     xlab="Age", 
     ylab="Counts", 
     main="Histogram for Age")

age.density <- density(customer$Age, na.rm=TRUE)

plot(age.density, 
     xlab="Age",
     ylab="Counts",
     main="Estimated Density for Age") 




x <- rnorm(1000,mean=20,sd=5)
y <- rnorm(1000,mean=15,sd=3)
z <- rnorm(1000,mean=25,sd=8)


install.packages("scatterplot3d", dependencies = TRUE)


library(scatterplot3d)
scatterplot3d(x,y,z)


pairs(subset(customer,TRUE,c("FutureValue","Age","PreviousOrders")), 
      pch=21, 
      bg=my.colors[match(customer$Segment,segments)])


pairs(subset(customer,TRUE,c("FutureValue","Age","PreviousOrders")),
      pch=21,
      bg=my.colors[match(customer$Segment,segments)])


library(lattice)
xyplot(FutureValue~Age,data=customer,
       xlab="Age",
       ylab="Estimated Future Value",
       main="My Trellis Graph")
xyplot(FutureValue~Age|Segment,
       data=customer,
       xlab="Age",
       ylab="Estimated Future Value",
       main="My Trellis Graph by Segment")
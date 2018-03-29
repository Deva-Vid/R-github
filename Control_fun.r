#IF STATEMENT

# Variables related to your last day of recordings
medium <- "LinkedIn"
num_views <- 14

# Examine the if statement for medium
if (medium == "LinkedIn") {
  print("Showing LinkedIn information")
}

# Write the if statement for num_views
if (num_views > 15) {
  print("You're popular!")
}


# Variables related to your last day of recordings
medium <- "LinkedIn"
num_views <- 14

# Control structure for medium
if (medium == "LinkedIn") {
  print("Showing LinkedIn information")
} else {
  print("Unknown medium")
}

# Control structure for num_views


#Vectorized IF
x <- seq(1,by=1,length.out = 10)
x
seq_along(x)


y <- ifelse(x<5,x,2)
y

# Variables related to your last day of recordings
medium <- "Facebook"
num_views <- 14

# Control structure for medium
if (medium == "LinkedIn") {
  print("Showing LinkedIn information")
} else if (medium == "Facebook") {
  print("Showing Facebook information")
} else {
  print("Unknown medium")
}

# Control structure for num_views
if (num_views > 15) {
  print("You're popular!")
} else if (num_views <= 15 & num_views > 10) {
  print("You are getting there !!!")  
} else {
  print("Try to be more visible!")
}



rnorm(20)


#FOR LOOP
#Squaring the values of a randomly generated vector
a <- rnorm(20,10,1)       #random number generation of 20 values with Mean=10 and SD=1
a
#Understanding that i takes the values that you provide 
#in this case it is vector x
asq1 <- 0
x<- c(20,90,100,400)
for(i in x)
{
  asq1<- asq1+i
  print(i)
  }

#Squaring the vector a using the loop
asq1 <- NULL
x<- seq(1,20,1)
x
for(i in x) 
{
  asq1[i]<- a[i]*a[i]
}


#Print the values
asq1

#Another way to do the same thing
asq1 <- 0
for(i in seq(1,length(a),1))
{
  #asq1<- a[i]*a[i]................??
  asq1 <- c(asq1,a[i]*a[i])
}
print(asq1)

#Easiest way to do it
a<- a*a
a<- a^2

#Quick revision
#Matrix multiplication in R
mymat1= matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
mymat2= matrix(c(5,6,7,8), nrow=2, ncol=2, byrow=TRUE)
compo_wise_result <- mymat1*mymat2
Mat_mul_result <- mymat1%*%mymat2

#NESTED FOR LOOP
mymat= matrix(nrow=3, ncol=3)
class(dim(mymat)[1])
dim(mymat)[2]

#How to create a 3x3 matrix whose each component is the 
#multiplication of its row number and column number
my_mat<-matrix(nrow=3,ncol=3)
for(i in 1:3)
{
 for(j in 1:3)
 {
  my_mat[i,j]= i*j 
 }
}


#WHILE LOOP
# Initialize the speed variable
speed <- 64

# Code the while loop
while (speed > 30) {
  print("Slow down!")
  speed <- speed - 7
}

# Print out the speed variable
speed


# Initialize the speed variable
speed <- 64

# Extend/adapt the while loop
while (speed > 30) {
  print(paste("Your speed is",speed))
  if (speed > 48) {
    print("Slow down big time!")
    speed <- speed - 11
  } else {
    print("Slow down!")
    speed <- speed - 6
  }
}


# Initialize the speed variable
speed <- 88
while (speed > 30) {
  print(paste("Your speed is", speed))
  # Break the while loop when speed exceeds 80
  if (speed > 80) {
  break
  }
  if (speed > 48) {
    print("Slow down big time!")
    speed <- speed - 11
  } else {
    print("Slow down!")
    speed <- speed - 6
  }
}


#REPEAT
z <- function()
{
  readline(prompt="Please, enter  value: ")
}


repeat
{
  response<-as.integer(z());
  if (response >= 40)
  {
    print("Well done!");
    break
  } else print("Sorry, the answer to whaterver the question MUST be greater than 40");
}


#NEXT
m=20;
for (k in 1:m)
{
  if (!(k%%2))
    next
  print(k)
}

mtcars
mtcars[order(mtcars$mpg)]
order(mtcars$mpg)
my <-matrix(1:16,nrow = 4,ncol = 4,byrow = FALSE)
my
t<-list(a=2.3 ,b=4.5,c=6.7 ,d=8.4 ) # list 
t
fm <- function(x) {
2*(x)  
}
fm(4)
op<- lapply(t,fm)
t

lapplybt<-function(y,j=2){
     j*(y)
}
output<-lapply(t,bt,j=4) #......use fun to pass double argument
output
 bt(t,2)
seq(10:4)
seq(10,4)
# matrix using  sequence function
D <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7)
hp<-matrix(D,nrow = 7,ncol = 4,byrow = TRUE)
hp
rep(seq(1,7),4) # repeat function and sequence function
hr<-matrix(rep(seq(1,7),4),nrow = 7,ncol = 4,byrow = FALSE)
hr
kf <- data.frame(hr) # converting matix to data frame
kf
class(kf)
lapply(kf,mean) # column wise mean for data frame
class(kf)
is.vector(kf$X1)
replicate(8,seq(1,4))
class(replicate)
D <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7)
replicate(12,sum(D))

dl<- function(y){
 
  set.seed(2)
    rnorm(y)
  
}
replicate(6,sum(dl(y=10)))

hist(x1)
x1<- runif(16)
cat1 <- rep(1:4, (length(x1)/4))
cat2 <- c(rep(1, 8), rep(2, 8))
cat3 <- rep(1:4, (length(x1)/4))
cat4 <- rep(1:8, 2)
s <- data.frame(x1)
class(s)
s
# adding column in data frame
v2<- cbind(s,cat1)
v2
# deleting from data frame
v3 <- v2[,-1]
v3
x <- c(1:5, 5:3)
x
#order & sort  in order you have to use  indexing  and subsetting in case of sort you can directly 
# apply on data 
ls(pos = "package:dplyr")
ls()
installed.packages()
install.packages("dplyr")  # to installed packages
"dplyr" %in% installed.packages()
ls()
search()
library(dplyr)  # to load the packages
install.packages("hflights")
library(hflights)
"hflights" %in% installed.packages()
data(packaage = "hflights")
data(hflights)
fix(hflights)  # data editor window
head(hflights)
summary(hflights)
fg<-c("A"="carrier","B" = "weather","C" = "national air system","D"= "security","p" = "Not canceled")
hflights$CancellationCode[hflights$CancellationCode == ""] <- "p"
fg
dr <- select(hflights,UniqueCarrier, contains("time"),ends_with("num"))

dim(dr)
dr2 <- select(hflights, contains("time"),starts_with("Arr"))
dr2
dim(dr2)
hflights[c("TaxiIn","TaxiOut","Distance")]
select(hflights, contains("time"),ends_with("Delay"))
select(hflights,contains("TaxiIn","TaxiOut","Distance"))
select(hflights,contains("Distance"),starts_with("Taxi"))
select(hflights,contains("TailNum"),starts_with("Taxi"))
select(hflights,contains("Year"),contains("Month"),contains("Dayofweek"),ends_with("Time"))
select(hflights,contains("TailNum"),starts_with("Taxi"))
select(hflights,Year:ArrTime)
mutate # new function to add new column in existing column
mutate(hflights,average_speed = (Distance/AirTime)*60)

mutate(hflights,date = as.Date(paste(Year, Month , DayofMonth,sep = "/")),format("%d %m %Y"))
ff <- filter(hflights,UniqueCarrier == "AA")
dim(ff)
ff
ff1<- filter(hflights, Distance >3000 )
dim(ff1)
filter(hflights, Distance > 3500 , Dest ) 
hflights[dim(hflights$UniqueCarrier)]
#to count no of plane of perticular flight 
nrow(unique(select(filter(hflights,UniqueCarrier == "CO"),TailNum )))
nrow(unique(select(filter(hflights,UniqueCarrier == "CO"),TailNum))) 
kg <- filter(hflights,DepTime > ArrTime , ArrTime > 600)
dim(kg)
nrow(kg)
hflights
arrange(filter(hflights,ArrDelay > 17,DepDelay > 17),DepTime )
summarise(filter(hflights,!is.na(DepTime),!is.na(AirTime)),max(AirTime) )
 max(AirTime) 
 mutate(hflights, total_time = (Distance / (ActualElapsedTime +100 )))

filter(hflights, total_time* 60 < 60  )
lkm <-  filter(hflights, (Distance / (ActualElapsedTime +100 ))*60 < 60  )
nrow(lkm)
hflights$Dest
hflights$UniqueCarrier
filter(hflights,Origin)
# to find average delay of flights
hflights%>%
group_by(UniqueCarrier)%>%
filter(!is.na(DepDelay))%>%
summarise(avg_delay =  mean(DepDelay))%>%
arrange(avg_delay)
hflights
hflights%>%
group_by(UniqueCarrier,Dest)%>%
summarise(n_count = n())%>%
mutate(rank1 = rank(desc(n_count))%>%
filter(rank1 == 1)
  
hflights%>%
group_by(UniqueCarrier,Dest)%>%
summarise(n_count = n())%>%
mutate(rank1 = rank(desc(n_count)))%>%
filter(rank1 == 1)
getwd() # to check working directory

sheets <- excel_sheets("weather.xlsx")
sheets
w1 <- read_excel("weather.xlsx", sheet = "Unclean_Data")
w1
class(w1)
dim(w1)
w2<-gather(w1,"day","value",X1:X31) # to gather data AND ARRANGE INTO columns
w2
dim(w2)
w3<-gather(w1,"day","value",X1:X31,na.rm = T)
w3
dim(w3)
#removing column x
w34 <- w3[-1]
head(w3)
w4<-spread(w34,measure,value)
w4
dim(w4)
colnames(w4)[1]=  "year"        # to change the column names "year "
w5 <- filter(w4,day == "X1", year == 2014 , month == 12)
dim(w5)
glimpse(w4)
w4[6:23]<-lapply(w4[6:23],as.numeric)
warnings()

st1 <-str_replace(w4$day,"X","") #  to replace x from day column 
st1
st2 <- separate(w4,day,c("y","day1"),sep = 1 ) #using seprate we remove x from day
st2
st3 <- st2[-3]
st3
dim(st3)
st3$day1 <- as.numeric(st3$day1) 
st3$CloudCover<- as.numeric(st3$CloudCover)
st3
st7 <- mutate(st3,date = as.Date(paste(year,month,day1,sep = "-"),format = "%Y-%m-%d"))
st7$date
class(st7$date)   
wh <- which(is.na(st7$date)) # TO  check NA at dates position 
wh
(is.na(st7$CloudCover))
st7 <- st7[-wh,] # To emove NA from that positiom
st7$date
dim(st7)
#sr_replce precipetation T replaced by 0

st7$PrecipitationIn
st7$PrecipitationIn <- str_replace(st7$PrecipitationIn,"T","0")
st7$PrecipitationIn
st7
st8 <- st7[,-c(1:3)] # removed year , month day column
dim(st8)
# To shift date at 1st position
st9 <- st8[,c(23,1:22)]
st9
head(st9)
which(colnames(st9)== "CloudCover")
which(colnames(st9) == "date")

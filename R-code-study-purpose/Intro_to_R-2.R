################################
####  Introduction  to R  ######
################################


#try copying and pasting these lines one at a time to your console and then press enter
#If you are using R-studio, put the cursor in any line and press CTL+ENTER

#R doesn't read or execute any line that starts with "#". 

#######################
#R as simple calculator
#######################
6/2
log(12)
exp(log(12))
4^2
4**2
9^(3+4)



#######################
# Vector and Matrices
#######################
x=c(3,6,8)   #defines a vector of length 3 and saves it under "x"
x+2       #adds 2 with each element of vector "x"
x^3       #raises the power of each element to 3


#######
#######
y=matrix(c(2,4,6,8),ncol=2,nrow=2)     #defines a 2x2 matrix and populates it with the values provided
y
y+2   #adds 2 to each element of the matrix
y^2   #raises the power of each element to 2

z=matrix(c(3,5,7,9),ncol=2,nrow=2)      #defining another matrix

y+z   #adds two matrices
y*z   #element wise multiplication
y%*%z #proper matrix multiplication

solve(y)    #calculate the inverse of matrix y


cbind(y,z)   #binding y and z sidewise
rbind(y,z)   #binding y and z putting z underneath y
#######
#######


length(x)   # length of the vector x, in this case it's 3
x[1]        # prints the first element of x
x[1:2]      # prints the first two elements of x
x[2:3]      # prints the 2nd and 3rd element of x
x[-1]       # prints all the elements of x EXCEPT the first one


x=rep(5,3)      # defines a vector of size 3 with 5 at each position (rep stands for repeat)
x

x=seq(1,10)     # sequence going from 1 to 10 (seq stands for sequence)
x

x=seq(1,10,by=2)    #sequence going from 1 to 10 but increasing by 2.
x



x=c(1:6)
p=




#############################################
#### Logic check and basic if statement #####
#############################################

x=4

x<5
x>5
x==4   #( == stands for euqal)
x!=4   #( != stands for not equal)

if(x==3){
  print("x is 3")
}else{
    print("x is not 3")
  }



####################
# for loops in R ###
####################

for (i in 1:10){
  print(i)
}

x=seq(1,10,by=2)
for (i in x){
  print(i)
}








############################
#Drawing random numbers
############################
sample(c(1,2,3,4,5,6),size=1)    #draws a random sample from the list of 1 to 6

#try size=2 or size=3 and see what happens

sample(c(1,2,3,4,5,6),size=3,replace = TRUE) 


#set.seed() ensures that everytime we are getting the same sequence of random number
set.seed(999)    
sample(c(1,2,3,4,5,6),size=3,replace = TRUE) 


#drawing 30 random sample from N(0,1) distribution and saving it under "r"
r=rnorm(30)



###############################
#calculating summary statistics
###############################

mean(r)  #calculate the mean of a vector
sd(r)    #standard deviation of a vector
min(r)   #minimum of a vector
max(r)   #maximum of a vector
median(r)#median
range(r) #range




#########################################################
#Linear combination of Normal (Week-1, lecture slide 9)##
#########################################################

X1=rnorm(10000,mean=10,sd=2^0.5)
X2=rnorm(10000,mean=20,sd=3^0.5)

Y=0.4*X1+0.6*X2

mean(Y)
mean((Y-mean(Y))^2)   #variance

plot(density(Y))


#######################################
####Calculating summary for a matrix###
#######################################

m=matrix(seq(1,30),nrow = 5)
m


rowSums(m)
rowMeans(m)

colSums(m)
colMeans(m)



#a bit more general function
apply(m,1,sum)              #takes the matrix "m", for each of it's row (1 stands for row) calculates sum
apply(m,1,mean)             #takes the matrix "m", for each of it's row (1 stands for row) calculates mean


#a bit more general function
apply(m,2,sum)              #takes the matrix "m", for each of it's column (2 stands for column) calculates sum
apply(m,2,mean)             #takes the matrix "m", for each of it's row (2 stands for column) calculates mean




###################
###Basic plots ####
###################

x=seq(1:10)
y=exp(x)

plot(x,y)

plot(x,y,type="l")
points(4,10000)
abline(h=10000)
abline(v=4)





###########################################
#Graph explaining LLN using dice example###
###########################################


###basic coding
x=seq(1,800,by=1)

plot(x, rep(3.5,length(x)),type="l")

set.seed(999)
y=sample(c(1:6),1,replace = T)
points(1,mean(y))


set.seed(999)
y=sample(c(1:6),2,replace = T)
points(2,mean(y))


set.seed(999)
y=sample(c(1:6),3,replace = T)
points(3,mean(y))






#####Same plot with less coding #####

x=seq(1,800,by=1)

plot(x,rep(3.5,length(x)),type="l",
     xlab="Sample Size (n)",lwd=2,col="blue", ylab="Sample means (dots)")

legend(490,4.95, "Population mean = 3.5",col="blue",text.col = "blue")


for (n in x){
  set.seed(999)
  y=sample(c(1:6),n,replace = T)
  points(n,mean(y))
}





#########################################
#### Quick check of some distribution####
#########################################

#three independent random variales
Z1=rnorm(100000)
Z2=rnorm(100000)
Z3=rnorm(100000)

#Try guessing this following distributions

plot(density(Z1))

plot(density(Z1^2))

plot(density( Z2^2 + Z3^2 ))


T=Z1/sqrt((Z2^2 + Z3^2)/2)

plot(density(T))



#### R code for week 2 ####

###########################################
##### CDF from Finite Population      #####
##### Week-2, Slide-  8               #####
###########################################


d=c(1,1,2,1,2,3,3,1,2,4)

########################
#Simple example first
x=1.5     #Let's calculate the F_X for one specific value of x

#Creating an inddicator vector....
d<=x      #Checks each element of d to see if the value is less or equal to 1.5 or not. Produces TRUE/FALSE or 1/0...

mean(d<=x) # calculating mean of an indicator vector = probability...
########################


#We will keep changing the value of x and will evaluate "mean(d<=x)" over and over again...

#Let's do the whole calculation now using a for loop...

x=seq(   min(d)-1  , max(d)+1  ,  by=0.001)    #a sequence of x values for which F_X will be calculated

F_X=numeric( length(x) )                       # defining a numeric vector with length = length of x

for(i in 1:length(x)){                         # i = 1,2,3,...., length(x)
  
  # taking the ith item from vector x, evaluating mean(d<=x) and saving it as the ith element in the F_X vector.
  F_X[i] = mean( d <= x[i]  )                   
  
}


#PLoting the CDF
plot(x,F_X)



#frequency table
table(d)

#relative frequency table, in other words it's f_X
prop.table(table(d))

#Cumulative relative frequency, in other words it's F_X
cumsum(prop.table(table(d)))






###############################
##### Ploting Exp(1) CDF  #####
###############################


#defining the x vector...
x=seq(0,5,by=0.0001)

#since we know CDF of Exp(1) is 1-e^(-x), using this to evaluate F_X. 
#Hence no looping here as the previous example...
F_X=1-exp(-x)

plot(x,F_X,main="CDF of Exp(1)")

abline(h=0.6,lty=2)          #adding a horizontal line... lty=2 produces dashed line...
abline(v=0.916,lty=2)
text(4,0.2,"0.6 quantile = 0.916")


which.min(  abs(  F_X -  0.6 )   )   # finds out the position of the F_X vector at which the value is closest to 0.6

#abs( ) calculates the absolute value of the argument inside the ( ) 


x[   which.min(abs(F_X-0.6))     ]    # Findng the value of x which produced the F_X that's closest to 0.6




#If the above which.min( ) code was confusing 
#take a look at this following 4 lines...
###########################################
x=seq(0,5,by=0.5)
F_X=1-exp(-x)

#take a look at the output this follwing line and think why the next lines gives x=1
cbind(x,  F_X, abs(F_X-0.6))     # prints the vector of x, vector of F_X, vector of absolute difference of 

x[   which.min(abs(F_X-0.6))     ] 
###########################################






#########################################
##### Plotting CDF of Bin(n=5,p=0.5) ####
#########################################


r=rbinom(  n=100000,  size=5,  p=0.5)
x=seq(-1,6,by=0.01)

F_X=numeric(length(x))
for(i in 1:length(x)){
  F_X[i]=mean(r<=x[i])
}

plot(x,F_X,main="CDF of Binomial(n=5,p=0.5)")

abline(h=0.6,lty=2)
abline(v=3,lty=2)
text(5,0.2,"0.6 quantile = 3")


#Finding the smallest x satisfying p<=F_X(x), ref: first 3 lines on slide 14
min(   x[    0.6<=F_X   ]    )



#we can plot a pmf fairly easily...
plot(     prop.table(   table(r)  )   )




###############################################################
#A little animation (not really...)
n=seq(5,55,by=5)
for (i in n){
  r=rbinom(1000000,size=i,p=0.5)
  plot( prop.table(   table(r)  )  ,    main = paste0("size= ",i))   #paste0(combines)
  Sys.sleep(1)   # stops executing anything for 1 sec...
}
################################################################


#the paste0( ) in the title of the plot command joins two elements and produces a string...
paste0("STA-","261")

#another way...
cn=261
paste0("STA-",cn)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###############################
##### Home work question ######
###############################

#Ques-1
#say we don't know the functional form of the cdf of Exp(1)...
#Try to plot the the cdf using random numbers generated from Exp(1)...
#Hint: The binomial example above... 


#Ques-2
#Can make an animation(!) showing Poisson pmf taking the form of Normal distribution

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#





#############################
#### Density Histogram  #####
#### Exercise 5.4.5     #####
#### E&R page 278       #####
#############################

x = c(3.9, 7.2, 6.9, 4.5, 5.8, 3.7, 4.4, 4.5, 5.6, 2.5,
      4.8, 8.5, 4.3, 1.2, 2.3, 3.1, 3.4, 4.8, 1.8, 3.7)

#Histogram with frequency/counts. R decides by itself on the interval boundaries.... 
hist(x)

#Density histogram
hist(x,freq=FALSE)


#Density histogram with user given intervals...
hist(x,freq=FALSE, breaks = c(1, 4.5, 5.5, 6.5, 10 ), include.lowest = FALSE)


#If you don't want the plot, rather want the actual numbers...
hist(x, breaks = c(1, 4.5, 5.5, 6.5, 10 ), include.lowest = FALSE, plot=FALSE)

#If you want to check the R documentation of any command, type ?followed by the command. For example 
?hist





#################################
##### Calculating quantiles #####
##### Slide - 16            #####
#################################

x=c(-2.1,-0.3,0.4,1.2,1.5,2.1,2.2,3.3,4.0)

# The linear interpolation formula we learned is the "type=4" 
# out of the 9 types of quantile calculating formula available in R

Q1= quantile(x,   0.25,  type=4)

Q3= quantile(x,   0.75,  type=4)

iqr = IQR(x,type=4)

#Calculating Median, slide 17

#Option 1
quantile(x,0.5,type=4)

#Option 2
quantile(x,0.5)




####################################
#### Boxplot (default one)      ####
####################################

#the numbers on this plot are slightly different from what you got from the quantile(   ,type=4) function...
#boxplot ( ) uses it's own definition of quantiles...
#in practice with large enough data, they all are very similar...
#If you are intersted take a look at the documentation of boxplot( ) command...


boxplot(x)   # the plot itself...
boxplot(x, horizontal = TRUE) 

#In case you don't want the plot rather want the numbers...
boxplot.stats(x)$stats
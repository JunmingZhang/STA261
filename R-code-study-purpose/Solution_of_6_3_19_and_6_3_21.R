
######################################################
### There are many ways of writing these codes   #####
### Feel free to use any as long as it works     #####
### Since simulation the exact answers will vary #####
######################################################

########
#6.3.19#
########

#writing a func that takes value of n (sample size) and
#calculates width of the interval

width_of_int=function(n){
  w=2*qnorm(0.975)*5/sqrt(n)
  return(w)
}

n=5:500 # creating a vector with different values of n


#calling the function for each value of n
width_vec=sapply(n,FUN=width_of_int)


#a plot(not needed for the ques)
plot(n,width_vec,type="l")
abline(h=1)

#finding the value of n starting from which the width gets smaller
#or equal to 1.
min(n[width_vec<=1])

#My ans: n=385 is the minimum size of a sample that will ensure that 
#the 95% CI width is no longer than 1.



########
#6.3.21#
########

#A func which calculates the interval and
#checks if the true theta=0.5 falls within or not 

theta_check=function(s,n){
  set.seed(s) #this is optional
  r=rbinom(n,size = 1,prob=0.5)
  xbar=mean(r) 
  l=xbar-qnorm(0.975)*sqrt(xbar*(1-xbar)/n)
  u=xbar+qnorm(0.975)*sqrt(xbar*(1-xbar)/n)
  return((l<=0.5)&(0.5<=u))
}


#Calling this function with different values of n
mean(sapply(1:10000,FUN=theta_check,n=5))
mean(sapply(1:10000,FUN=theta_check,n=20))




#######################
# Now try to do 6.3.22 

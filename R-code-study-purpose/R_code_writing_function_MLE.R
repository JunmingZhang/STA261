#### R code for week 3-4 ####


################################
### Defining functions in R ####
################################

#Instead of using built-in functions often we write our own functions in R


# Here is an example of a function that takes a value, multiplies it by 2 and returns the output.
# the "x" within ( ) is called the argument of this function
# whenever you call this function, you will have to provide a value for this argument (x).
# Otherwise it won't work...

first_func=function(x){
  x*2
}

# Calling that function with argument 3... then it should return 6...
first_func(3)


#Let's define another function which will calculate the mean of a vector
#we know mean = sumof numbers/number of items
#Here is a more formal way of writing a function.
#Ideally the last line within a function should be a "return( )" command.
 
our_mean=function(x){
  m=sum(x)/length(x)
  return(m)
}

#calling our function with an argument.
our_mean(  c(2,4,6)  )



#We will show an example of using FUNCTION in calculating MLE



####################################
# Week-3, slide-11
# 1,0,1,1,0 are 5 iid samples from 
# Bernouli(theta) distribution
####################################

#saving the data under vector "d"
d=c(1,0,1,1,0)

# this following command calculates the density/mass for
# each data point for a specific value of theta (for example 0.4)
dbinom(d,size=1,prob = 0.4)


#Note: Bernoulli is Binomial with size=1 ( num of toss = 1)


# same calculation but for theta=0.45
dbinom(d,size=1,prob = 0.45)


# by definition Likelihood is the product of these individual densities/mass
# So this following line is the value of the likelihood function evaluated
# at theta=0.45. "prod" stands for product.
prod(dbinom(d,size=1,prob = 0.45))


# So we need to evaluate this function for all values of theta between 0 and 1
# Let's define theta as a vector of possible values.

theta=seq(0,1,by=0.001)

# we want to calculate the product of densities for each value of this theta vector.
# So lets define a function which will take theta as an argument and return 
# the product of the densities/mass as the output.

lik_hood=function(t){
  prod(dbinom(d,size=1,prob = t))
}


#lets try this function
lik_hood(0.45)
#the value matches to the one we got a moment ago.


# let's APPLY this funciton to the "theta" vector that we created.
# In other words, lets evaluate this function at each value of theta
# Lets also save the output as "L_theta"

L_theta = sapply( theta  , FUN = lik_hood)

# the above line takes each element from the vector "theta" passes on to the function
# called lik_hood (that we defined) and saves the output.


# Lets plot the likelihood function (the same picture you saw on week-3,slide-13)
plot(theta, L_theta)

#we want to find out that maximizes L_theta

theta[which.max(L_theta)]

#The ans is 0.6




##############
#Home work
##############

#say you have this follwing data from a poisson(lambda) distribution


d=c( 8,  3,  6,  2,  7,  2,  4,  5,  6,  5,
     5, 13,  5,  2,  2,  5,  8,  3,  6,  6,
     2,  4,  7,  5,  6,  4,  1,  3,  3,  5)


# 1. write a fucntion that will calculate the Likelihood under the poisson distribution,
#    using lambda as the argument.

# 2. Evaluate the likelihood function and plot it for 0<lambda<=3

# 3. Do you think the likelihood is maximized at 3? Or do you have doubt?

# 4. Let's use lambda between 0 and 10 now and plot the likelihood function.

# 5. find the value of lambda that maximizes the likelihood...








#################################################
#### A graph showing Chi-sq/df converges to 1 ###
#### Week_4, slide 23                         ###
#################################################

for(j in seq(10,1010,by=50)){
  plot(density(rchisq(100000,df=j)/j),main = paste0("Chisq(df=",j,")/df"),xlim=c(0.25,1.75))
  Sys.sleep(0.5)
}


#A chisq with a million df divided by its df...
plot(density(rchisq(100000,df=1000000)/1000000),xlim=c(0.25,1.75))



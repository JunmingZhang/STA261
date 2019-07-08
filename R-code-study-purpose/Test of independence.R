#####################################################
#####################################################
##### Chi-sq test of independece/homogeneity ########
#####################################################

#####################################################
##### A function that takes a matrix as an   ########
##### argument and calculates the Chi-sq     ########
##### test statistic and the corresponding   ######## 
##### p-value...                             ########
#####################################################

my.chisq.test=function(x){
  
  a=nrow(x)
  b=ncol(x)
  
  r.sum=apply(x,1,sum)  #calculating row totals
  c.sum=apply(x,2,sum)  #calculating column totals
  n=sum(x)              #calculatign grand total
  deg.free=(a-1)*(b-1)
  
  exp.count=(r.sum%*%t(c.sum))/n   #calculating expected counts for all the cells
  
  test.stat=sum(((x-exp.count)^2)/exp.count)
  p=1-pchisq(test.stat,df=deg.free)
  
  result=paste0("Test Statatistic = ", round(test.stat,4), "; degrees of freedom = ", deg.free ,"; p-value = ", round(p,4))
  
  return(result)
}



################################
# Let's try our function     ###
# Example: Week-10, slide-16 ###

my.chisq.test(  rbind(c(17,11,11,14),
                      c(17,9,8,7),
                      c(12,13,19,28))   )





################################################
#same data using the default function given in R

chisq.test(  rbind(c(17,11,11,14),
                   c(17,9,8,7),
                   c(12,13,19,28))   )


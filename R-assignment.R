#Q1 a)
x = c(21,22,23,24,25)
mean(x)

#Q1 b)
sum((x - mean(x))^2)/length(x)

#Q2 a)
expand.grid( c(21:25), c(21:25), c(21:25) )

#Q2 b)
X_bar = c(1:125)
i = 1
for (a in 21:25) {
  for (b in 21:25) {
    for (c in 21:25) {
      X_bar[i] = sum(a + b + c)/3
      i = i + 1
    }
  }
}
X_bar

#Q3 a)
freq = table(X_bar)
print(freq)
prop = prop.table(freq)
print(prop)

# Q3 b)
plot(prop, main="X-bar vs proportion")

# Q3 c)
prop_up = 0
for (i in 1:length(prop)) {
  prop_up = prop_up + as.numeric(names(prop[i])) * prop[i] * 125
}
prop_mean = prop_up / 125
print(prop_mean)

# check 3 c
# prop_up = 0
# for (i in 1:125) {
#   prop_up = prop_up + X_bar[i]
# }
# prop_mean = prop_up/125
# print(prop_mean)

# Q3 d)
prop_up = 0
for (i in 1:length(prop)) {
  prop_up = prop_up + (as.numeric(names(prop[i])) - prop_mean)^2 * prop[i] * 125
}
prop_var = prop_up/125
print(prop_var)

# check 3 d
# prop_up = 0
# for (i in 1:125) {
#   prop_up = prop_up + (X_bar[i] - prop_mean)^2
# }
# prop_var = prop_up/125
# print(prop_var)

# Q3 e)
print("3 b) -> central limit theorem")
print("3 c) d) -> law of large number")

#Q4
find_var_sd = function (a, b, c) {
  avg = (a + b + c) / 3
  return ((a - avg) ^ 2 + (b - avg)^2 + (c - avg)^2)
}

i = 1
pop_variance = c(1:125)
sam_variance = c(1:125)
for (a in 21:25) {
  for (b in 21:25) {
    for (c in 21:25) {
      pop_variance[i] = find_var_sd(a, b, c) / 2
      sam_variance[i] = find_var_sd(a, b, c) / 3
      i = i + 1
    }
  }
}
#Q4 a)
bias_s_sq =  mean(pop_variance) - 2
bias_sig_sq = mean(sam_variance) - 2
print(bias_s_sq)
print(bias_sig_sq)

#Q4 b)
var_sig = mean(sam_variance ^ 2) - mean(sam_variance) ^ 2
print(var_sig)
MSE_sig = var_sig + bias_sig_sq ^ 2
print(MSE_sig)

#Q5 a)
sd <- sqrt(2)
miu <- 23
n <- 3
error <- qnorm(0.975)*sd/sqrt(n)
error
captureCI = 0
for (i in 1 : 125) {
  captureCI = captureCI + ((miu > X_bar[i] - error) && (miu < X_bar[i] + error))
}
captureCI
prop_capture = captureCI/125
prop_capture

#Q5 b)
x_b = mean(c(23, 24, 25))
x_b
t_statistic = (x_b - miu) / (sqrt(2)/sqrt(3))
p_val = 2 * (1 - pnorm(abs(t_statistic)))
p_val

#Q5 c)
find_pop_mean = function(sample_mean, Xbar) {
  i = 0
  distance = abs(sample_mean - mean(Xbar))
  upper_bound = mean(Xbar) + distance
  lower_bound = mean(Xbar) - distance
  for (j in 1 : 125) {
    i = i + ((Xbar[j] <= lower_bound) || (Xbar[j] >= upper_bound))
  }
  i = i / length(Xbar)
  return (i)
}

another_p_val = find_pop_mean(24, X_bar)
# P_val = 1 - prop_capture
# P_val
another_p_val


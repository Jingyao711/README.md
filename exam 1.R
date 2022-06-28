exam1 = read.table('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex1\\ex1\\ex1data1.txt', sep = ",")
# identity matrix
diag(5)

# Plotting the Data
plot(exam1$V1,exam1$V2,pch = 4 ,col='red', xlab='Population of City in 10,000s',ylab='Profit in $10,000s')

# Implementation
m = length(exam1$V1)
X = cbind(rep(1,m),exam1$V1)
theta = matrix(c(0,0),2,1)
iter = 1500
alpha = 0.01

# computing the cost
h=X%*%theta
J = 1/(2*m)*sum((h-exam1$V2)^2)

i=0
# Gradient descent
for (i in seq(0,m)) { 
  theta=theta-alpha/m*t(X)%*%(h-exam1$V2)
  h=X%*%theta
  J = 1/(2*m)*sum((h-exam1$V2)^2)
  i=i+1
}

# predict
h=X%*%theta
p1 = matrix(c(1,3.5),1,2)
p2 = matrix(c(1,7),1,2)
h1 = p1%*%theta
plot(exam1$V1,exam1$V2,pch = 4 ,col='red', xlab='Population of City in 10,000s',ylab='Profit in $10,000s')
lines(X[,2],h,col='blue',type='l')

### multivariable
exam2 = read.table('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex1\\ex1\\ex1data2.txt', sep = ",")

# Implementation
m = length(exam2$V1)
X = cbind(rep(1,m),exam2$V1,exam2$V2)
theta = matrix(c(0,0,0),3,1)
iter = 1500
alpha = 0.01

# standardized
mean1=mean(X[,2])
std1 = sd(X[,2])
X[,2] = (X[,2]-mean1)/std1

mean2=mean(X[,3])
std2 = sd(X[,3])
X[,3] = (X[,3]-mean2)/std2

# computing the cost
h=X%*%theta
J=rep(0,m)

i=0
# Gradient descent
for (i in seq(0,iter)) { 
  theta=theta-alpha/m*t(X)%*%(h-exam2$V3)
  h=X%*%theta
  J[i] = 1/(2*m)*sum((h-exam2$V3)^2)
  i=i+1
}

plot(seq(1,iter),J,type = 'l',col='green')

# theta Normal Equations
library(matlib)
theta_n = inv(t(X)%*%X)%*%t(X)%*%exam2$V3

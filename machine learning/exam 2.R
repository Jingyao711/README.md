# read data
data1 = read.table('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex2\\ex2\\ex2data1.txt',sep = ',')

colors = c('yellow','black')
pch = c(16,3)
X1 = seq(30,100,1)
X2 = -theta[1]/theta[3]-theta[2]*X1/theta[3]

plot(data1$V1, data1$V2, col=colors[factor(data1$V3, levels = c("0", "1"))],pch = pch[factor(data1$V3, levels = c("0", "1"))],xlab='Exam 1 score',ylab='exam 2 score')
lines(X1,X2,col='blue',type='l')

plot(X1,X2)

iter = 400
alpha = 0.001
#sigmoid function
m = length(data1$V1)
X = cbind(rep(1,m),data1$V1,data1$V2)
theta = matrix(c(0.1,0.1,0.1),3,1)
z = X%*%theta
sig = 1/(1+exp(-z))


# Cost function
J = rep(0,iter)
i=1
for (i in 1:iter) {
  theta = theta-alpha/m*t(X)%*%(sig-data1$V3)
  z = X%*%theta
  sig = 1/(1+exp(-z))
  J[i] = 1/m*sum(-data1$V3*log(sig)-(1-data1$V3)*log(1-sig))
  i=i+1
}

plot(seq(1,iter),J,type = 'l')


# cost function
J <- function(theta){
  z = X%*%theta
  sig = 1/(1+exp(-z))
  C=1/m*sum(-data1$V3*log(sig)-(1-data1$V3)*log(1-sig))
  return(C)
}
z <- outer(theta1, theta2, J)
persp(theta1, theta2, z)
library(rgl)
persp3d(theta1, theta3, z,theta=155,phi=30,col="green4", ltheta=-120,shade=.75,border=NA,box=FALSE)

z = matrix(0,101,101)

for (i in 1:101) {
  for (j in 1:101) {
    z[i,j] = J(theta1[i],theta3[j])
  }
}

theta_optim <- optim(par=theta,fn=J)
#set theta
theta <- theta_optim$par
#cost at optimal value of the theta
theta_optim$value


# Predict 
p1 = c(1,45,85)
z = p1%*%theta
sig = 1/(1+exp(-z))

# multi
data2 = read.table('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex2\\ex2\\ex2data2.txt',sep = ',')

#Visualizing the data
colors = c('yellow','black')
plot(data2$V1, data2$V2, col=colors[factor(data2$V3, levels = c("0", "1"))],pch = pch[factor(data2$V3, levels = c("0", "1"))],xlab='Exam 1 score',ylab='exam 2 score')

# Cost function
m = length(data2$V1)
mapfeature = cbind(rep(1,length(data2$V1)),data2$V1,data2$V2,data2$V1^2,data2$V1*data2$V2,data2$V2^2,data2$V1^3,data2$V1^2*data2$V2,data2$V1*data2$V2^2,data2$V2^3,data2$V1^4,data2$V1^3*data2$V2,data2$V1^2*data2$V2^2,data2$V1*data2$V2^3,data2$V2^4,data2$V1^5,data2$V1^4*data2$V2,data2$V1^3*data2$V2^2,data2$V1^2*data2$V2^3,data2$V1*data2$V2^4,data2$V2^5,data2$V1^6,data2$V1^5*data2$V2,data2$V1^4*data2$V2^2,data2$V1^3*data2$V2^3,data2$V1^2*data2$V2^4,data2$V1*data2$V2^5,data2$V2^6)
theta2 = matrix(rep(0,28),28,1)
z = mapfeature%*%theta2
sig = 1/(1+exp(-z))
lambda = 1
J = 1/m*sum(-data2$V3*log(sig)-(1-data2$V3)*log(1-sig))+lambda*sum(theta2[2:28]^2)/(2*m)

# cost function
J <- function(theta){
  z = mapfeature%*%theta
  sig = 1/(1+exp(-z))
  C=1/m*sum(-data2$V3*log(sig)-(1-data2$V3)*log(1-sig))+lambda*sum(theta2[2:28]^2)/(2*m)
  return(C)
}
theta_optim <- optim(par=theta2,fn=J)
#set theta
theta <- theta_optim$par
#cost at optimal value of the theta
theta_optim$value

#plot decision rule boundary
# class labels: simple distance from origin
classes <- ifelse(z > 0, "black", "orange")

grid <- expand.grid(x=1:100, y=1:100)
classes.grid <- knn(train.df, grid, classes, k=25, prob=TRUE)  # note last argument
prob.grid <- attr(classes.grid, "prob")
prob.grid <- ifelse(classes.grid == "blue", prob.grid, 1 - prob.grid)

# plot the boundary
contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
        col="grey", drawlabels=FALSE, lwd=2)
# add points from test dataset
points(test.df, col=classes.test)
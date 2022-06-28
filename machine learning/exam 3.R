data1 = readMat('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex3\\ex3\\ex3data1.mat')
m = length(data1$y);
rand_indices = sample(1:m,100,replace = F);
sel = X(rand_indices, :);
X = data1$X[rand_indices,]
y= data1$y[rand_indices,]
m = 100
n = 400
example_width = 20
example_height = 20

# Compute number of items to display
display_rows = 10
display_cols = 10

# Between images padding
pad = 1

# Setup blank display
display_array = matrix(rep(1),pad + display_rows * (example_height + pad),pad + display_cols * (example_width + pad))

# Copy each example into a patch on the display array
curr_ex = 1;
for (j in 1:display_rows){
  for (i in 1:display_cols){
      display_array[pad + (j - 1) * (example_height + pad) + (1:example_height),pad + (j - 1) * (example_width + pad) + (1:example_width)] =  matrix(X[curr_ex, ], example_height, example_width) 
      curr_ex = curr_ex + 1
    
  }
}


# The col argument allows customizing the color palette of the image. You can pass as variable a function such as gray.colors, topo.colors, hcl.colors or a similar function. The default value is hcl.colors(12, "YlOrRd", rev = TRUE).
# Note that if you increase the number of values the color image will be smoothed.

image(display_array, col = gray.colors(50))

# Part 2a: Vectorize Logistic Regression 
theta = matrix(c(-2,-1,1,2),4,1)
X_t = cbind(rep(1,5),matrix(seq(1,15,1)/10,5,3))
y_t = matrix(c(1,0,1,0,1),5,1)
m= length(y_t)
lambda_t = 3
g = 1/(1+exp(-X_t%*%theta))
reg = lambda_t/(2*m)*t(theta[2:4])%*%theta[2:4]
C = 1/m*sum(-y_t*log(g)-(1-y_t)*log(1-g)) + reg
grad = matrix(rep(0,4),4,1)
grad[1]=1/m*t(g-y_t)%*%X_t[,1]
grad[2:4]=1/m*t(g-y_t)%*%X_t[,2:4]+lambda_t/m*theta[2:4]

all_theta_t = matrix(rep(0,40),10,4)

cost_function = function(Y,theta){
  
  m = length(Y)
  n = length(theta)
  g = 1/(1+exp(-X%*%theta))
  reg = 0.1/(2*m)*t(theta[2:n])%*%theta[2:n]
  C = 1/m*sum(-Y*log(g)-(1-Y)*log(1-g)) + reg
  return(C)
}


a = cost_function(X_t,y_t,theta,lambda_t)

all_theta = matrix(rep(0.05,4010),401,10)
cost = rep(0,10)
X = cbind(matrix(rep(1,5000),5000,1), data1$X)
lambda_t = 0.1
for (i in 1:10) {
  Y = ifelse(data1$y==i,1,0)
  all_theta[,i] <- optim(par=all_theta[,i],fn=cost_function,Y=Y)$par
  cost[i]= optim(par=all_theta[,i],fn=cost_function)$value
}

G = 1/(1+exp(-X%*%all_theta))
G = cbind(G,matrix(rep(0,5000),5000,1))

G[,11] = max(G[,1:10])
for (i in 1:5000) {
  G[i,11] = max(G[i,1:10])
}

colnames(G)[apply(G,1,which.max)]
theta1 = data2$Theta1
theta2 = data2$Theta2

# Neural Networks
data2 = readMat('C:\\work\\BaiduNetdiskWorkspace\\coursera\\machine learning\\machine-learning-ex3\\ex3\\ex3weights.mat')
cost_function_NN = function(theta1,theta2,X){
  m=dim(data1$X)[1]
  num_labels = dim(data2$Theta2)[1]
  p = matrix(rep(0,m),m,1)
  g1 = 1/(1+exp(-X%*%t(theta1)))
  g1 = cbind(matrix(rep(1,5000),5000,1), g1)
  g2 = 1/(1+exp(-g1%*%t(theta2)))
  
}

colnames(g2) = c(1,2,3,4,5,6,7,8,9,10)
Y_pred = colnames(g2)[apply(g2,1,which.max)]
count(Y_pred==data1$y)/5000
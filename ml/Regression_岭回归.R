  cement <- data.frame(X1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10), 
                       X2 = c(26,29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68), 
                       X3 = c(6, 15, 8, 8, 6,9, 17, 22, 18, 4, 23, 9, 8), 
                       X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26,34, 12, 12), 
                       Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1,115.9, 83.8, 113.3, 109.4))
  library(MASS)
  
  ## Attaching package: 'MASS'
  ## The following object is masked _by_ '.GlobalEnv':
  ## cement
  
  ridge.sol <- lm.ridge(Y ~ ., lambda = seq(0, 150, length = 151), data = cement,model = TRUE)
  
  names(ridge.sol) # 变量名字

  
  ridge.sol$lambda[which.min(ridge.sol$GCV)] ##找到GCV最小时的lambdaGCV
  ridge.sol$coef[which.min(ridge.sol$GCV)] ##找到GCV最小时对应的系数
  
  par(mfrow = c(1, 2))
  
  # 画出图形，并作出lambdaGCV取最小值时的那条竖直线
  matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients",
          type = "l", lty = 1:20)
  abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
  
  # 下面的语句绘出lambda同GCV之间关系的图形
  plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda),
       ylab = expression(beta))
  
  abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
  
  par(mfrow = c(1, 1))
  
  # 从上图看，lambda的选择并不是那么重要，只要不离lambda=0太近就没有多大差别。
  # 下面利用ridge包中的linearRidge()函数进行自动选择岭回归参数
  
  library(ridge)
  
  mod <- linearRidge(Y ~ ., data = cement)
  
  summary(mod)

  
  # 从模型运行结果看，测岭回归参数值为0.0147，各自变量的系数显著想明显提高
  # （除了X3仍不显著）最后，利用Lasso回归解决共线性问题
  library(lars)
  
  x = as.matrix(cement[, 1:4])
  y = as.matrix(cement[, 5])
  
  (laa = lars(x, y, type = "lar")) #lars函数值用于矩阵型数据
  
  plot(laa) #绘出图
  
  summary(laa) #给出Cp值
  
  
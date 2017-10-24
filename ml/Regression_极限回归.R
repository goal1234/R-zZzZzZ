elmtrain.default <-
  function(x,y,nhid,actfun,...) {
    require(MASS)
    
    if(nhid < 1) stop("ERROR: number of hidden neurons must be >= 1")
    ########1.选择数据，X与Y  
    T <- t(y)
    P <- t(x)
    ########2.随机产生权值，目的在于将X值进行变化  
    
    inpweight <- randomMatrix(nrow(P),nhid,-1,1)
    tempH <- inpweight %*% P
    biashid <- runif(nhid,min=-1,max=1)
    biasMatrix <- matrix(rep(biashid, ncol(P)), nrow=nhid, ncol=ncol(P), byrow = F) 
    
    tempH = tempH + biasMatrix
    ########3.将变化后的X值进行高维映射，最常用是sig函数   
    if(actfun == "sig") H = 1 / (1 + exp(-1*tempH))
    else {
      if(actfun == "sin") H = sin(tempH)
      else {
        if(actfun == "radbas") H = exp(-1*(tempH^2))
        else {
          if(actfun == "hardlim") H = hardlim(tempH)
          else {
            if(actfun == "hardlims") H = hardlims(tempH)
            else {
              if(actfun == "satlins") H = satlins(tempH)
              else {
                if(actfun == "tansig") H = 2/(1+exp(-2*tempH))-1
                else {
                  if(actfun == "tribas") H = tribas(tempH)
                  else {
                    if(actfun == "poslin") H = poslin(tempH)
                    else {
                      if(actfun == "purelin") H = tempH
                      else stop(paste("ERROR: ",actfun," is not a valid activation function.",sep=""))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    ########4.拟合出模型系数，即Y=AX中的A   
    outweight <- ginv(t(H), tol = sqrt(.Machine$double.eps)) %*% t(T)
    Y <- t(t(H) %*% outweight)
    model = list(inpweight=inpweight,biashid=biashid,outweight=outweight,actfun=actfun,nhid=nhid,predictions=t(Y))
    model$fitted.values <- t(Y)
    model$residuals <- y - model$fitted.values
    model$call <- match.call()
    class(model) <- "elmNN"
    model
  }

function (object, newdata = NULL, ...) 
{
  if (is.null(newdata)) 
    predictions <- fitted(object)
  else {
    if (!is.null(object$formula)) {
      x <- model.matrix(object$formula, newdata)
    }
    else {
      x <- newdata
    }
    
    ########1.获取训练模型中的参数
    inpweight <- object$inpweight
    biashid <- object$biashid
    outweight <- object$outweight
    actfun <- object$actfun
    nhid <- object$nhid
    TV.P <- t(x)
    
    ########2.通过参数将X值进行变化  
    
    tmpHTest = inpweight %*% TV.P
    biasMatrixTE <- matrix(rep(biashid, ncol(TV.P)), nrow = nhid, 
                           ncol = ncol(TV.P), byrow = F)
    tmpHTest = tmpHTest + biasMatrixTE
    
    ########3.高维度映射，通常选择sig函数
    if (actfun == "sig") 
      HTest = 1/(1 + exp(-1 * tmpHTest))
    else {
      if (actfun == "sin") 
        HTest = sin(tmpHTest)
      else {
        if (actfun == "radbas") 
          HTest = exp(-1 * (tmpHTest^2))
        else {
          if (actfun == "hardlim") 
            HTest = hardlim(tmpHTest)
          else {
            if (actfun == "hardlims") 
              HTest = hardlims(tmpHTest)
            else {
              if (actfun == "satlins") 
                HTest = satlins(tmpHTest)
              else {
                if (actfun == "tansig") 
                  HTest = 2/(1 + exp(-2 * tmpHTest)) - 
                    1
                else {
                  if (actfun == "tribas") 
                    HTest = tribas(tmpHTest)
                  else {
                    if (actfun == "poslin") 
                      HTest = poslin(tmpHTest)
                    else {
                      if (actfun == "purelin") 
                        HTest = tmpHTest
                      else stop(paste("ERROR: ", actfun, 
                                      " is not a valid activation function.", 
                                      sep = ""))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    ########4.进行预测的值计算，即Y（预测）=AX
    TY = t(t(HTest) %*% outweight)
    predictions <- t(TY)
  }
  predictions
}

#------------------自带的库进行极限回归---------------------#
  library(elmNN)
  set.seed(1234)
  Var1 <- runif(50, 0, 100) 
  sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
  model <- elmtrain.formula(Sqrt~Var1, data=sqrt.data, nhid=10, actfun="sig")
  new <- data.frame(Sqrt=0,Var1 = runif(50,0,100))
p <- predict(model,newdata=new)
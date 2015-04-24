xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  y_new=tapply(y,x,sample,replace=rep)
  y_vec=unname(unlist(y_new))
  return (y_vec)
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  indices=sample(1:length(err), length(err), replace=rep)
  y=fit+err[indices]
  return (y)
 
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree==1){
    fit= lm(y ~ x)
    coeff=as.vector(c(fit$coef[1],fit$coef[2]))
  } else {
    fit= lm(y ~ x + I(x^2))
    coeff=as.vector(c(fit$coef[1],fit$coef[2], fit$coef[3]))
  }
 
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
 if (is.null(fit)){
   newy=genBootY(data[,1],data[,2])
   coef=fitModel(data[,1],newy,degree)
 } else {
   newy2=genBootR(fit[,1],fit[,2],F)
   coef=fitModel(data[,1],newy2,degree)
 }
 return (coef)
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  coef1=c()
  for (i in 1:B) {
    coeff1= oneBoot(data, NULL, degree = 1)
    coef1=c(coef1,coeff1)
  }
  coef11=matrix(coef1, 2, B)
  coef2=c()
  for (i in 1:B) {
    coeff2= oneBoot(data, NULL, degree = 2)
    coef2=c(coef2,coeff2)    
  }
  coef12=matrix(coef2, 3, B)
  
  ce1=fitModel(data[,1],data[,2],1)
  y_fit=ce1[2]*data[,1]+ce1[1]
  err1=data[,2]-y_fit
  fit1=as.matrix(cbind(y_fit,err1))
  coef3=c()
  for (i in 1:B) {
    coeff3=oneBoot(data,fit1,1)
    coef3=c(coef3,coeff3)
  }
  coef13=matrix(coef3,2,B)
  
  ce2=fitModel(data[,1],data[,2],2)
  y_fit2=ce2[3]*data[,1]*data[,1]+ce2[2]*data[,1]+ce2[1]
  err2=data[,2]-y_fit2
  fit2=as.matrix(cbind(y_fit2,err2))
  coef4=c()
  for (i in 1:B){
    coeff4=oneBoot(data,fit2,2)
    coef4=c(coef4,coeff4)
  }
  coef14=matrix(coef4,3,B)
  
  coeff=list(coef11,coef12, coef13, coef14)
  
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x,y)
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  if (nrow(coeff)==2){
    sapply(1:ncol(coeff), function(x) abline(coef=coeff[,x],col=rgb(0,0,0,alpha=0.02)))
  } else {
    sapply(1:ncol(coeff), function(y) curve(coeff[3,y]*x^2+coeff[2,y]*x+coeff[1,y],col=rgb(0,0,0,alpha=0.02), add=TRUE))
  }
  curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3] * x^2, add=TRUE, col='red')
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

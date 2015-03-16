#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.init <- function(r, c, p){
  m= matrix(0,r,c)
  for (i in 1:r){
    for (j in 1:c){
      m[i,j]= sample(c(0,1,2), 1, replace= T, prob=c(1-p,p/2,p/2))
    }
  }
  
  
  return(m)
}

bml.step <- function(m){
  m_original=m
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (m_original[i,j]==1){
        if (j==ncol(m_original)){
          if (m_original[i,1]==0){
            m[i,1]=1
            m[i,j]=0  
          } 
        } else {
          if (m_original[i, j+1]==0){
            m[i, j+1]=1
            m[i,j]=0
          }
        }  
      }
    }
  }
  m_odd=m
  for(i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (m_odd[i,j]==2){
        if (i==1){
          if (m_odd[nrow(m),j]==0){
            m[nrow(m),j]=2
            m[1,j]=0
          }
        } else {
          if (m_odd[i-1, j]==0){
            m[i-1,j]=2
            m[i,j]=0
          }
        }
      }
    }
  }
  if (all(m==m_original)){
    grid.new=F
  } else{
    grid.new=T
  }
  
  return(list(m, grid.new))
}
comb = function(n, x) {
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}

bml.gridlock=function (r,c,p){
  init=bml.init(r,c,p)
  m=init
  step=0
  while (bml.step(m)[2]==T){
    m=matrix(unlist(bml.step(m)[1]),r,c)
    if (step>=comb(r*c, r*c*(1-p))*(2**(r*c*p))/4){
      return ("free flow")
    }
    step=step+1
  }
  return (step)
}

bml.gridlock1=function (r,c,p){
  init=bml.init(r,c,p)
  m=init
  step=0
  while (bml.step(m)[2]==T){
    m=matrix(unlist(bml.step(m)[1]),r,c)
    if (step>=2500){
      return ("free flow")
    }
    step=step+1
  }
  return (step)
}


bml.grid1_30=function(r,c,p){
  list_grid=list(bml.gridlock1(r,c,p))
  for (i in 1:29){
    list_grid=c(list_grid,bml.gridlock1(r,c,p))
  }
  return (list_grid)
}

mean_new = function(x){
  mean1=mean(as.numeric(unlist(x)),na.rm=TRUE)
  return (mean1)
  }
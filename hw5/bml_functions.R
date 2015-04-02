#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m= matrix(0,r,c)
  for (i in 1:r){
    for (j in 1:c){
      m[i,j]= sample(c(0,1,2), 1, replace= T, prob=c(1-p,p/2,p/2))
    }
  }

  
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

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

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m=bml.init(r,c,p)
  steps=500
  while (steps > 0){
    m=matrix(unlist(bml.step(m)[1]),r,c)
    image(m)
    Sys.sleep(0.15)
    steps=steps-1
  }
  return (m)

}

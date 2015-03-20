# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  has_adopted=matrix(initial.doctors)
  for (i in 2:n.days){
    a=sample(1:length(initial.doctors),2)
    if (has_adopted[a[1],i-1]==has_adopted[a[2],i-1]){
      has_adopted=cbind(has_adopted,has_adopted[,i-1])
    } else {
      if (has_adopted[a[1],i-1]==0){
        has_adopted=cbind(has_adopted,has_adopted[,i-1])
        has_adopted[a[1],i]= sample(c(0,1),1,prob=c(1-p,p))
      } else {
        has_adopted=cbind(has_adopted,has_adopted[,i-1])
        has_adopted[a[2],i]= sample(c(0,1),1,prob=c(1-p,p))        
      }
    }
  }
  return (has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
initial.doctors=sample(c(0,1),20,replace=T,prob=c(0.9,0.1))
a1=sim.doctors(initial.doctors, 20, 50, 0.5)
a2=sim.doctors(initial.doctors, 20, 50, 0.6)
a3=sim.doctors(initial.doctors, 20, 50, 0.7)
a4=sim.doctors(initial.doctors, 20, 50, 0.8)
a5=sim.doctors(initial.doctors, 20, 50, 0.9)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
plot(x=1:50,y=apply(a1,2,sum),xlab="day",ylab="number of doctors adopted", type="o", col="green",pch=21)
lines(x=1:50,y=apply(a2,2,sum),type="o",col="red",pch=22)
lines(x=1:50,y=apply(a3,2,sum),type="o",col="blue",pch=23)
lines(x=1:50,y=apply(a4,2,sum),type="o",col="yellow",pch=24)
lines(x=1:50,y=apply(a5,2,sum),type="o",col="purple",pch=25)
legend(35,10,c("p=0.5","p=0.6","p=0.7","p=0.8","p=0.9"),lty=c(1,1),cex=0.8,col=c('green','red','blue','yellow','purple'),pch=21:25)

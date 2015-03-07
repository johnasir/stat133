#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  num_cars = round(r * c * p)
  rcars = round(num_cars/2)
  bcars= num_cars - rcars
  empty = (r * c) - num_cars
  data = sample(c(rep(0,empty), rep(1, rcars), rep(2, bcars)))
  m = matrix(data = data, nrow = r, ncol = c)
  
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
m = bml.init(4,4, .5)
bml.step <- function(m){

  
   #return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){

}

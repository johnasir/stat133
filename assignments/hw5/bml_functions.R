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
bml.north <- function(c){ #moves the two's(blue) up if possible ina column
    copy = c
    for (i in 1:length(copy)) {    
        if (i == 1) {
            if (copy[i] == 2 && copy[length(c)] == 0) {
                c[i] <- 0 
                c[length(c)] <- 2
            }
        } else {
            if (copy[i] == 2 && copy[i-1] == 0) {
                c[i] <- 0 
                c[i-1] <- 2
            }
        }
    }
    return (c)
}

bml.east <- function(r){ #moves the one's(red) right if possible ina column
    copy = r
    for (i in 1:length(r)) {    
        if (i == length(r)) {
            if (copy[i] == 1 && copy[1] == 0) {
                r[i] <- 0 
                r[1] <- 1
            }
        } else {
            if (copy[i] == 1 && copy[i+1] == 0) {
                r[i] <- 0 
                r[i+1] <- 1
            }
        }
    }
    return (r)
}

#m = bml.init(4,4, .5)
bml.step <- function(m){
    old = m 
    for (r in 1:nrow(m)) {
        m[r,] = bml.east(m[r,])
    }
    for (c in 1:ncol(m)) {
        m[,c] = bml.north(m[,c])
    }

    return(list(m, isTRUE(all.equal(old,m))))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...) did i hit gridlock (FALSE IF I HIT GRIDLOCK)

bml.sim <- function(r, c, p){
    m = bml.init(r,c,p)
    for (i in 1:200) {
        x = bml.step(m)
        if (x[[2]] == TRUE) {
            return (FALSE)
        } 
    }
    return (TRUE)
    
    
}


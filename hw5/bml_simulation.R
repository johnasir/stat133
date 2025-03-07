#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
counter = 0
for (i in 1:100) {
    if(!bml.sim(5,4,.9)) {
        print (i)
        counter = counter + 1
    }
}
print(paste0("It gridlocked ", counter, " times"))

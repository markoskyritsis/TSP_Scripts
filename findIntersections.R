library(matlib)
library(dplyr) #For efficient check if values in range

findSlopeIntercept <- function(x1,x2,y1,y2) {
  #Find slope
  m <- 0  
  c <- 0
  if (x2 - x1 != 0) {
    m <- (y2 - y1) / (x2 - x1)
    c <- y1 - m*x1
  }
  #Find y-intercept
  return(c(m,c))
}



findIntersections <- function(x1,x2,y1,y2,x3,x4,y3,y4) {
  #Change vectors into lines
  eq1 <-  findSlopeIntercept(x1,x2,y1,y2)
  eq2 <-  findSlopeIntercept(x3,x4,y3,y4)
  
  #Assign coefficients and constants
  m1 <- eq1[1]
  c1 <- eq1[2]
  
  m2 <- eq2[1]
  c2 <- eq2[2]
  
  if (c1 == 0 | c2 == 0) {
    #Looks like we have a vertical line
    x <- 0
    y <- 0
    if (c1 == 0) {
      #Line 1 is vertical!
      x <- x1
      y <- m2*x + c2
    }
    else {
      #Line 2 is vertical!
      x <- x3
      y <- m1*x + c1
    }
    return(c(y,x))
  }
  else {
    #Use matlib library to find the intersection (system of two equations)
    A <- matrix(c(1,1,-m1,-m2),2,2)
    b <- c(c1,c2)
    
    #showEqn(A,b)
    #plotEqn(A,b)
    
    #Return the intersect
    intersect <- c(echelon(A,b)[1,3],echelon(A,b)[2,3])
    #print(intersect)
    return(intersect)
  }
}

checkWithinInterval <- function(x,y,x1,x2,y1,y2) {
  #Just check if the intersection point is between the two intervals
  if (y2 < y1) {
    y3 <- y2
    y2 <- y1
    y1 <- y3
  }
  if (x2 < x1) {
    x3 <- x2
    x2 <- x1
    x1 <- x3
  }  
  return(between(x,x1,x2) & between(y,y1,y2))
}

findGraphIntersections <- function(df, output = T) {
  interPoints <- list()
  count <- 1
  if (output == T) {
    plot(df$x, df$y)
    lines(df$x, df$y)
  }
  #Go through each line and check for intersections
  for (i in 1:(nrow(df)-3)) {
    for (j in (i+2):(nrow(df)-1)) {
      intersect <- findIntersections(df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)],df$x[j],df$x[(j+1)],df$y[j],df$y[(j+1)])
      y <- intersect[1]
      x <- intersect[2]
      #if ((i != 1 & j != nrow(df)-1) & checkWithinInterval(x,y,df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)]) == TRUE & checkWithinInterval(x,y,df$x[j],df$x[(j+1)],df$y[j],df$y[(j+1)]) == TRUE) {
      if ((df$x[i] != df$x[(j+1)]) & checkWithinInterval(x,y,df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)]) == TRUE & checkWithinInterval(x,y,df$x[j],df$x[(j+1)],df$y[j],df$y[(j+1)]) == TRUE) {
        if (output == T) {
          print(c(df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)]))
          print(c(df$x[j],df$x[(j+1)],df$y[j],df$y[(j+1)]))
          print(c(x,y))
          points(x,y, col = "red")
        }
        interPoints[[count]] <- c(x,y)
        count <- count + 1
      }
      
    }
  }
  return(interPoints)
}


#Example: load the csv file that comes with the R script on Github
df <- read.csv("Problem1.csv", header = T)
df$X <- NULL
#make sure the column names are x and y if not then change them


#Let's see how many crossings we have
intersections <- findGraphIntersections(df, output = T)

#Should find just one crossing. The plot highlights it
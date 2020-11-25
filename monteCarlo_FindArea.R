library(matlib)
library(dplyr) #For efficient check if values in range
randomX <- 0
randomY <- 0


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
    #Horizontal lines (Asymptotes, just return 0,0)
    if (m1 == 0 & m2 == 0) {
      return(c(0,0))
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


findPointIntersections <- function(df,pointX,pointY, output = T) {
  rx <- pointX
  ry <- pointY
  rx2 <- 0
  ry2 <- ry
  # 
  # if (randomX <= 2500) {
  #   rx2 <- 0
  #   
  # } else {
  #   rx2 <- 5000
  # }
  
  interPoints <- list()
  count <- 1
  if (output == T) {
    #   plot(df$x, df$y)
    #    lines(df$x, df$y)
    points(pointX,pointY, col = "blue")
  }
  #Go through each line and check for intersections
  for (i in 1:(nrow(df)-1)) {
    intersect <- findIntersections(df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)],rx,rx2,ry,ry2)
    y <- intersect[1]
    x <- intersect[2]
    if (checkWithinInterval(x,y,df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)]) == TRUE) {
      countit <- FALSE
      if (rx <= 2500) {
        if (x < rx) {
          countit <- TRUE
        }
      } else {
        if (x > rx) {
          countit <- TRUE
        }
      }
      if (countit == TRUE) {
        if (output == T) {
          # print(c(df$x[i],df$x[(i+1)],df$y[i],df$y[(i+1)]))
          # print(c(df$x[j],df$x[(j+1)],df$y[j],df$y[(j+1)]))
          print(c(x,y))
          #  points(x,y, col = "red")
        }
        interPoints[[count]] <- c(x,y)
        count <- count + 1
      }
    }
    
  }
  if (output == T) {
    if ((count-1) %% 2 == 0) {
      points(pointX,pointY, col = "blue")
    } else {
      points(pointX,pointY, col = "red")
    }
  }
  return((count-1))
}


#Here you go. Time to check for intersections of added points


monteCarlo <- function(df, no) {
  plot(df$x, df$y)
  lines(df$x, df$y)
  
  Area <- 1
  totalOut <- 0
  totalIn <- 0
  ratio <- 0
  randomX <- 0
  randomY <- 0 
  for (i in 1:no) {
    randomX <- runif(1,min = 1, max = 4999)
    randomY <- runif(1,min = 1, max = 4999)
    
    randomX
    randomY
    
    intersectPoint <- findPointIntersections(df,randomX,randomY,T)
    
    if (intersectPoint %% 2 == 0) {
      totalOut <- totalOut + 1
    } else {
      totalIn <- totalIn + 1
    }
  }
  ratio <- totalIn / (totalIn + totalOut)
  print(ratio)
}


#Example as folllows. Not optimised, but works nonetheless.

df <- read.csv("Example_Graph.csv", header = T)
df$X <- NULL
plot(df$x, df$y)
lines(df$x, df$y)

#Let's check the ratio between the irregular polygon and the total area. 
#The graph is in arbitrary units, so the area doesn't really matter, but if
#it was in e.g., cm, we could just multiply the ratio by the cm^2 in the end
#to get a really good approximation of the area of the polygon in cm^2

monteCarlo(df, 5000)


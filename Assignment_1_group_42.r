getManhattanDistance <- function(p1, p2) {
  return (abs(p1[1] - p2[1]) + abs(p1[2] - p2[2]))
}

visited <- function(node, set) {
  if (length(set) == 0) {
    return (0)
  }
  for (a in 1:length(set)) {
    if (all(node == c(set[[a]]$x, set[[a]]$y))) {
      return (a)
    }
  }
  return (0)
}

findGoal <- function(carCoordinate, packages) {
  unVisitedPack <- which(packages[,5] == 0)
  closestPack <- NULL
  closestDist <- NULL
  
  if (length(unVisitedPack) == 1) {
    unPickedPackages <- packages[unVisitedPack,]
    return (unPickedPackages)
  } else {
    unPickedPackages <- packages[unVisitedPack,]
  }
  for (i in 1:length(unVisitedPack)) {
    package <- unPickedPackages[i,]
    packagePickup <- c(package[1], package[2])
    distance <- getManhattanDistance(carCoordinate, packagePickup)
    if (is.null(closestDist) || distance < closestDist) {
      closestPack <- package
      closestDist <- distance
    }
  }
  return (closestPack)
}

backTrack <- function(node, set) {
  parent <- node$parent
  child <- node 
  child1 <- NULL 
  while (!all(parent == c(0,0))) {
    for (a in 1:length(set)) {
      pos <- c(set[[a]]$x, set[[a]]$y)
      if (all(parent == pos)) {
        if (!all(set[[a]]$parent == c(0,0))) {
          child <- set[[a]]
        }
        child1 <- set[[a]]
        parent <- set[[a]]$parent
        break
      }
    }
  }
  return (child)
}

aStarSearchAlgorithm <- function(goalCoordinate, carCoordinate, hroads, vroads ) {
  # f = g + h

  openSet <- list()
  closedSet <- list()
  distance <- getManhattanDistance(carCoordinate, goalCoordinate)

  h <- getManhattanDistance(carCoordinate, goalCoordinate)
  startNode <- list(x = carCoordinate[1], y = carCoordinate[2], g = 0, h = h, f = h, parent = c(0, 0))
  openSet[[length(openSet) + 1]] <- startNode
  
  while (TRUE) {
    scores <- sapply(openSet, function(item) item$f)
    best_index <- which.min(scores)
    currentNode <- openSet[[best_index]]
    openSet <- openSet[-best_index]
    closedSet[[length(closedSet) + 1]] <- currentNode
    currentNodePosition <- c(currentNode$x, currentNode$y)
    h <- getManhattanDistance(currentNodePosition, goalCoordinate)
    
    if (h == 0) {
      finalF <- currentNode$f
      nextMove <- backTrack(currentNode, closedSet)
      nodeData <- list(node = nextMove, finalF = finalF)
      return(nodeData)
    }

    x <- currentNode$x
    y <- currentNode$y
    n1 <- c(x + 1, y)
    n2 <- c(x - 1, y)
    n3 <- c(x, y + 1)
    n4 <- c(x, y - 1)
    possibleNodes <- list(n1, n2, n3, n4)

    
    for (i in 1:4) {

      # if (withinBounds(possibleNodes)) {
      if (possibleNodes[[i]][1] > 0 & possibleNodes[[i]][1] < 11 & possibleNodes[[i]][2] > 0 & possibleNodes[[i]][2] < 11 & !visited(possibleNodes[[i]], closedSet)) {
        
        x <- possibleNodes[[i]][1]
        y <- possibleNodes[[i]][2]

        g = switch(
          i,
          hroads[x - 1, y],
          hroads[x, y],
          vroads[x, y - 1],
          vroads[x, y]
        )

        h <- getManhattanDistance(c(x, y), carCoordinate)

        nodeIndex <- visited(possibleNodes[[i]], openSet)
        parent <- c(currentNode$x, currentNode$y)
        g <- g + currentNode$g
        newNode <- list(x = x, y = y, g = g, h = h, f = h + g, parent = parent)
        if (!nodeIndex) {
          openSet[[length(openSet) + 1]] <- newNode
        } else if (openSet[[nodeIndex]]$f > newNode$f) {
          openSet[[nodeIndex]] <- newNode
        }
      }
    }
  }
}

myFunction <- function(roads, car, packages) {
  carCoordinate <- c(car$x, car$y)
  
  if (car$load == 0) {
    closestPackage <- findGoal(carCoordinate, packages)
    goalCoordinate <- c(closestPackage[1], closestPackage[2])
  } else {
    row <- car$load
    goalCoordinate <- c(packages[row, 3], packages[row, 4])
  }
  
  nodeData <- aStarSearchAlgorithm(goalCoordinate, carCoordinate, roads$hroads, roads$vroads)
  foundPath <- nodeData$node

  if (car$y > foundPath[2]) {
    car$nextMove <- 2
  } else if (car$x > foundPath[1]) {
    car$nextMove <- 4
  } else if (car$x < foundPath[1]) {
    car$nextMove <- 6
  } else if (car$y < foundPath[2]) {
    car$nextMove <- 8
  } else {
    car$nextMove <- 5
  }

  return (car)
}


#' dumbDM
#'
#' This control function just moves randomly, until all packages are picked up and delivered by accident!
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
#' basicDM
#'
#' This control function will pick up and deliver the packages in the order they
#' are given (FIFO). The packages are then delivered ignoring the trafic conditions
#' by first moving horizontally and then vertically.
#' 
#' As a first step, you should make sure you do better than this.
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
#' manualDM
#'
#' If you have the urge to play the game manually (giving moves 2, 4, 5, 6, or 8 using the keyboard) you
#' can pass this control function to runDeliveryMan
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' testDM
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' The mean for the par function (with n=500) on this is 172.734, and the sd is approximately 39.065.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' This set of seeds is chosen so as to include a tricky game that has pick ups and deliveries on the same
#' spot. This will occur in the actual games you are evaluated on too.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 4 minutes (250 seconds). If the evaluation machine is slower than expected,
#' this will be altered so that the required time is 25% slower than the par function.
#'
#' The par function takes approximately 96 seconds on my laptop (with n=500 and verbose=0).
#'
#' @param myFunction The function you have created to control the Delivery Man game.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runDeliveryMan output of the result of each game.
#' @param returnVec Set to TRUE if you want the results of the games played returned as a vector.
#' @param n The number of games played. You will be evaluated on a set of 500 games, which is also the default here.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If returnVec is false, a scalar giving the mean of the results of the games played. If returnVec is TRUE
#' a vector giving the result of each game played. If the time limit is breached, a NA is returned.
#' @export
testDM=function(myFunction,verbose=0,returnVec=FALSE,n=500,seed=21,timeLimit=250){
  if (!is.na(seed))
    set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  aStar=sapply(seeds,function(s){
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    runDeliveryMan(myFunction,doPlot=F,pause=0,verbose=verbose==2)
  })
  endTime=Sys.time()
  if (verbose>=1){
    cat("\nMean:",mean(aStar))
    cat("\nStd Dev:",sd(aStar))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return(aStar)
  else
    return (mean(aStar))
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. <1,1> is the bottom left, and <dim,dim> is the top right.
#'(2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not
#' delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10. Note that
#' this means you will have to remove duplicated nodes from your frontier to keep your AStar
#' computationally reasonable! There is a time limit for how long an average game can be run in, and
#' if your program takes too long, you will penalized or even fail.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5,verbose=T) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          if (verbose)
            cat("\nCongratulations! You suceeded in",i,"turns!")
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return (NA)
}
#' @keywords internal
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}
#' @keywords internal
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$x,car$y]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$x,car$y]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$x,car$y]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$x,car$y]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

#' @keywords internal
plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

#' @keywords internal
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @keywords internal
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n-1)
  vroads=matrix(rep(1,(n-1)*n),nrow=n)
  list(hroads=hroads,vroads=vroads)
}

#' @keywords internal
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row,row+1),c(col,col),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row,row),c(col,col+1),col=vroads[row,col])
    }
  }
}
#' @keywords internal
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}
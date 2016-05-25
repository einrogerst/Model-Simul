#state variables
simClock <- 0
#timeLastEvent <- 0
arrivalTime <- numeric()

#statistics
numCustServed <- 0
delaysTotal <- 0
areaQ <- 0
areaB <- 0

### objetos para simular exemplo do livro
### comentar em caso de simulação normal
#timeBetweenArrivals <- c(0.4, 1.2, 0.5, 1.7, 0.2, 1.6, 0.2, 1.4, 1.9)
#serviceTimes <- c(2.0, 0.7, 0.2, 1.1, 3.7, 0.6)
#timeNextArrival <- simClock + timeBetweenArrivals[1]
#timeBetweenArrivals <- timeBetweenArrivals[-1]

## objetos para simulação com função exponencial
timeNextArrival <- rexp(1, rate = 1) 
timeNextDeparture <- Inf

#stop criteria (number of served customers)
reqCustServed <- 20 #definir para 6 para simular valores livro

update <- function(curTime, event){
  timeLast <<- simClock
  simClock <<- curTime
  diffTime <- curTime - timeLast
  areaQ <<- areaQ + length(arrivalTime) * diffTime
  areaB <<- if(serverBusy) areaB + diffTime else areaB
}

logDF <- data.frame()
log <- function(event){
  logDF <<- rbind(logDF, 
                  data.frame(time=simClock, 
                             type=event, 
                             state=as.numeric(serverBusy), 
                             queue=length(arrivalTime), 
                             instantAvgQ=delaysTotal/numCustServed,
                             cumQ = areaQ, 
                             cumB = areaB))
}

servers <- 
  data.frame(
    id=c(1, 2, 3),
    busy=c(F, F, T),
    depTime=c(0, 0, 0)
  )

allServersBusy <- function(){
  as.logical(sum(servers$busy)==nrow(servers))
}

if(length(which(!servers$busy))==1) which(!servers$busy) else sample(which(!servers$busy), 1)

class(c(which(!servers$busy)))

log("start")
while(numCustServed < reqCustServed){
  if(timeNextArrival < timeNextDeparture){
    update(timeNextArrival, "arrival")
    timeNextArrival <- simClock + rexp(1, rate = 1)
    #timeNextArrival <- simClock + timeBetweenArrivals[1] #valores livro
    #timeBetweenArrivals <- timeBetweenArrivals[-1] #valores livro
    if(allServersBusy){
      #customer goes to queue
      arrivalTime <- c(arrivalTime, simClock)
    }
    else{
      numCustServed <- numCustServed + 1
      
      serverBusy <- TRUE
      #timeNextDeparture <- simClock + serviceTimes[1] #valores livro
      #serviceTimes <- serviceTimes[-1]
      timeNextDeparture <- simClock + rexp(1, rate = 1)
    }
    log("arrive")
  }else{
    update(timeNextDeparture, "departure")
    if(length(arrivalTime)==0){
      serverBusy <- FALSE
      timeNextDeparture <- Inf
    }
    else{
      delay <- simClock - arrivalTime[1]
      delaysTotal <- delaysTotal + delay
      numCustServed <- numCustServed + 1
      #timeNextDeparture <- simClock + serviceTimes[1] # valores livro
      #serviceTimes <- serviceTimes[-1]
      timeNextDeparture <- simClock + rexp(1, rate = 1) 
      arrivalTime <- arrivalTime[-1]
    }
    log("depart")
  }
}

op <- par(mfrow = c(2,1),
          oma = c(1,1,1,1) + 0.1,
          mar = c(4,1,2,1) + 0.1)

plot(stepfun(logDF$time, c(0, logDF$queue)), 
     main="Queue size over time", 
     xlab="", ylab="")
abline(h=areaQ/simClock, col="red")

mtext(paste0("[red] Estimated average queue size (q): ", 
             sprintf("%1.2f", areaQ/simClock)),
      side=1, line=2)
mtext(paste0("Estimated averge delay (d): ",
             sprintf("%1.2f", delaysTotal/numCustServed), " u.t."), 
      side=1, line=3) 

plot(stepfun(logDF$time, c(0, logDF$state)), 
     main="Server state over time",
     xlab="", ylab="")

mtext(paste0("Estimated utilization (b): ",
             sprintf("%0.2f%%", 100*areaB/simClock)),
      side=1, line=2)

print(logDF)


#N1 = 16/maio -> avaliação intermediaria !!!
#N2 = 6/jun -> avaliação final
#PE = 13/Jun -> Prova Extra
#NF1 = (0.5*N1)+(0.5*N2)
#Recuperação
#NF2 = (0.55*NF1)+(0.45*PE) 

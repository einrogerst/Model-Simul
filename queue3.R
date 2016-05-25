#simulador para fila única e n servos
setwd("C:\\Users\\i826950\\Desktop\\Personal\\Unisinos\\model&simul")

#state variables
simClock <- 0

queue <- 
  data.frame(
    clientNum <- numeric(),
    arrivalTime <- numeric(),
    chamada <- character()
  )

#statistics
numCustServed <- 0
delaysTotal <- 0
delay <- 0
areaQ <- 0
areaB <- 0

data <- read.csv(file="data18.csv", stringsAsFactors = F)
data$arrivalTimestamp <- as.POSIXct(data$arrivalTimestamp)
data$servStartTimestamp <- as.POSIXct(data$servStartTimestamp)

clientNum <- 1
timeNextArrival <- data[clientNum, "arrivalTimestamp"]
timeNextDeparture <- Inf # just to make shure the first event is an arrival

#stop criteria (number of served customers)
reqCustServed <- 200 

update <- function(curTime, event){
  timeLast <<- simClock
  simClock <<- curTime
  diffTime <- as.numeric(curTime) - as.numeric(timeLast)
  areaQ <<- areaQ + nrow(queue) * diffTime
  areaB <<- if(allServersBusy()) areaB + diffTime else areaB
}

logDF <- data.frame()
log <- function(event, clientNum, chamada){
  logDF <<- rbind(logDF, 
                  data.frame(time=as.POSIXct(simClock, origin = "1970-01-01"), 
                             type=event, 
                             clientNum=clientNum,
                             chamada=chamada,
                             busyServers=sum(servers$busy), 
                             queue=nrow(queue)
                             #instantAvgQ=delaysTotal/numCustServed,
                             #cumQ = areaQ, 
                             #cumB = areaB
                             ))
}

numServers <- 5
servers <- 
  data.frame(
    busy=rep(F, numServers),
    depTime=Inf,
    clientNum=0,
    chamada=""
  )

allServersBusy <- function(){
  as.logical(sum(servers$busy)==nrow(servers))
}

allocateServer <- function(clientNum, chamada) {
  choosenServer <-
    if(length(which(!servers$busy))==1) 
      which(!servers$busy) 
    else 
      sample(which(!servers$busy), 1)
  servers[choosenServer, ] <<- list(T, getDepartTime(clientNum), clientNum, chamada)
}

deallocateServer <- function(server) {
  servers[server, ] <<- c(F, Inf, 0, "")
}

getDepartTime <- function(clientNum) {
  simClock + data[clientNum, "servDuration"]
}

log("start", 0, chamada="")
while(numCustServed < reqCustServed){
  if(timeNextArrival < as.POSIXct(timeNextDeparture, origin = "1970-01-01")){
    update(timeNextArrival, "arrival")
    arrivingClient <- clientNum
    chamada <- data[arrivingClient, "Chamada"]
    if(allServersBusy()){
      if(substr(chamada, 2, 2)=="P"){
        #customer goes to start of the queue
        queue <- rbind(data.frame(clientNum=arrivingClient, arrivalTime=simClock, chamada=chamada), queue)      
      } else {
        #customer goes to end of the queue
        queue <- rbind(queue, data.frame(clientNum=arrivingClient, arrivalTime=simClock, chamada=chamada))
      }
    } else {
      allocateServer(clientNum, chamada)
      timeNextDeparture <- min(servers$depTime)
      serverNextDeparture <- which.min(servers$depTime)
    }
    log("arrive", arrivingClient, chamada=chamada)
    clientNum <- clientNum + 1
    timeNextArrival <- data[clientNum, "arrivalTimestamp"]
  }else{
    update(timeNextDeparture, "departure")
    departingClientNum <- servers[serverNextDeparture, "clientNum"]
    departingClientChamada <- servers[serverNextDeparture, "chamada"]
    deallocateServer(serverNextDeparture)
    numCustServed <- numCustServed + 1
    timeNextDeparture <- min(servers$depTime)
    serverNextDeparture <- which.min(servers$depTime)
    if(nrow(queue)>0){
      delay <- simClock - as.numeric(queue[1, "arrivalTime"])
      delaysTotal <- delaysTotal + delay
      allocateServer(clientNum=queue[1, "clientNum"], chamada=queue[1, "chamada"])
      timeNextDeparture <- min(servers$depTime)
      serverNextDeparture <- which.min(servers$depTime)
      queue <- queue[-1,]
    }
    log("depart", departingClientNum, chamada=departingClientChamada)
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



t.test



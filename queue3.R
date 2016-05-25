#simulador para fila única e n servos
options(stringsAsFactors = FALSE)

#state variables
simClock <- 0

queue <- 
  data.frame(
    clientNum <- numeric(),
    arrivalTime <- numeric(),
    chamada <- character(),
    stringsAsFactors = F
  )

#statistics
numCustServed <- 0
delaysTotal <- 0
delay <- 0
areaQ <- 0
areaB <- 0

data <- read.csv(file="data1718.csv", stringsAsFactors = F)
data$arrivalTimestamp <- as.POSIXct(data$arrivalTimestamp)
data$servStartTimestamp <- as.POSIXct(data$servStartTimestamp)

clientNum <- 1
timeNextArrival <- data[clientNum, "arrivalTimestamp"]
timeNextDeparture <- Inf # just to make shure the first event is an arrival

#stop criteria (number of served customers)
reqCustServed <- 500 

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

numServers <- 9
servers <- 
  data.frame(
    busy=rep(FALSE, numServers),
    depTime=Inf,
    clientNum=0,
    chamada="",
    stringsAsFactors = F
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
  servers[choosenServer, ] <<- list(busy=T, 
                                    depTime=getDepartTime(clientNum), 
                                    clientNum=clientNum, 
                                    chamada=chamada)
}

deallocateServer <- function(server) {
  servers[server, ] <<- list(FALSE, Inf, 0, "")
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
        queue <- rbind(data.frame(clientNum=arrivingClient, 
                                  arrivalTime=simClock, 
                                  chamada=chamada), 
                       queue)      
      } else {
        #customer goes to end of the queue
        queue <- rbind(queue, 
                       data.frame(clientNum=arrivingClient, 
                                  arrivalTime=simClock, 
                                  chamada=chamada))
      }
    } else {
      allocateServer(clientNum, chamada)
      timeNextDeparture <- min(servers$depTime)
      serverNextDeparture <- which.min(servers$depTime)
    }
    log("arrive", arrivingClient, chamada=chamada)
    clientNum <- clientNum + 1
    timeNextArrival <- if (is.na(data[clientNum, "arrivalTimestamp"])) Inf else data[clientNum, "arrivalTimestamp"]
  }else{
    update(timeNextDeparture, "departure")
    departingClientNum <- servers[serverNextDeparture, "clientNum"]
    departingClientChamada <- servers[serverNextDeparture, "chamada"]
    deallocateServer(serverNextDeparture)
    numCustServed <- numCustServed + 1
    if(nrow(queue)>0){
      delay <- simClock - as.numeric(queue[1, "arrivalTime"])
      delaysTotal <- delaysTotal + delay
      allocateServer(clientNum=queue[1, "clientNum"], chamada=queue[1, "chamada"])
      queue <- queue[-1,]
    }
    timeNextDeparture <- min(servers$depTime)
    serverNextDeparture <- which.min(servers$depTime)
    log(event="depart", clientNum=departingClientNum, chamada=departingClientChamada)
  }
}

write.csv()
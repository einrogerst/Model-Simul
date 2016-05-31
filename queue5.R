#simulador para fila única e n servos com curvas características do horário entre 20 e 21h
options(stringsAsFactors = FALSE)
library(VGAM)

###### funçoes auxiliares
# chegadas 20h: lognormal
# sigma = 1.544
# mu = 2.969
# location = 1.7486 
# Percentual de atendimentos preferenciais: 0.0158932

getNextArrival <- function() {
  nextTime <- simClock + as.numeric(round(1.7486 + rlnorm(1, meanlog = 2.969, sdlog=2.969)))
  nextType <- rbinom(1, 1, prob=0.0158932)
  list(time=nextTime, type=nextType)
}

# duracao 20h: gamma
# shape alpha = 0.63677
# scale beta = 660.47
# location = 3.0

getDepartTime <- function() {
  simClock + as.numeric(3 + round(rgamma(1, shape = 0.63677, scale = 660.47)))
}

update <- function(curTime, event){
  timeLast <<- simClock
  simClock <<- curTime
  diffTime <- as.numeric(curTime) - as.numeric(timeLast)
  areaQ <<- areaQ + nrow(queue) * diffTime
  areaB <<- areaB + diffTime * sum(servers$busy)
}

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
                                    depTime=getDepartTime(), 
                                    clientNum=clientNum, 
                                    chamada=chamada)
}

deallocateServer <- function(server) {
  servers[server, ] <<- list(FALSE, Inf, 0, "")
}

logDF <- data.frame()
log <- function(round, event, clientNum, chamada){
  logDF <<- rbind(logDF, 
                  data.frame(round=round,
                             time=as.POSIXct(simClock, origin = "1970-01-01"), 
                             type=event, 
                             clientNum=clientNum,
                             chamada=chamada,
                             busyServers=sum(servers$busy), 
                             queueSize=nrow(queue),
                             numDelayed = numDelayedCustomers,
                             cumQ = areaQ, 
                             cumB = areaB
                  ))
}

######################## SIMULACAO

for(simRound in 1:10){
  
  #state variables
  clientNum <- 1
  startTime <- strptime(paste(Sys.Date(), "20:00:00"), "%Y-%m-%d %H:%M:%S") 
  simClock <- startTime
  timeNextArrival <- getNextArrival()$time
  timeNextDeparture <- Inf # just to make shure the first event is an arrival
  numCustServed <- 0
  
  #statistics
  delaysTotal <- 0
  delay <- 0
  areaQ <- 0
  areaB <- 0
  numDelayedCustomers <- 0
  
  ##### entidades
  queue <- 
    data.frame(
      clientNum = numeric(),
      arrivalTime = numeric(),
      chamada = character(),
      stringsAsFactors = F
    )
  
  ##### define servers
  numInitFreeServers <- 5
  numInitBusyServers <- 4
  initDepart <- numeric()
  for(i in 1:numInitBusyServers) initDepart <- c(initDepart, getDepartTime())
  
  servers <- 
    data.frame(
      busy=c(rep(FALSE, numInitFreeServers), rep(TRUE, numInitBusyServers)),
      depTime=c(rep(Inf, numInitFreeServers), initDepart),
      clientNum=c(rep(0, numInitFreeServers), seq(from = -numInitBusyServers, to = -1)),
      chamada=c(rep("", numInitFreeServers), rep("I", numInitBusyServers)),
      stringsAsFactors = F
    )
  
  endTime <- startTime + 3600
  log(round=simRound, event="start", clientNum = 0, chamada="")
  while(simClock <= endTime){
    if(timeNextArrival < as.POSIXct(timeNextDeparture, origin = "1970-01-01")){
      update(timeNextArrival, "arrival")
      arrivingClient <- clientNum
      chamada <- if(getNextArrival()$type) "AP" else "A"
      if(allServersBusy()){
        numDelayedCustomers <- numDelayedCustomers + 1
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
      log(round = simRound, event = "arrive", clientNum = arrivingClient, chamada=chamada)
      clientNum <- clientNum + 1
      timeNextArrival <- getNextArrival()$time
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
      log(round = simRound, event="depart", clientNum=departingClientNum, chamada=departingClientChamada)
    }
  }
  
}

write.csv2(logDF, file = "logSim20.csv", row.names = FALSE)

library(ggplot2)
ggplot(data=logDF, aes(x=time, y=queueSize, group=as.factor(round))) + 
  geom_step(aes(colour=as.factor(round)), show.legend = F) +
  scale_x_datetime(limits=c(as.POSIXct(startTime), as.POSIXct(endTime)))

ggplot(data=logDF, aes(x=time, y=busyServers, group=as.factor(round))) + 
  geom_step(aes(colour=as.factor(round)), show.legend = F) +
  scale_x_datetime(limits=c(as.POSIXct(startTime), as.POSIXct(endTime)))



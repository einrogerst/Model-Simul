#simulador para fila única com prioridade
#número parametrizavel de servos (ocupados e livres na partida)
#sem retrição no tamanho da fila, e com tamanho inicial parametrizavel 
#modelo considerando curvas de distribuição dos dados obtidos entre 18 e 19h

options(stringsAsFactors = FALSE)
library(VGAM)

Sys.time()
###### funçoes auxiliares
numRounds <- 200
simTime <- 3600
numInitFreeServers <- 2
numInitBusyServers <- 7
initQueueSize <- 1

getDepartTime <- function(ini = F) {
  interval <- as.numeric(round(rdagum(1, scale = 595.51, shape1.a = 2.2426, shape2.p = 0.41321)))
  if(ini)
    depTime <- simClock + (interval/2)
  else
    depTime <- simClock + interval
  depTime
}

getArrival <- function(ini = F) {
  interval <- as.numeric(round(1.9462 + rgengamma.stacy(1, scale = 0.16301, d = 0.31255, k = 5.4788)))
  if(ini)
    arrivalTime <- simClock - (interval/2)
  else
    arrivalTime <- simClock + interval
  arrivalType <- rbinom(1, 1, prob=0.012394)
  list(time=arrivalTime, type=arrivalType)
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
                  data.frame(round = simRound,
                             time=as.POSIXct(simClock, origin = "1970-01-01"), 
                             type=event, 
                             clientNum=clientNum,
                             chamada=chamada,
                             busyServers=sum(servers$busy), 
                             queueSize=nrow(queue),
                             numDelayed = numDelayedCustomers,
                             numServed = numCustServed,
                             cumD = delaysTotal,
                             cumQ = areaQ, 
                             cumB = areaB
                  ))}

######################## SIMULACAO

for(simRound in 1:numRounds){

  ##### init state variables
  clientNum <- 1
  startTime <- strptime(paste(Sys.Date(), "18:00:00"), "%Y-%m-%d %H:%M:%S")
  simClock <- startTime
  
  ##### define filas
  queue <- 
    data.frame(
      clientNum = if(initQueueSize>0) seq(from = -initQueueSize, to = -1) else numeric(),
      arrivalTime = if(initQueueSize>0) rep(startTime, initQueueSize) else numeric(),
      chamada = rep("I", initQueueSize),
      stringsAsFactors = F
    )
  
  ##### define servers
  initDepart <- numeric()
  for(i in 1:numInitBusyServers) initDepart <- c(initDepart, getDepartTime(ini =))
  
  servers <- 
    data.frame(
      busy=c(rep(FALSE, numInitFreeServers), rep(TRUE, numInitBusyServers)),
      depTime=c(rep(Inf, numInitFreeServers), initDepart),
      clientNum=c(rep(0, numInitFreeServers), seq(from = -numInitBusyServers-initQueueSize, to = -initQueueSize-1)),
      chamada=c(rep("", numInitFreeServers), rep("I", numInitBusyServers)),
      stringsAsFactors = F
    )  

  #state variables
  timeNextArrival <- getArrival()$time
  timeNextDeparture <- min(servers$depTime)
  serverNextDeparture <- which.min(servers$depTime)
  numCustServed <- 0
  
  #statistics
  delaysTotal <- 0
  delay <- 0
  areaQ <- 0
  areaB <- 0
  numDelayedCustomers <- 0

  endTime <- startTime + simTime
  log(round=simRound, event="start", clientNum = 0, chamada="")
  while(simClock <= endTime){
    if(timeNextArrival < as.POSIXct(timeNextDeparture, origin = "1970-01-01")){
      update(timeNextArrival, "arrival")
      arrivingClient <- clientNum
      chamada <- if(getArrival()$type) "AP" else "A"
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
      log(round = simRound, event = "arrive", clientNum = arrivingClient, chamada=chamada)
      clientNum <- clientNum + 1
      timeNextArrival <- getArrival()$time
    }else{
      update(timeNextDeparture, "departure")
      departingClientNum <- servers[serverNextDeparture, "clientNum"]
      departingClientChamada <- servers[serverNextDeparture, "chamada"]
      deallocateServer(serverNextDeparture)
      numCustServed <- numCustServed + 1
      if(nrow(queue)>0){
        delay <- simClock - as.numeric(queue[1, "arrivalTime"])
        delaysTotal <- delaysTotal + delay
        numDelayedCustomers <- numDelayedCustomers + 1
        allocateServer(clientNum=queue[1, "clientNum"], chamada=queue[1, "chamada"])
        queue <- queue[-1,]
      }
      timeNextDeparture <- min(servers$depTime)
      serverNextDeparture <- which.min(servers$depTime)
      log(round = simRound, event="depart", clientNum=departingClientNum, chamada=departingClientChamada)
    }
  }
  
}

write.csv2(logDF, file = "logSim-18-9.csv", row.names = FALSE)

library(plyr)
simSummary <-
  ddply(logDF, 
        .(round), 
        function(x) c(QsizeAtStart=x[which.min(x$time), "queueSize"], 
                      busySrvAtStart=x[which.min(x$time), "busyServers"],
                      #deltaD=x[which.max(x$time), "cumD"] - x[which.min(x$time), "cumD"],
                      deltaServed=x[which.max(x$time), "numServed"] - x[which.min(x$time), "numServed"],
                      avgD=(x[which.max(x$time), "cumD"] - x[which.min(x$time), "cumD"])/(x[which.max(x$time), "numServed"] - x[which.min(x$time), "numServed"]),
                      #deltaQ=x[which.max(x$time), "cumQ"] - x[which.min(x$time), "cumQ"],
                      avgQ=(x[which.max(x$time), "cumQ"] - x[which.min(x$time), "cumQ"])/(simTime),
                      #deltaB=x[which.max(x$time), "cumB"] - x[which.min(x$time), "cumB"],
                      avgU=(x[which.max(x$time), "cumB"] - x[which.min(x$time), "cumB"])/(simTime*(numInitFreeServers+numInitBusyServers))
                      ))

write.csv2(logDF, file = "simSummary-18-9.csv", row.names = FALSE)

hist(simSummary[simSummary$avgU>0.9,"deltaServed"])
apply(simSummary[simSummary$avgU>0.9,sapply(simSummary, is.numeric)], 2, mean)
apply(simSummary[simSummary$avgU>0.9,sapply(simSummary, is.numeric)], 2, sd)
nrow(simSummary[simSummary$avgU>0.9,])

Sys.time()
# 
# library(ggplot2)
# ggplot(data=logDF, aes(x=time, y=queueSize, group=as.factor(round))) + 
#   geom_step(aes(colour=as.factor(round)), show.legend = F) +
#   scale_x_datetime(limits=c(as.POSIXct(startTime), as.POSIXct(endTime)))
# 
# ggplot(data=logDF, aes(x=time, y=busyServers, group=as.factor(round))) + 
#   geom_step(aes(colour=as.factor(round)), show.legend = F) +
#   scale_x_datetime(limits=c(as.POSIXct(startTime), as.POSIXct(endTime)))

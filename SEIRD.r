library(data.table)
library(ggplot2)

RawData <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
RawData <- RawData[location=="Hungary"]
RawData$date <- RawData$date - 1

temp <- RawData[date>="2020-08-01"&date<="2021-02-05"]

ggplot(temp, aes(x = date, y = new_deaths_smoothed_per_million)) +
  geom_line()


SEIRD <- function(time, state, parameters) {
  par <- as.list(c(time, state, parameters))
  with(par, {
    dS <- -beta*I*S/N
    dE <- beta*I*S/N - gamma*E
    dI <- gamma*E - lambda*I - mu*I
    dR <- lambda*I
    dD <- mu*I
    list(c(dS, dE,dI, dR, dD))
  })
}

N <- 10e6
init <- c(S = N - 1, E = 1, I = 0, R = 0, D = 0)
days <- 1:nrow(temp)
parameters <- c(beta = 0.4, gamma = 0.8, lambda = 0.4 ,  mu = 0.001,  h = 0.05)
sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = parameters[-5]))
sol$date <- as.Date("2020-08-01")-1+sol$time
sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
sol$D <- as.numeric(sol$D)
sol$H <- sol$I * parameters[5]
sol$H <- as.numeric(sol$H)


ggplot(merge(temp, sol), aes(x = date)) +
  geom_line(aes(y = new_deaths_smoothed)) +
  geom_line(aes(y = D), color = "red")

with(merge(temp, sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients))

RSS <- function(parameters){
  init <- setNames(c(N - parameters[1], parameters[1],0, 0, 0), c("S", "E", "I", "R", "D"))
  sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(parameters[c(2,3,4,5)], c("beta", "gamma", "lambda", "mu"))))
  sol$date <- as.Date("2020-08-01")-1+sol$time
  sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
  sol$D <- as.numeric(sol$D)
  sol$H <- sol$I * parameters[6]
  sol$H <- as.numeric(sol$H)
  
  return(with(merge(temp, sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients)))
}

RSS(c(initE = 1, beta = 0.4, gamma = 0.8, lambda = 0.4, mu = 0.001, h = 0.05))

res <- nloptr::nloptr(
  c(initE = 10, beta = 0.5, gamma = 0.5, lambda = 0.4, mu = 0.0005, h = 0.05), RSS,
  lb = c(0, 0, 0,  0, 0,0),
  ub = c(1000, 1, 1, 1, 0.001, 1),
  opts = list(algorithm = "NLOPT_GN_CRS2_LM", maxeval = 2000))

res <- nloptr::nloptr(
  res$solution, RSS,
  opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 10000))

res$sol[1]
res$sol[2]
res$sol[3]
res$sol[4]
res$sol[5]
res$sol[6]

init <- setNames(c(N - res$solution[1], res$solution[1], 0, 0, 0), c("S", "E", "I", "R", "D"))
sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(res$solution[c(2,3,4,5)], c("beta", "gamma","lambda", "mu"))))
sol$date <- as.Date("2020-08-01")-1+sol$time
sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
sol$D <- as.numeric(sol$D)
sol$H <- sol$I * res$solution[6]
sol$H <- as.numeric(sol$H)

ggplot(merge(temp, sol), aes(x = date)) +
  geom_line(aes(y = new_deaths_smoothed)) +
  geom_line(aes(y = D), color = "red")

ggplot(merge(temp, sol), aes(x = date)) +
  geom_line(aes(y = hosp_patients)) +
  geom_line(aes(y = H), color = "red")

with(merge(temp, sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients))


#res$solution[2]/(res$solution[3] + res$solution[4])
#res$solution[4]/ (res$solution[3]+res$solution[4])


#for a chosen date:

RSS <- function(parameters){
  init <- setNames(c(N - parameters[1], parameters[1],0, 0, 0), c("S", "E", "I", "R", "D"))
  sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(parameters[c(2,3,4,5)], c("beta", "gamma", "lambda", "mu"))))
  sol$date <- as.Date("2020-08-01")-1+sol$time
  sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
  sol$D <- as.numeric(sol$D)
  sol$H <- sol$I * parameters[6]
  sol$H <- as.numeric(sol$H)
  return(with(merge(temp[date<="2020-10-15"], sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients)))
}

res <- nloptr::nloptr(
  c(initE = 10, beta = 0.5, gamma = 0.5, lambda = 0.4, mu = 0.0005, h = 0.05), RSS,
  lb = c(0, 0, 0,  0, 0,0),
  ub = c(1000, 1, 1, 1, 0.001, 1),
  opts = list(algorithm = "NLOPT_GN_CRS2_LM", maxeval = 2000))

res <- nloptr::nloptr(
  res$solution, RSS,
  opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 10000))


init <- setNames(c(N - res$solution[1], res$solution[1], 0, 0, 0), c("S", "E", "I", "R", "D"))
sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(res$solution[c(2,3,4,5)], c("beta", "gamma","lambda", "mu"))))
sol$date <- as.Date("2020-08-01")-1+sol$time
sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
sol$D <- as.numeric(sol$D)
sol$H <- sol$I * res$solution[6]
sol$H <- as.numeric(sol$H)


ggplot(merge(temp, sol), aes(x = date)) +
  geom_line(aes(y = new_deaths_smoothed)) +
  geom_line(aes(y = D), color = "red") + geom_vline(xintercept = as.Date("2020-10-15"), color = "blue")


ggplot(merge(temp, sol), aes(x = date)) +
  geom_line(aes(y = hosp_patients)) +
  geom_line(aes(y = H), color = "red")+ geom_vline(xintercept = as.Date("2020-10-15"), color = "blue")


with(merge(temp, sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients))



#graph for all dates:

opt <- function(pred_date) {
  
 RSS <- function(parameters){
    init <- setNames(c(N - parameters[1], parameters[1],0, 0, 0), c("S", "E", "I", "R", "D"))
    sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(parameters[c(2,3,4,5)], c("beta", "gamma", "lambda", "mu"))))
    sol$date <- as.Date("2020-08-01")-1+sol$time
    sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
    sol$D <- as.numeric(sol$D)
    sol$H <- sol$I * parameters[6]
    sol$H <- as.numeric(sol$H)
    return(with(merge(temp[date<=pred_date], sol), MLmetrics::MAPE(D, new_deaths_smoothed) + MLmetrics::MAPE(H, hosp_patients)))
  }
  
 res <- nloptr::nloptr(
   c(initE = 10, beta = 0.5, gamma = 0.5, lambda = 0.4, mu = 0.0005, h = 0.05), RSS,
   lb = c(0, 0, 0,  0, 0,0),
   ub = c(1000, 1, 1, 1, 0.001, 1),
   opts = list(algorithm = "NLOPT_GN_CRS2_LM", maxeval = 2000))
 
 res <- nloptr::nloptr(
   res$solution, RSS,
   opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 10000))
 
 init <- setNames(c(N - res$solution[1], res$solution[1], 0, 0, 0), c("S", "E", "I", "R", "D"))
 sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD, parms = setNames(res$solution[c(2,3,4,5)], c("beta", "gamma","lambda", "mu"))))
 sol$date <- as.Date("2020-08-01")-1+sol$time
 sol$D[2:nrow(temp)] <- lapply(2:nrow(temp),function(x) sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
 sol$D <- as.numeric(sol$D)
 sol$H <- sol$I * res$solution[6]
 sol$H <- as.numeric(sol$H)
 return(sol)
}

SEIRDpreds <- rbindlist(lapply(lubridate::ymd(substring(list.files(pattern = "*.xlsx"), 1, 10) ), function(x) opt(x)[,c("date", "D", "H")]), idcol = "startdate")
SEIRDpreds$startdate <- lubridate::ymd(substring(list.files(pattern = "*.xlsx"), 1, 10))[SEIRDpreds$startdate]
SEIRDpreds  <- SEIRDpreds[date>=startdate]
SEIRDpreds <- merge(SEIRDpreds, temp)

ggplot(SEIRDpreds, aes(x = date, y = D, group = factor(startdate), color = factor(startdate))) + geom_line() + geom_line(aes(y = new_deaths_smoothed), color = "black") 


ggplot(SEIRDpreds, aes(x = date, y = H, group = factor(startdate), color = factor(startdate))) + geom_line() + geom_line(aes(y = hosp_patients), color = "black") 

startdates <-lubridate::ymd(substring(list.files(pattern = "*.xlsx"), 1, 10))
startdates_1 <- Filter(function(x) x <="2021-02-05" , startdates)
mape_deaths <-lapply(startdates_1, function(x) MLmetrics::MAPE(SEIRDpreds[startdate==x]$D,SEIRDpreds[startdate==x]$new_deaths_smoothed ))
mape_deaths_table <- data.table(startdates_1=startdates_1, mape_deaths=mape_deaths)
mape_hosp <-lapply(startdates_1, function(x) MLmetrics::MAPE(SEIRDpreds[startdate==x]$H,SEIRDpreds[startdate==x]$hosp_patients ))
mape_hosp_table <- data.table(startdates_1=startdates_1, mape_hosp=mape_hosp)
mape_table <- merge(mape_deaths_table, mape_hosp_table)

ggplot(mape_table, aes(x = startdates_1, y = as.numeric(mape_deaths), color= "deaths"))+geom_point() +geom_line() +geom_point(aes(y = as.numeric(mape_hosp), color = "hospital"))+geom_line(aes(y = as.numeric(mape_hosp), color = "hospital"))+xlab("prediction date") +ylab("MAPE")





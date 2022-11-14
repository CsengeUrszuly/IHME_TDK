RawData <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
RawData <- RawData[location=="Hungary"]
RawData$date <- RawData$date - 1

owid <- RawData[,c("date", "new_deaths_smoothed", "hosp_patients")]

library(data.table)
library(ggplot2)

get_hun_data <- function(data) {
  data_name <- rio::import(data)
  cols <- colnames(data_name)
  newcols <- unlist(strsplit(cols, ","))
  sep_data <-
    tidyr::separate(data = data_name, col = cols, into = newcols, sep = ",")
  names(sep_data) = gsub('"', "", names(sep_data))
  sep_data[,"location_name"] = gsub('"', "", sep_data[,"location_name"] )
  setDT(sep_data)
  return(sep_data)
}

IHMEpreds <- rbindlist(lapply(list.files(pattern = "*.xlsx"), function(file) get_hun_data(file)[,c("date", "deaths_lower", "deaths_mean", "deaths_upper", "allbed_lower", "allbed_mean", "allbed_upper")]), idcol = "startdate")
IHMEpreds$startdate <- lubridate::ymd(substring(list.files(pattern = "*.xlsx"), 1, 10))[IHMEpreds$startdate]
startdates <-lubridate::ymd(substring(list.files(pattern = "*.xlsx"), 1, 10))
IHMEpreds$deaths_upper <- as.numeric(IHMEpreds$deaths_upper)
IHMEpreds$deaths_lower <- as.numeric(IHMEpreds$deaths_lower)
IHMEpreds$deaths_mean <- as.numeric(IHMEpreds$deaths_mean)
IHMEpreds$allbed_lower <- as.numeric(IHMEpreds$allbed_lower)
IHMEpreds$allbed_upper <- as.numeric(IHMEpreds$allbed_upper)
IHMEpreds$allbed_mean <- as.numeric(IHMEpreds$allbed_mean)
IHMEpreds$date <- lubridate::ymd(IHMEpreds$date)
IHMEpreds  <- IHMEpreds[date>=startdate]
IHMEpreds <- merge(IHMEpreds, owid)

#predictions and actual data, daily deaths
ggplot(IHMEpreds, aes(x = date, y = deaths_mean, group = factor(startdate), color = factor(startdate))) + geom_line() + geom_line(aes(y = new_deaths_smoothed), color = "black")
#predictions and actual data, daily hospital patients
ggplot(IHMEpreds, aes(x = date, y = allbed_mean, group = factor(startdate), color = factor(startdate))) + geom_line() + geom_line(aes(y = hosp_patients), color = "black")


mape_deaths <-lapply(startdates, function(x) MLmetrics::MAPE(IHMEpreds[startdate==x]$deaths_mean,IHMEpreds[startdate==x]$new_deaths_smoothed ))
mape_deaths_table <- data.table(startdates=startdates, mape_deaths=mape_deaths)
mape_hosp <-lapply(startdates, function(x) MLmetrics::MAPE(IHMEpreds[startdate==x]$allbed_mean,IHMEpreds[startdate==x]$hosp_patients ))
mape_hosp_table <- data.table(startdates=startdates, mape_hosp=mape_hosp)
mape_table <- merge(mape_deaths_table, mape_hosp_table)

#mean absolute percentage error 
ggplot(mape_table, aes(x = startdates, y = as.numeric(mape_deaths), color= "deaths"))+geom_point() +geom_line() +geom_point(aes(y = as.numeric(mape_hosp), color = "hospital"))+geom_line(aes(y = as.numeric(mape_hosp), color = "hospital"))+xlab("prediction date") +ylab("MAPE")

MIS <- function(x,L,U) {
  value <-mean((U-L)+2/0.05*(L-x)*I(x < L) + 2/0.05*(x-U)*I(x > U))
  return(value)
}

mis_deaths <-lapply(startdates, function(x) MIS(IHMEpreds[startdate==x]$new_deaths_smoothed, IHMEpreds[startdate==x]$deaths_lower, IHMEpreds[startdate==x]$deaths_upper))
mis_deaths_table <- data.table(startdates=startdates, mis_deaths=mis_deaths)
mis_hosp <-lapply(startdates, function(x) MIS(IHMEpreds[startdate==x]$hosp_patients,IHMEpreds[startdate==x]$allbed_lower,IHMEpreds[startdate==x]$allbed_upper))
mis_hosp_table <- data.table(startdates=startdates, mis_hosp=mis_hosp)
mis_table <- merge(mis_deaths_table, mis_hosp_table)

#mis metrics from the Mexican article
ggplot(mis_table, aes(x = startdates, y = as.numeric(mis_deaths), color= "deaths"))+geom_line()+geom_point() +xlab("prediction date") +ylab("MIS")
ggplot(mis_table, aes(x = startdates, y = as.numeric(mis_hosp), color= "hospital"))+geom_line()+geom_point() +xlab("prediction date") +ylab("MIS")

coverage <- function(x,L,U){
  value <- mean(I(L < x & x < U))
  return(value)
}
IHMEcoverage_deaths <-lapply(startdates, function(x) coverage(IHMEpreds[startdate==x]$new_deaths_smoothed, IHMEpreds[startdate==x]$deaths_lower, IHMEpreds[startdate==x]$deaths_upper))
IHMEcoverage_deaths_table <- data.table(startdates=startdates, IHMEcoverage_deaths=IHMEcoverage_deaths)
IHMEcoverage_hosp <-lapply(startdates, function(x) coverage(IHMEpreds[startdate==x]$hosp_patients,IHMEpreds[startdate==x]$allbed_lower,IHMEpreds[startdate==x]$allbed_upper))
IHMEcoverage_hosp_table <- data.table(startdates=startdates, IHMEcoverage_hosp=IHMEcoverage_hosp)
IHMEcoverage_table <- merge(IHMEcoverage_deaths_table, IHMEcoverage_hosp_table)

ggplot(IHMEcoverage_table, aes(x = startdates, y = as.numeric(IHMEcoverage_deaths), color= "deaths"))+geom_line()+geom_point() +xlab("prediction date") +ylab("coverage")
ggsave("coverage_deaths.png")
                           
ggplot(IHMEcoverage_table, aes(x = startdates, y = as.numeric(IHMEcoverage_hosp), color= "deaths"))+geom_line()+geom_point() +xlab("prediction date") +ylab("coverage")
ggsave("coverage_hosp.png")

max_deaths_pred <-lapply(startdates, function(x) max(IHMEpreds[startdate==x]$deaths_mean))
max_deaths_actual <- lapply(startdates, function(x) max(IHMEpreds[startdate==x]$new_deaths_smoothed))
max_deaths_table <- data.table(startdates=startdates, max_deaths_pred=max_deaths_pred, max_deaths_actual=max_deaths_actual)
max_deaths_table$lower <- as.numeric(max_deaths_table$max_deaths_actual) * 0.8
max_deaths_table$upper <- as.numeric(max_deaths_table$max_deaths_actual) * 1.2

# predicted maximum values compared to actual maximum over the prediction intervals, daily deaths
ggplot2::ggplot(max_deaths_table, ggplot2::aes(x = startdates, y = as.numeric(max_deaths_pred), color= "predicted maximum"))+ggplot2::geom_line()+ggplot2::geom_point() +ggplot2::geom_line(ggplot2::aes(y = as.numeric(max_deaths_actual), color = "actual smoothed maximum")) +ggplot2::geom_point(ggplot2::aes(y = as.numeric(max_deaths_actual), color = "actual smoothed maximum"))+ggplot2::xlab("prediction date") +ggplot2::ylab("max of deaths over the predicted interval")+ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, color="20% error interval"))

max_hosp_pred <-lapply(startdates, function(x) max(IHMEpreds[startdate==x]$allbed_mean))
max_hosp_actual <- lapply(startdates, function(x) max(IHMEpreds[startdate==x]$hosp_patients))
max_hosp_table <- data.table(startdates=startdates, max_hosp_pred=max_hosp_pred, max_hosp_actual=max_hosp_actual)
max_hosp_table$lower <- as.numeric(max_hosp_table$max_hosp_actual) * 0.8
max_hosp_table$upper <- as.numeric(max_hosp_table$max_hosp_actual) * 1.2

# predicted maximum values compared to actual maximum over the prediction intervals, hospital patients
ggplot2::ggplot(max_hosp_table, ggplot2::aes(x = startdates, y = as.numeric(max_hosp_pred), color= "predicted maximum"))+ggplot2::geom_line()+ggplot2::geom_point() +ggplot2::geom_line(ggplot2::aes(y = as.numeric(max_hosp_actual), color = "actual smoothed maximum")) +ggplot2::geom_point(ggplot2::aes(y = as.numeric(max_hosp_actual), color = "actual smoothed maximum"))+ggplot2::xlab("prediction date") +ggplot2::ylab("max of hosp. patients over the predicted interval")+ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, color="20% error interval"))


IHMEpreds$date <- as.character(IHMEpreds$date)
max_deaths_pred <-lapply(startdates, function(x) IHMEpreds[startdate==x][which.max(IHMEpreds[startdate==x]$deaths_mean),]$date)
max_deaths_actual <- lapply(startdates, function(x)IHMEpreds[startdate==x][which.max(IHMEpreds[startdate==x]$new_deaths_smoothed),]$date)
max_deaths_table <- data.table(startdates=startdates, max_deaths_pred=max_deaths_pred, max_deaths_actual=max_deaths_actual)
max_deaths_table$max_deaths_pred <- lubridate::ymd(max_deaths_table$max_deaths_pred)
max_deaths_table$max_deaths_actual <- lubridate::ymd(max_deaths_table$max_deaths_actual)
max_deaths_table$lower <- as.numeric(max_deaths_table$max_deaths_actual) * 0.8
max_deaths_table$upper <- as.numeric(max_deaths_table$max_deaths_actual) * 1.2

#predicted vs actual peak time, daily deaths
ggplot2::ggplot(max_deaths_table, ggplot2::aes(x = startdates, y = max_deaths_pred, color= "predicted peak time"))+ggplot2::geom_point() +ggplot2::geom_line() +ggplot2::geom_point(ggplot2::aes(y = max_deaths_actual, color = "actual peak time")) +ggplot2::geom_line(ggplot2::aes(y = max_deaths_actual, color = "actual peak time"))+ggplot2::xlab("prediction date") +ggplot2::ylab("peak time of daily deaths over the predicted interval")

max_hosp_pred <-lapply(startdates, function(x) IHMEpreds[startdate==x][which.max(IHMEpreds[startdate==x]$allbed_mean),]$date)
max_hosp_actual <- lapply(startdates, function(x)IHMEpreds[startdate==x][which.max(IHMEpreds[startdate==x]$hosp_patients),]$date)
max_hosp_table <- data.table(startdates=startdates, max_hosp_pred=max_hosp_pred, max_hosp_actual=max_hosp_actual)
max_hosp_table$max_hosp_pred <- lubridate::ymd(max_hosp_table$max_hosp_pred)
max_hosp_table$max_hosp_actual <- lubridate::ymd(max_hosp_table$max_hosp_actual)
max_hosp_table$lower <- as.numeric(max_hosp_table$max_hosp_actual) * 0.8
max_hosp_table$upper <- as.numeric(max_hosp_table$max_hosp_actual) * 1.2

#predicted vs actual peak time, hospital patients
ggplot2::ggplot(max_hosp_table, ggplot2::aes(x = startdates, y = max_hosp_pred, color= "predicted peak time"))+ggplot2::geom_point() +ggplot2::geom_line()+ggplot2::geom_point(ggplot2::aes(y = max_hosp_actual, color = "actual peak time")) +ggplot2::geom_line(ggplot2::aes(y = max_hosp_actual, color = "actual peak time"))+ggplot2::xlab("prediction date") +ggplot2::ylab("peak time of hospital patients over the predicted interval")



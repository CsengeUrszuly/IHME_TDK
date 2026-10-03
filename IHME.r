library(data.table)
library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "bottom"))

RawData <- fread("https://github.com/owid/covid-19-data/raw/refs/heads/master/public/data/owid-covid-data-old.csv")
RawData$date <- RawData$date - 1

IHMEpreds <- rbindlist(lapply(list.files("./IHME_teljes/", pattern = "*.xlsx", full.names = TRUE), function(file)
  fread(text = paste0(names(readxl::read_excel(file))[1], "\n",
                      paste0(readxl::read_excel(file)[[1]], collapse = "\n")))),
  use.names = TRUE, fill = TRUE, idcol = "startdate")
IHMEpreds <- IHMEpreds[, .(startdate, date, deaths_lower, deaths_mean, deaths_upper,
                           allbed_lower, allbed_mean, allbed_upper, location_name)]
IHMEpreds$startdate <- lubridate::ymd(substring(list.files("./IHME_teljes/", pattern = "*.xlsx"), 1, 10))[IHMEpreds$startdate]
startdates <-lubridate::ymd(substring(list.files("./IHME_teljes/", pattern = "*.xlsx"), 1, 10))
IHMEpreds$deaths_upper <- as.numeric(IHMEpreds$deaths_upper)
IHMEpreds$deaths_lower <- as.numeric(IHMEpreds$deaths_lower)
IHMEpreds$deaths_mean <- as.numeric(IHMEpreds$deaths_mean)
IHMEpreds$allbed_lower <- as.numeric(IHMEpreds$allbed_lower)
IHMEpreds$allbed_upper <- as.numeric(IHMEpreds$allbed_upper)
IHMEpreds$allbed_mean <- as.numeric(IHMEpreds$allbed_mean)
IHMEpreds$date <- lubridate::ymd(IHMEpreds$date)
IHMEpreds  <- IHMEpreds[date >= startdate]
IHMEpreds <- merge(IHMEpreds,
                   RawData[, .(date, new_deaths_smoothed,
                               hosp_patients,
                               location_name = location)],
                   by = c("date", "location_name"), all.x = TRUE)

################################################################

MAPE <- function(y_pred, y_true) mean(abs((y_true - y_pred)/y_true))

plotdates <- c("2020-09-03", "2020-10-02", "2020-11-12",
               "2020-12-03", "2021-01-15", "2021-02-04",
               "2021-03-06", "2021-04-01", "2021-05-06")

ggplot(melt(IHMEpreds[location_name == "Hungary" & startdate %in% plotdates],
            id.vars = c("date", "location_name", "startdate", "new_deaths_smoothed", "hosp_patients"))[
              variable %in% c("deaths_mean", "allbed_mean"), .(date, startdate, truevalue = ifelse(variable == "deaths_mean", new_deaths_smoothed, hosp_patients),
                                                               variable = ifelse(variable == "deaths_mean", "Halálozások száma", "Kórházban kezeltek száma"), value)],
       aes(x = date, y = value, group = factor(startdate), color = factor(startdate))) +
  facet_wrap(~variable, scales = "free") + geom_line() + geom_line(aes(y = truevalue), color = "black") +
  scale_x_date(date_labels = "%Y. %m. %d.") +
  labs(x = "Dátum", y = "Szám [fő]", color = "Predikció dátuma") +
  guides(color = guide_legend(nrow = 1))
ggsave("../UjFigure1.pdf", width = 16, height = 9, device = cairo_pdf)

################################################################

N <- 10e6

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

opt <- function(pred_date, country) {  
  temp <- RawData[location == country & date >= "2020-08-01" & date <= "2021-02-05"]
  days <- 1:nrow(temp)
  
  RSS <- function(parameters) {
    init <- setNames(c(N - parameters[1], parameters[1], 0, 0, 0), c("S", "E", "I", "R", "D"))
    sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD,
      parms = setNames(parameters[c(2,3,4,5)], c("beta", "gamma", "lambda", "mu"))))
    sol$date <- as.Date("2020-08-01") - 1 + sol$time
    sol$D[2:nrow(temp)] <- lapply(2:nrow(temp), function(x)
      sol$D[as.numeric(x)] - sol$D[as.numeric(x) - 1])
    sol$D <- as.numeric(sol$D)
    sol$H <- sol$I * parameters[6]
    sol$H <- as.numeric(sol$H)
    return(with(merge(temp[date <= pred_date], sol),
                MAPE(D, new_deaths_smoothed) +
                  MAPE(H, hosp_patients)))
  }
  
  res <- nloptr::nloptr(
    c(initE = 10, beta = 0.5, gamma = 0.5, lambda = 0.4,
      mu = 0.0005, h = 0.05),
    RSS,
    lb = c(0, 0, 0,  0, 0,0),
    ub = c(1000, 1, 1, 1, 0.001, 1),
    opts = list(algorithm = "NLOPT_GN_CRS2_LM", maxeval = 2000, ranseed = 1L)
  )
  
  res <- nloptr::nloptr(res$solution, RSS, opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 10000))
  
  init <- setNames(c(N - res$solution[1], res$solution[1], 0, 0, 0), c("S", "E", "I", "R", "D"))
  sol <- as.data.frame(deSolve::ode(y = init, times = days, func = SEIRD,
    parms = setNames(res$solution[c(2,3,4,5)], c("beta", "gamma","lambda", "mu"))))
  sol$date <- as.Date("2020-08-01")-1+sol$time
  sol$D[2:nrow(temp)] <- lapply(2:nrow(temp), function(x)
    sol$D[as.numeric(x)] - sol$D[as.numeric(x)-1])
  sol$D <- as.numeric(sol$D)
  sol$H <- sol$I * res$solution[6]
  sol$H <- as.numeric(sol$H)
  return(sol)
}

SEIRDpreds <- rbindlist(lapply(lubridate::ymd(substring(list.files("./IHME_teljes/", pattern = "*.xlsx"), 1, 10)),
  function(x) opt(x, country = "Hungary")[,c("date", "D", "H")]), idcol = "startdate")
SEIRDpreds$startdate <- lubridate::ymd(substring(list.files("./IHME_teljes/", pattern = "*.xlsx"), 1, 10))[SEIRDpreds$startdate]
SEIRDpreds  <- SEIRDpreds[date >= startdate]
SEIRDpreds <- merge(IHMEpreds[location_name == "Hungary"], SEIRDpreds, by = c("date", "startdate"), all.x = TRUE)

ggplot(melt(SEIRDpreds[startdate <= "2020-11-19" & date<="2021-02-05", .(date, startdate, SEIRD = H, IHME = allbed_mean, hosp_patients)],
            id.vars = c("date", "startdate", "hosp_patients")),
       aes(x = date, y = value, group = factor(startdate), color = factor(startdate))) +
  facet_wrap(~variable) +
  geom_line() + geom_line(aes(y = hosp_patients), color = "black") +
  labs(x = "Dátum", y = "Szám [fő]", color = "Predikció dátuma")

ggplot(melt(SEIRDpreds[startdate <= "2020-11-19" & date<="2021-02-05", .(date, startdate, D, H, deaths_mean, allbed_mean, hosp_patients, new_deaths_smoothed)],
            id.vars = c("date", "startdate", "new_deaths_smoothed", "hosp_patients"))[, .(date, startdate, value, model = ifelse(variable %in% c("H", "D"), "SEIRD", "IHME"),
                                                                                          truevalue = ifelse(variable %in% c("deaths_mean", "D"), new_deaths_smoothed, hosp_patients),
                                                                                          variable = ifelse(variable %in% c("deaths_mean", "D"), "Halálozások száma", "Kórházban kezeltek száma"))],
       aes(x = date, y = value, group = factor(startdate), color = factor(startdate))) +
  facet_grid(variable ~ model, scales = "free") +
  geom_line() + geom_line(aes(y = truevalue), color = "black") +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = "Dátum", y = "Szám [fő]", color = "Predikció dátuma")
ggsave("../UjFigure2.pdf", width = 16, height = 9, device = cairo_pdf)

mape_table <- data.table(
  startdates = startdates,
  mape_deaths_seird = sapply(startdates, function(x) MAPE(SEIRDpreds[startdate==x]$D ,SEIRDpreds[startdate==x]$new_deaths_smoothed)),
  mape_hosp_seird = sapply(startdates, function(x) MAPE(SEIRDpreds[startdate==x]$H, SEIRDpreds[startdate==x]$hosp_patients)),
  mape_deaths_ihme = sapply(startdates, function(x) MAPE(SEIRDpreds[startdate==x]$deaths_mean, SEIRDpreds[startdate==x]$new_deaths_smoothed)),
  mape_hosp_ihme = sapply(startdates, function(x) MAPE(SEIRDpreds[startdate==x]$allbed_mean, SEIRDpreds[startdate==x]$hosp_patients))
)

ggplot(melt(mape_table, id.vars = "startdates"),
       aes(x = startdates, y = value,
           group = ifelse(grepl("seird", variable), "SEIRD", "IHME"),
           color = ifelse(grepl("seird", variable), "SEIRD", "IHME"))) +
  facet_wrap(~ifelse(grepl("hosp", variable), "Kórházban ápoltak száma", "Halottak száma")) +
  geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Predikció dátuma", y = "MAPE [%]", color = "Módszer")
ggsave("../UjFigure3.pdf", width = 16, height = 9, device = cairo_pdf)

melt(mape_table[!is.na(mape_deaths_seird)], id.vars = "startdates",
     measure.vars = list(seird = 2:3, ihme = 4:5))[,.(seird = mean(seird), ihme = mean(ihme)) , .(variable)]

melt(mape_table[!is.na(mape_deaths_seird)], id.vars = "startdates",
     measure.vars = list(seird = 2:3, ihme = 4:5))[, .(startdates, variable, seird, ihme, seird < ihme)]

################################################################

IHMEcoverage_table <- IHMEpreds[
  , .(coverage_deaths = mean(new_deaths_smoothed > deaths_lower & new_deaths_smoothed < deaths_upper),
      coverage_hosp = mean(hosp_patients > allbed_lower & hosp_patients < allbed_upper)),
  .(location_name, startdate)]

ggplot(melt(IHMEcoverage_table[location_name == "Hungary"], id.vars = c("location_name", "startdate")),
       aes(x = startdate, y = value, group = ifelse(variable == "coverage_deaths", "Halálozás", "Kórházban ápoltak száma"),
           color = ifelse(variable == "coverage_deaths", "Halálozás", "Kórházban ápoltak száma"))) +
  geom_line() + geom_point() + #scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0.95, color = "blue") +
  labs(x = "Predikció dátuma", y = "Lefedés", color = "")
ggsave("../UjFigure4.pdf", width = 16, height = 9, device = cairo_pdf)

################################################################

max_table <- IHMEpreds[
  , .(pred_death_value = max(deaths_mean), actual_death_value = max(new_deaths_smoothed),
      pred_death_time = date[which.max(deaths_mean)], actual_death_time = date[which.max(new_deaths_smoothed)],
      pred_hosp_value = max(allbed_mean), actual_hosp_value = max(hosp_patients),
      pred_hosp_time = date[which.max(allbed_mean)], actual_hosp_time = date[which.max(hosp_patients)]),
  .(location_name, startdate)]

ggplot(melt(max_table[location_name == "Hungary", .(location_name, startdate, pred_death_value, actual_death_value, pred_hosp_value, actual_hosp_value)],
            id.vars = c("location_name", "startdate")),
       aes(x = startdate, y = value, group = ifelse(grepl("actual", variable), "Tényleges", "Predikált"),
           color = ifelse(grepl("actual", variable), "Tényleges", "Predikált"))) +
  facet_wrap(~ifelse(grepl("death", variable), "Halálozások száma", "Kórházban ápoltak száma"), scales = "free") +
  geom_line() + geom_point() +
  labs(x = "Predikció időpontja", y = "Szám [fő]", color = "")
ggsave("../UjFigure5.pdf", width = 16, height = 9, device = cairo_pdf)

ggplot(melt(max_table[location_name == "Hungary", .(location_name, startdate, pred_death_time, actual_death_time, pred_hosp_time, actual_hosp_time)],
            id.vars = c("location_name", "startdate")),
       aes(x = startdate, y = value, group = ifelse(grepl("actual", variable), "Tényleges", "Predikált"),
           color = ifelse(grepl("actual", variable), "Tényleges", "Predikált"))) +
  facet_wrap(~ifelse(grepl("death", variable), "Halálozások száma", "Kórházban ápoltak száma"), scales = "free") +
  geom_line() + geom_point() +
  labs(x = "Predikció időpontja", y = "Időpont", color = "")
ggsave("../UjFigure6.pdf", width = 16, height = 9, device = cairo_pdf)

################################################################

ggplot(melt(IHMEpreds[location_name == "Denmark" & startdate %in% plotdates],
            id.vars = c("date", "location_name", "startdate", "new_deaths_smoothed", "hosp_patients"))[
              variable %in% c("deaths_mean", "allbed_mean"), .(date, startdate, truevalue = ifelse(variable == "deaths_mean", new_deaths_smoothed, hosp_patients),
                                                               variable = ifelse(variable == "deaths_mean", "Halálozások száma", "Kórházban kezeltek száma"), value)],
       aes(x = date, y = value, group = factor(startdate), color = factor(startdate))) +
  facet_wrap(~variable, scales = "free") + geom_line() + geom_line(aes(y = truevalue), color = "black") +
  scale_x_date(date_labels = "%Y. %m. %d.") +
  labs(x = "Dátum", y = "Szám [fő]", color = "Predikció dátuma") +
  guides(color = guide_legend(nrow = 1))
ggsave("../UjFigure7.pdf", width = 16, height = 9, device = cairo_pdf)
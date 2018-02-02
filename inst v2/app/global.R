library(shiny)
library(shinydashboard)
library(quantmod)
library(dygraphs)
library(dplyr)
library(htmlwidgets)
library(xts)


# Function definitions ###################
get_stock_matrix <- function(ti_symbols = c("AAPL",
                                            "MSFT",
                                            "GOOG"),
                             src = "yahoo",
                             start = "2015-01-01",
                             end = Sys.Date(),
                             metric = ".Close"){
  stock_env <- new.env()
  start <- as.Date(start)
  end <- as.Date(end)
  
  
  
  getSymbols(ti_symbols, src = src,
             from = start, to = end,
             env = stock_env)
  
  stock_l <- as.list(stock_env)
  stock_l <- lapply(ti_symbols, function(x){
    sel_metric <- paste0(x,metric)
    stock_l[[x]][,sel_metric]
  })
  
  # return a matrix of stocks
  # which can easily be converted to xts
  as.matrix(as.xts(do.call("cbind",stock_l)))
}

create_index <- function(xts_mat,base_period = 1,
                         times = 100){
  out <- t(t(xts_mat) / xts_mat[base_period,])
  if(!is.null(times)) out <- out * times
  out
}

ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}

create_ma_comp <- function(mat,wdw){
  d <- as.data.frame(mat)
  d$year <- format(as.Date(rownames(d)),"%Y")
  by_year <- split(d,f = d$year)
  l <- lapply(by_year, function(x){
    y_pos <- grep("year",names(by_year[[1]]))
    as.matrix(as.xts(x[,-y_pos]))
  })
  ll <- lapply(l,create_index)
  tt <- do.call("rbind",ll)
  
  d <- as.data.frame(tt)
  d$year <- format(as.Date(rownames(d)),"%Y")
  d$month <- as.numeric(format(as.Date(rownames(d)),"%m"))
  d$day <- as.numeric(format(as.Date(rownames(d)),"%d"))
  
  dp <- as_data_frame(d)
  out <- dp %>% 
    group_by(month,day) %>% 
    arrange(month,day) %>% 
    summarise_if(is.numeric,mean) 
  
  l <- lapply(out,function(x) na.omit(ma(x,wdw)))
  do.call("cbind",l[-c(1:2)])
}

#the axis label is passed as a date, this function outputs only the month of the date
getMonth <- 'function(d){
               var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
               return monthNames[d.getMonth()];
               }'

#the x values are passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
                var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                date = new Date(d);
                return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'

seasonality <- function(start_year = 2015,
                        start_month = 1,
                        start_day = 1,
                        end_year = 2017,
                        end_month = 4,
                        end_day = 30,
                        stocks = c("MSFT","GOOG"),
                        src = c("google"),
                        mva = 10){
  
  years <- seq(start_year,end_year)
  start_months <- rep(start_month, length(years))
  start_days <- rep(start_day, length(years))
  
  start_dates <- as.Date( paste(years,
                                start_months,
                                start_days,
                                sep = "."),
                          format = "%Y.%m.%d")
  
  
  end_months <- rep(end_month, length(years))
  end_days <- rep(end_day, length(years))
  end_dates <- as.Date( paste(years,
                              end_months,
                              end_days,
                              sep = "."),
                        format = "%Y.%m.%d")
  
  data <- list()
  for(i in 1:length(years) ) {
    data[[i]] <- get_stock_matrix(stocks,
                                  src,
                                  c(start_dates[i]),
                                  c(end_dates[i]),
                                  c(".Close"))
  }
  tt <- do.call("rbind",data)
  
  
  tt <- create_ma_comp(tt, wdw = mva)
  hallo <- create_index(tt)
  hallo <- as.data.frame(hallo)
  
  
  from <- start_dates[1]
  to <- end_dates[1]
  length.out <- nrow(hallo)
  
  names <- seq(from, to, by = ((to - from)/(length.out - 1)))
  #names <- format(names, format = "%m-%d")
  rownames(hallo) <- names
  hallo <- cbind(names, hallo)
  hallo <- as.matrix(hallo)
  #x <- xts(hallo[,-1], order.by=as.Date(hallo[,1]))
  x <- xts(hallo[,-1, drop=FALSE], order.by=as.Date(hallo[,1]))
  
  
}

tot_return <- function(out = x){
  
  a <- as.data.frame(out)
  end <- tail(a, n=1)
  b <- cbind(as.data.frame(colnames(a)), t(end))
  b <- t(end)
  colnames(b) <- c("Total Return")
  d <- as.data.frame(b[order(b, decreasing = TRUE),])
  d
  
  #d <- as.data.table(d)
}




# CWA CODIS 氣候資料服務系統爬蟲

library(httr)
library(lubridate)

# A1. 單站日表單 ----
codis_stn_day = function(sid, start.YYYYMMDD, end.YYYYMMDD, rm.naCol = T){
  
  DT.start = ymd(start.YYYYMMDD)
  DT.end = ymd(end.YYYYMMDD)
  
  sid.head = substr(sid, 1, 2)
  if (sid.head %in% c("46", "47")){stnType = "cwb"
  } else if (sid.head %in% c("C0")){stnType = "auto_C0"
  } else if (sid.head %in% c("C1")){stnType = "auto_C1"
  } else {stnType = "agr"}
  
  stndata = lapply(seq.Date(DT.start, DT.end, by = 1), function(DT){
 
    res = content(POST(url = "https://codis.cwa.gov.tw/api/station?",
                       body = list(
                         "date" = paste0(DT, "T00:00:00.000+08:00"),
                         "type" = "table_date",
                         "stn_ID" = sid,
                         "stn_type" = stnType,
                         "start" = paste0(DT, "T00:00:00"),
                         "end" = paste0(DT, "T23:59:59"))),
                  type = "application/json", simplifyVector = T)
    
    stndata.tmp = res$data$dts[[1]]
    
    ## 次層dataframe轉出
    idx.df = which(sapply(names(stndata.tmp), function(i) class(stndata.tmp[,i])) == "data.frame")
    
    stndata.ret = stndata.tmp[,-idx.df]
    
    for (idx in idx.df){
      tmp.df = stndata.tmp[,idx]
      names(tmp.df) = paste(names(stndata.tmp)[idx], names(tmp.df), sep = "_")
      stndata.ret = cbind(stndata.ret, tmp.df)
    }
    
    print(sprintf("processing... %s-%s", sid, DT))
    
    return(stndata.ret)
  })
  
  stndata = do.call(rbind, stndata)
  
  stndata$DataTime = ymd_hms(stndata$DataTime, tz = "Asia/Taipei")
  stndata$DataTime[format(stndata$DataTime, "%H%M") == "2359"] = stndata$DataTime[format(stndata$DataTime, "%H%M") == "2359"] + 60
  
  stndata[,grep("[fq]$", names(stndata))] = NULL
  stndata$UpdateTime = NULL
  
  if (rm.naCol){stndata[which(colSums(is.na(stndata)) == nrow(stndata))] = NULL}
  
  return(stndata)
}

# A2. 單站月表單 ----
codis_stn_month = function(sid, start.YYYYMM, end.YYYYMM, rm.naCol = T){
  
  DT.start = ym(start.YYYYMM)
  DT.end = ym(end.YYYYMM)
  
  sid.head = substr(sid, 1, 2)
  if (sid.head %in% c("46", "47")){stnType = "cwb"
  } else if (sid.head %in% c("C0")){stnType = "auto_C0"
  } else if (sid.head %in% c("C1")){stnType = "auto_C1"
  } else {stnType = "agr"}
  
  stndata = lapply(seq.Date(DT.start, DT.end, by = "1 month"), function(DT){

    res = content(POST(url = "https://codis.cwa.gov.tw/api/station",
                       body = list(
                         "date" = "2022-05-01T00:00:00.000+08:00",
                         "type" = "table_month",
                         "stn_ID" = sid,
                         "start" = paste0(DT, "T00:00:00"),
                         "stn_type" = stnType,
                         "end" = paste0(DT + months(1) - days(1), "T00:00:00"))),
                  type = "application/json", simplifyVector = T)
    
    stndata.tmp = res$data$dts[[1]]
    
    ## 次層dataframe轉出
    idx.df = which(sapply(names(stndata.tmp), function(i) class(stndata.tmp[,i])) == "data.frame")
    
    stndata.ret = stndata.tmp[,-idx.df]
    
    for (idx in idx.df){
      tmp.df = stndata.tmp[,idx]
      names(tmp.df) = paste(names(stndata.tmp)[idx], names(tmp.df), sep = "_")
      stndata.ret = cbind(stndata.ret, tmp.df)
    }
    
    print(sprintf("processing... %s-%s", sid, DT))
    
    return(stndata.ret)
  })
  
  stndata = do.call(rbind, stndata)
  
  stndata[,grep("WindDirection_CountForCode", names(stndata))] = NULL
  stndata[,grep("WindSpeed_TotalForCode", names(stndata))] = NULL
  stndata[,grep("[fq]$", names(stndata))] = NULL
  stndata$UpdateTime = NULL
  stndata$WeatherCondition = NULL
  
  for (idx in c(1, grep("Time", names(stndata)))){
    stndata[,idx] = ymd_hms(stndata[,idx], tz = "Asia/Taipei")
  }
  
  if (rm.naCol){stndata[which(colSums(is.na(stndata)) == nrow(stndata))] = NULL}
  
  return(stndata)
}

# A3. 單站年表單 ----
codis_stn_year = function(sid, start.YYYY, end.YYYY, rm.naCol = T){
  
  DT.start = ymd(paste0(start.YYYY, "-01-01"))
  DT.end = ymd(paste0(end.YYYY, "-12-31"))
  
  sid.head = substr(sid, 1, 2)
  if (sid.head %in% c("46", "47")){stnType = "cwb"
  } else if (sid.head %in% c("C0")){stnType = "auto_C0"
  } else if (sid.head %in% c("C1")){stnType = "auto_C1"
  } else {stnType = "agr"}
  
  stndata = lapply(seq.Date(DT.start, DT.end, by = "1 year"), function(DT){
    
    res = content(POST(url = "https://codis.cwa.gov.tw/api/station",
                       body = list(
                         "date" = "2022-05-01T00:00:00.000+08:00",
                         "type" = "table_year",
                         "stn_ID" = sid,
                         "stn_type" = stnType,
                         "start" = paste0(DT, "T00:00:00"),
                         "end" = paste0(DT + years(1) - days(1), "T00:00:00"))),
                  type = "application/json", simplifyVector = T)
    
    stndata.tmp = res$data$dts[[1]]
    
    ## 次層dataframe轉出
    idx.df = which(sapply(names(stndata.tmp), function(i) class(stndata.tmp[,i])) == "data.frame")
    
    stndata.ret = stndata.tmp[,-idx.df]
    
    for (idx in idx.df){
      tmp.df = stndata.tmp[,idx]
      names(tmp.df) = paste(names(stndata.tmp)[idx], names(tmp.df), sep = "_")
      stndata.ret = cbind(stndata.ret, tmp.df)
    }
    
    print(sprintf("processing... %s-%s", sid, DT))
    
    return(stndata.ret)
  })
  
  stndata = do.call(rbind, stndata)

  stndata[,grep("[fq]$", names(stndata))] = NULL
  stndata[,grep("Flag$", names(stndata))] = NULL
  stndata$UpdateTime = NULL

  for (idx in c(1, grep("Time", names(stndata)))){
    stndata[,idx] = ymd_hms(stndata[,idx], tz = "Asia/Taipei")
  }
  
  if (rm.naCol){stndata[which(colSums(is.na(stndata)) == nrow(stndata))] = NULL}
  
  return(stndata)
}

# S. 測站清單 ----
codis_stnInfo = function(){
  
  res = content(GET(url = "https://codis.cwa.gov.tw/api/station_list"),
                type = "application/json", simplifyVector = T)
  
  stnInfo = do.call(rbind, res$data$item)
  
  for (i in c("altitude", "longitude", "latitude")){
    stnInfo[,i] = as.numeric(stnInfo[,i])
  }
  
  for (i in c("stationStartDate", "stationEndDate")){
    stnInfo[,i] = ymd(stnInfo[,i], tz = "Asia/Taipei")
  }
  
  return(stnInfo)
}


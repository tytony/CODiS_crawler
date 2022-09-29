# CWB CODIS爬蟲

中央氣象局氣候資料服務系統 (<https://codis.cwb.gov.tw/StationData>) 爬蟲程序

## Usage

-   單站日表單－ codis_stn_day (sid, start.YYYYMMDD, end.YYYYMMDD, rm.naCol = T)

-   單站月表單－ codis_stn_month (sid, start.YYYYMM, end.YYYYMM, rm.naCol = T)

-   單站年表單－ codis_stn_year (sid, start.YYYY, end.YYYY, rm.naCol = T)

-   測站清單－ codis_stnInfo ()

## Arguments

-   sid－ 測站代碼（6碼）
-   start.YYYYMMDD－ 起始時間（西元YYYY年MM月DD日）
-   end.YYYYMMDD－ 結束時間（西元YYYY年MM月DD日）
-   rm.naCol－ 若為True，移除全部為NA之欄位

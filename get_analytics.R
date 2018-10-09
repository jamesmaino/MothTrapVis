library(tidyverse)

# download connection data
df <- rsconnect::showMetrics("container_status",
                             c("connect_count"),
                             server="shinyapps.io", 
                             from = "52w")

# aggregate connections by hour
dsum = 
  df %>% 
  mutate(
    date = as.POSIXct(as.numeric(as.character(timestamp)),origin="1970-01-01",tz="GMT"),
    week = format(date, '%w-%Y'),
    day = format(date, '%d%m%Y'),
    hour = format(date, '%d%m%Y% %H')
    
  )%>%
  group_by(hour)%>%
  summarise(
    date = min(date),
    mean = sum(connect_count)
  )

# plot connections per hour
plot(dsum$date,dsum$mean)

# assume each hour where app was used more than one minute is a use
sum(dsum$mean>1)
     
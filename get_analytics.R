library(tidyverse)

# download connection data
df <- rsconnect::showMetrics("container_status",
                             c("connect_count"),
                             server="shinyapps.io", 
                             from = "18w")

# aggregate connections by hour
dsum = 
  df %>% 
  mutate(
    date = as.POSIXct(as.numeric(as.character(timestamp)),origin="1970-01-01",tz="GMT"),
    week = format(date, '%w-%Y'),
    day = format(date, '%d%m%Y'),
    hour = format(date, '%d%m%Y% %H')
    
  ) %>%
  group_by(day)%>%
  summarise(
    date = min(date),
    mean = sum(connect_count)/60 # mins to hours
  )

# plot connections per hour
plot(dsum$date,dsum$mean)
ggplot(dsum) + 
  geom_line(aes(date, mean)) + 
  xlab('2018') + 
  ylab('MothTrapVis daily usage (hours)') + 
  theme_classic()
ggsave('analytics.png', height = 4, width = 6)

# assume each hour where app was used more than one minute is a use
sum(dsum$mean>1)

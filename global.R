library(dplyr)
require(RCurl)

# load data through dropbox
# d2<-read.csv(url('https://www.dropbox.com/s/9xf62j35vja2n0h/Budworm%20traps%202015%20with%20GPSgoogle.csv?raw=1'), header = FALSE, stringsAsFactors = FALSE)

# load H. punctigera data through google doc
myCsv <- getURL("https://docs.google.com/spreadsheets/d/16tksOn7SAv4ezJCX_z4no4MpJDtua6pGqVPjfg57OZ8/pub?gid=0&single=true&output=csv")
d2<-read.csv(textConnection(myCsv), header = FALSE, stringsAsFactors = FALSE)
# warning('not using internet')
# d2<-read.csv('Helicoverpa trap data - H. punctigera.csv', header = FALSE, stringsAsFactors = FALSE)


d1<-d2[8:nrow(d2),]
dates<-as.Date(d1$V1,format = '%d/%m/%Y')
d1<-as.matrix(d1)
d1<-apply(d1,2,function(x)gsub('\\s+', '',x))
d1<-d1[,-1]
d1[d1[]=='']<-NA
d<-as.data.frame(d1, stringsAsFactors = FALSE) # remove whitespace

trap1<-d2[1:7,]
trap1[trap1[]=='']<-NA
trap<-as.matrix(t(trap1))
colnames(trap) = trap[1, ] # the first row will be the header
trap = trap[-1, ]          # removing the first row.
for (myCol in c("State", "Location","District", "Operator" )){
trap[is.na(trap[,myCol]),myCol]<-'MISSING_INFORMATION'
}

# create clean data frame
# initialise dataframe
tcnames<-c('State', 'Location', 'District', 'Latitude','Longitude', 'Operator')
dcnames<-c('Date', 'Count','Misc','ID', tcnames)
df<-data.frame(matrix(NA,ncol = length(dcnames), nrow = length(d1[!is.na(d1[])])))
names(df)<-dcnames
df$Date<-as.Date(df$Date)

count<-1
for (j in 1:ncol(d)){
  for(i in 1:nrow(d)){
    if(!is.na(d1[i,j])){
      moths<-suppressWarnings(as.numeric(d1[i,j]))
      if(is.na(moths)){
        df[count,'Misc']<-d1[i,j]
      }else{
        df[count,'Count']<-moths
      }
      df[count,tcnames]<-trap[j,tcnames]
      df[count,'Date']<-dates[i]
      df[count,'ID']<-j
      count = count + 1
    }
  }
}
df$Latitude<-as.numeric(df$Latitude)
df$Longitude<-as.numeric(df$Longitude)
df$YearWeek<-df$Date

names(df)<- tolower(names(df))

# standardise
mygroups<-group_by(df, id, latitude, longitude)
cleantable <- df %>% filter(!is.na(longitude)&is.na(misc))
cleantable$id<-factor(cleantable$id)

###############################################################################
# load H. armigera data through google doc
myCsv <- getURL("https://docs.google.com/spreadsheets/d/16tksOn7SAv4ezJCX_z4no4MpJDtua6pGqVPjfg57OZ8/pub?gid=1462632689&single=true&output=csv")
d2<-read.csv(textConnection(myCsv), header = FALSE, stringsAsFactors = FALSE)
# d2<-read.csv('Helicoverpa trap data - H. armigera.csv', header = FALSE, stringsAsFactors = FALSE)
  
d1<-d2[8:nrow(d2),]
dates<-as.Date(d1$V1,format = '%d/%m/%Y')
d1<-as.matrix(d1)
d1<-apply(d1,2,function(x)gsub('\\s+', '',x))
d1<-d1[,-1]
d1[d1[]=='']<-NA
d<-as.data.frame(d1, stringsAsFactors = FALSE) # remove whitespace

trap1<-d2[1:7,]
trap1[trap1[]=='']<-NA
trap<-as.matrix(t(trap1))
colnames(trap) = trap[1, ] # the first row will be the header
trap = trap[-1, ]          # removing the first row.
for (myCol in c("State", "Location","District", "Operator" )){
  trap[is.na(trap[,myCol]),myCol]<-'MISSING_INFORMATION'
}

# create clean data frame
# initialise dataframe
tcnames<-c('State', 'Location', 'District', 'Latitude','Longitude', 'Operator')
dcnames<-c('Date', 'Count','Misc','ID', tcnames)
df<-data.frame(matrix(NA,ncol = length(dcnames), nrow = length(d1[!is.na(d1[])])))
names(df)<-dcnames
df$Date<-as.Date(df$Date)

count<-1
for (j in 1:ncol(d)){
  for(i in 1:nrow(d)){
    if(!is.na(d1[i,j])){
      moths<-suppressWarnings(as.numeric(d1[i,j]))
      if(is.na(moths)){
        df[count,'Misc']<-d1[i,j]
      }else{
        df[count,'Count']<-moths
      }
      df[count,tcnames]<-trap[j,tcnames]
      df[count,'Date']<-dates[i]
      df[count,'ID']<-j
      count = count + 1
    }
  }
}
df$Latitude<-as.numeric(df$Latitude)
df$Longitude<-as.numeric(df$Longitude)
df$YearWeek<-df$Date

names(df)<- tolower(names(df))

# standardise
mygroups<-group_by(df, id, latitude, longitude)
cleantable1 <- df %>% filter(!is.na(longitude)&is.na(misc))
cleantable1$id<-factor(cleantable1$id)






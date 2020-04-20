#06GeneratingStructuredDataset.R
requireNamespace('readxl')
#devtools::install_github('Mthrun/TSAT') ##https://github.com/Mthrun/TSAT
library('TSAT') 

##load Data ----
path=ReDi("Corona2020/90RawData",'F')
setwd(path)
DF=readxl::read_excel('Covid19data_1April2020.xlsx')
DFselected=DF[,c(1,12,13)]

path=ReDi("Corona2020/90RawData",'F')
setwd(path)
DF2=readxl::read_excel('Covid19data_16April2020.xlsx')

#Prepare to Transformation to Numeric
DF2$`Total Cases`=gsub(',','',DF2$`Total Cases`)
DF2$`Total Deaths`=gsub(',','',DF2$`Total Deaths`)
DF2$`Total Recovered`=gsub(',','',DF2$`Total Recovered`)
DF2$`Active Cases`=gsub(',','',DF2$`Active Cases`)
DF2$`Serious Critical`=gsub(',','',DF2$`Serious Critical`)
DF2$`Total Tests`=gsub(',','',DF2$`Total Tests`)

DF2$`Tot Cases/1M pop`=gsub(',','',DF2$`Tot Cases/1M pop`)

DF2$`Deaths/1M pop`=gsub(',','',DF2$`Deaths/1M pop`)
DF2$`Tests/1M pop`=gsub(',','',DF2$`Tests/1M pop`)
##Combine Data ----
DF=merge(DFselected,DF2,'Country','Country',all = T)

Time=as.Date(strptime(paste(2020,DF$FirstCaseMonth,DF$FirstCaseDay),format = '%Y %m %d'))

#Manual Imput of Missing 
ind=which(is.na(Time))
DF$Time=Time
DF$Country[ind[1]]
DF$Time[ind[1]]=as.Date('2020-04-04')#https://www.france24.com/en/20200404-falkland-islands-confirm-first-coronavirus-case

DF$Country[ind[2]]
DF$Time[ind[2]]=as.Date('2020-04-02')#https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_Afrika

DF$Country[ind[3]]
DF$Time[ind[3]]=as.Date('2020-04-05')#https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Saint_Pierre_and_Miquelon

DF$Country[ind[4]]
DF$Time[ind[4]]=as.Date('2020-04-06')#https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_Afrika

DF$Country[ind[5]]
DF$Time[ind[5]]=as.Date('2020-04-05')#https://www.afro.who.int/news/south-sudan-confirms-first-case-covid-19

DF$Country[ind[6]]
DF$Time[ind[6]]=as.Date('2020-04-08')#https://minurso.unmissions.org/regular-updates-minurso-covid-19

DF$Country[ind[7]]
DF$Time[ind[7]]=as.Date('2020-04-10')#https://www.freitag.de/autoren/dklose/first-case-of-covid-19-reported-in-yemen

which(is.na(DF$Time))


#Write Out Structured Dataset
DFrel=DF[,c(15,1,4:14)]
WriteDates('Covid19data_16April2020',DFrel,OutDirectory ="F:/Subversion/PRO/Research/Corona2020/09Originale" ,Comments = '05ClusterAnalysis_DBS.R')

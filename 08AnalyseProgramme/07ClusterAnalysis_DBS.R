#07ClusterAnalysis_DBS.R
library(DataVisualizations)#cran
library(TSAT) #github
library(DatabionicSwarm)#cran
library(GeneralizedUmatrix)#cran

# Lines 8-104 are about preprocessing
# Lines 106-133 show the application of DBS on the preprocessed data
# Lines 138-169 show exeplary evaluation of cluster quality
#               and investigate if clusters are meaningful

##LoadData And Select Numeric ----
Liste=ReadDates('Covid19data_16April2020',ReDi("Corona2020/09Originale","F"))
Data=Liste$Data
Time=Liste$Time

DaysPastOutbreak=abs(as.Date('2020-04-16')-Time)
Names=Data[,1]
Data2=cbind(Data[,2:ncol(Data)],DaysPastOutbreak)
PlotMissingvalues(Data2)
colnames(Data2)
DataSelected=Data2[,-c(2,4)]

## Account of Missing Value ----
mode(DataSelected)="numeric"
PlotMissingvalues(DataSelected)
colnames(DataSelected)

#Imputation
DataSelected=DataSelected[,-5]

PlotMissingvalues(DataSelected)
DataSelected[!is.finite(DataSelected)]=NaN
DataKnnimputed=KNNimputation(DataSelected,k = 7)#internal function
#now imputation with k-nearest neighbor approach is performed.
setwd(ReDi('Corona2020/01Transformierte','F'))
save(file='Covid19data_16April2020.rda',DataKnnimputed,Names,DataSelected)

## Preprocessing  ----
setwd(ReDi('Corona2020/01Transformierte','F'))
load(file='Covid19data_16April2020.rda')#,DataKnnimputed,Names,DataSelected)

Trans=DataKnnimputed
MDplot(Trans,Scaling = 'Log')

colnames(Trans)

# Acccount for Variance ----
Trans[,c(1:8)]=SignedLog(Trans[,c(1:8)])

MDplot(Trans)

# Account for Correlations ----
cc=cor(Trans)
diag(cc)=0
Pixelmatrix(cc)
colnames(Trans)
plot(Trans[,c(1,4)])
plot(Trans[,c(2,4)])
plot(Trans[,c(7,4)])

Trans=Trans[,-4]

cc=cor(Trans)
diag(cc)=0
Pixelmatrix(cc)

plot(Trans[,c(1,2)])
plot(Trans[,c(1,3)])

ma=MAplot(Trans[,1],Trans[,3],islog = F)

Trans[,1]=ma$MA[,1]
Trans[,3]=ma$MA[,2]

cc=cor(Trans)
diag(cc)=0
Pixelmatrix(cc)

plot(Trans[,c(3,2)])
plot(Trans[,c(3,6)])

ma=MAplot(Trans[,3],Trans[,5],islog = F)

Trans[,3]=ma$MA[,1]
Trans[,6]=ma$MA[,2]

cc=cor(Trans)
diag(cc)=0
Pixelmatrix(cc)

plot(Trans[,c(2,6)])
plot(Trans[,c(4,7)])

ma=MAplot(Trans[,4],Trans[,7],islog = F)

Trans[,4]=ma$MA[,1]
Trans[,7]=ma$MA[,2]

cc=cor(Trans)
diag(cc)=0
Pixelmatrix(cc)

Trans[,8]=Trans[,8]/100
MDplot(Trans)
# With more work, the variance could be standardized better.
#However, data serves only as an illustrative example for DBS
setwd(ReDi('Corona2020/01Transformierte','F'))
save(file='Covid19data_16April2020_Transformed_decorrelated.rda',Trans,DataKnnimputed,Names,DataSelected)

## Cluster Analysis with DBS ----
setwd(ReDi('Corona2020/01Transformierte','F'))
load(file='Covid19data_16April2020_Transformed_decorrelated.rda')#,Trans,DataKnnimputed,Names,DataSelected)

#1st DBS module
proj=Pswarm(Trans)#then choose euclidean

#2nd DBS module
genU=GeneratePswarmVisualization(Trans,ProjectedPoints = proj$ProjectedPoints,proj$LC)
plotTopographicMap(genU$Umatrix,genU$Bestmatches,NoLevels=8)#3clusters
NormalizedUmatrix=NormalizeUmatrix(Trans,genU$Umatrix,genU$Bestmatches) #improves visualizaton sometimes

#3rd DBS module
Cls=DBSclustering(k = 3,DataOrDistance = Trans,PlotIt = T,StructureType = T,BestMatches = genU$Bestmatches,genU$LC)
plotTopographicMap(NormalizedUmatrix,genU$Bestmatches,Cls = Cls,NoLevels=8)#3 clusters

library(ProjectionBasedClustering)#CRAN
#Cut out Island of 3D landscape
imx=interactiveGeneralizedUmatrixIsland(NormalizedUmatrix,genU$Bestmatches,Cls = Cls)

#Mark Outliers interactively (and maybe detected further clusters)
Cls2=interactiveClustering(NormalizedUmatrix,genU$Bestmatches,Cls = Cls,Imx = imx)

Cls3=RenameDescendingClassSize(Cls2)

setwd(ReDi('Corona2020/01Transformierte','F'))
save(file='Covid19data_16April2020_DBSclustering.rda',Trans,DataKnnimputed,Names,DataSelected,Cls3,imx,genU,proj,NormalizedUmatrix)#

## External Evaluation of Clustering ----
setwd(ReDi('Corona2020/01Transformierte','F'))
load(file='Covid19data_16April2020_DBSclustering.rda')#,Trans,DataKnnimputed,Names,DataSelected,Cls3,imx,genU,proj,NormalizedUmatrix)#

plotTopographicMap(NormalizedUmatrix,genU$Bestmatches,Cls = Cls3,Imx = imx,NoLevels=8)

Heatmap(DistanceMatrix(Trans),Cls3)
ClassCount(Cls3)

Cls4=Cls3
Cls4[Cls4>5]=6

library(countrycode) #cran
c_codes=countrycode::countrycode(Names, origin = 'country.name', destination = 'iso3c')

ind=order(Cls4)
DataVisualizations::Worldmap(CountryCodes = c_codes[ind],Cls = Cls4[ind],Colors = DatabionicSwarm::DefaultColorSequence[1:6],MissingCountryColor = 'grey')

## Meaningful Cluster Structures ? ----
#internal Function
tree=trainbestCART(DataSelected,colnames(DataSelected),Cls4)
classrules=CART2Rules(tree)
classrules
ClassNames=c('LowDeathRate&ManyRecovered','LowCasesLowTests','LowCasesLowPop','HighCasesAndDeaths','HighDeathsMediumCasesORLowCasesLowPop','Outliers')
names(ClassNames)=DatabionicSwarm::DefaultColorSequence[1:6]
ClassNames
plotTopographicMap(NormalizedUmatrix,genU$Bestmatches,Cls4,Imx = imx,Names = ClassNames)
library(rgl)
rgl.snapshot('TopograhpicMap_Covid19.png')
#The topographic map can be also printed in 3D, please see documentation
setwd(ReDi('Corona2020/01Transformierte','F'))
save(file='Covid19data_16April2020_DBSclustering_Exlained.rda',Trans,DataKnnimputed,Names,DataSelected,Cls3,imx,genU,proj,NormalizedUmatrix,ClassNames,c_codes,tree,classrules)
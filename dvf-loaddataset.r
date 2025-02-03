################################################################################
# DVF (Données de Valeur Foncières) : https://explore.data.gouv.fr/fr/immobilier
# training: ventes Chaville/Viroflay/Versailles < 2024, prediction: ventes Chaville/Viroflay/Versailles 2024 ou liste explicite à estimer
# source: https://explore.data.gouv.fr/fr/immobilier?onglet=carte&filtre=tous&lqt=46.89392&lng=2.18672&zoom=12.00&lat=48.80794&code=92022&level=commune
#
recode_str_int <- function(df, feature) {
	list_str <- sort(unique(df[,feature]))
	list_int <- 1:length(list_str)
	names(list_int) = list_str
	df$feature_new = list_int[df[,feature]]
	df$feature_new
}

if (setdebug==1) {
	Sys.getenv()
}

# setsource
# 0: dvf-chaville
# 1: dvf-viroflay
# 2: dvf-chaville+2 samples
# 3: dvf-viroflay+1 sample
# 4: dvf-versailles
# 5: dvf-versailles sample
#
library(dplyr)
if (setsource==0) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-chaville.csv")
	pryear="2024"

} else if (setsource==1) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-viroflay.csv")
	pryear="2024"

} else if (setsource==2) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-chaville.csv")
	dfn<-read.csv("c:\\ML\\dvfnew.csv",sep=";")
	dfn=filter(dfn,code_postal==92370)
	df0<-rbind(df0,dfn)
	pryear="2025"
} else if (setsource==3) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-viroflay.csv")
	dfn<-read.csv("c:\\ML\\dvfnew.csv",sep=";")
	dfn=filter(dfn,code_postal==78220)
	df0<-rbind(df0,dfn)
	pryear="2025"
} else if (setsource==4) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-versailles.csv")
	pryear="2024"
} else if (setsource==5) {
	df0<-read.csv("c:\\users\\steph\\downloads\\dvf-versailles.csv")
	dfn<-read.csv("c:\\ML\\dvfnew.csv",sep=";")
	dfn[is.na(dfn)]<-0
	dfn=filter(dfn,code_postal==78000)
	df0<-rbind(df0,dfn)
	pryear="2025"
} else {
	print("wrong setsource")
}

# Filter on type_local in {"Maison","Appartement"}, nature_mutation is "Vente", valeur_fonciere in [70000, 2500000], unique id_prefixe
# Create id_type_local from type_local string
# Create surface_carrez from various surface_carrez fields
# split dataframe obtained df into dt (training) and dp (prediction)
#
df1=filter(df0,
	  (type_local=="Maison" | type_local=="Appartement")
	& nature_mutation=="Vente"
	& valeur_fonciere >   70000
	& valeur_fonciere < 2500000
)
df2 <- df1 %>% add_count(df1$id_mutation,name="id_occurrence")
df2$id_prefixe<-recode_str_int(df2,"section_prefixe")
df2$id_type_local<-recode_str_int(df2,"type_local")
df <- filter(df2,
	id_occurrence==1
)

dt=filter(df,
	substring(id_mutation,1,4)!=pryear
)
dt[is.na(dt)]<-0
dt$surface_carrez<-floor(
	dt$lot1_surface_carrez+dt$lot2_surface_carrez+dt$lot3_surface_carrez+dt$lot4_surface_carrez+dt$lot5_surface_carrez
)
dt$surface_carrez<-pmax(dt$surface_carrez,dt$surface_reelle_bati)

dp=filter(df,
	substring(id_mutation,1,4)==pryear
)
dp[is.na(dp)]<-0
dp$surface_carrez<-floor(
	dp$lot1_surface_carrez+dp$lot2_surface_carrez+dp$lot3_surface_carrez+dp$lot4_surface_carrez+dp$lot5_surface_carrez
)
dp$surface_carrez<-pmax(dp$surface_carrez,dp$surface_reelle_bati)


# https://www.sthda.com/french/wiki/ggplot2-histogramme-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
#
library(ggplot2)
ggplot(dt,aes(x=valeur_fonciere))+geom_histogram()
ggplot(dt,aes(x=valeur_fonciere))+geom_histogram(binwidth=50000,colour="black",fill="white")
ggplot(dt,aes(x=valeur_fonciere))+geom_density()
ggplot(dt,aes(x=valeur_fonciere))+geom_histogram(binwidth=50000,colour="black",fill="white") + geom_density(alpha=.2,fill="#FF6666")

# https://r-charts.com/distribution/histogram-group-ggplot2/
#
ggplot(dt,aes(x=valeur_fonciere,fill=type_local))+geom_histogram()
ggplot(dt,aes(x=valeur_fonciere,fill=type_local))+geom_density()

# chart shows different price distribution between Appartment and House (wider sd)
ggplot(dt,aes(x=valeur_fonciere,fill=type_local))+geom_density()+geom_vline(aes(xintercept=mean(valeur_fonciere)), color="blue", linetype="dashed", size=1)


library("data.table")
tt<-setDT(dt)
tp<-setDT(dp)

################################################################################
# data and kpi functions
#
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}

mae <- function(y,y_pred) {
  return(mean( abs(y-y_pred) ))
}

rmse <- function(y,y_pred) {
  return(sqrt((mean(y-y_pred))^2))
}

accuracy <- function(y,y_pred) {
    sum(abs(y-y_pred)<20000)/length(y)
}

accuracy2 <- function(actual,predicted) {
	difference <- ((actual-predicted)/actual)
	return(1-abs(mean(difference)))
}

statdf <- function(original,predicted,dataframe) {
    return( data.frame(
        Item = c('mean(original)','sd(original)','mean(predicted)','sd(predicted)','rmse','mae','accuracy','accuracy2','rows'), 
        Value = c(
            format(round(mean(original), 1), nsmall = 1),
            format(round(sd(original), 1), nsmall = 1),
            format(round(mean(predicted), 1), nsmall = 1),
            format(round(sd(predicted), 1), nsmall = 1),
            round(rmse(original,predicted),digits=1),
            round(mae(original,predicted),digits=1),
            round(accuracy(original,predicted),digits=1),
	    round(100*accuracy2(original,predicted),digits=1),            
            format(round(nrow(dataframe), 0), nsmall = 0)
        ),
        check.names = FALSE
    ))
}

################################################################################
# display functions
#
# plot original & predicted
plot_original_predicted <- function(original,predicted) {

    x_axes = seq(1:length(predicted))
    plot(x_axes, original, type="l", col="red")
    lines(x_axes, predicted, col="blue")
    legend("topleft", legend=c("y-original", "y-predicted"), col=c("red", "blue"), lty=1,cex=0.8)
}

# plot original & 2 predicted
plot_original_firstprediction_secondprediction <- function(original,firstprediction,secondprediction) {

    x_axes = seq(1:length(original))
    plot(x_axes, original, type="l", col="black")
    lines(x_axes, firstprediction, col="red")
    lines(x_axes, secondprediction, col="blue")    
    legend("topleft", legend=c("y-original", "y-firstprediction", "y-secondprediction"), col=c("black", "red","blue"), lty=1,cex=0.8)
}

# plot histogram and gaussian
plot_histogram_and_gaussian <- function(orig,pred) {

    library(ggplot2)
    library(reshape2)
    library(tidyr)

    bw=10000
    n_obs=sum(!is.na(orig))
    df1<-data.frame(orig,pred)
    df1 %>% 
        gather(key=Type, value=Value) %>% 
        ggplot(aes(x=Value,fill=Type)) + 
#        geom_histogram(position="dodge",binwidth=bw)+
        stat_function(fun=function(x) dnorm(x,mean=mean(orig),sd=sd(orig))*bw*n_obs,colour = "red") +
        stat_function(fun=function(x) dnorm(x,mean=mean(pred),sd=sd(pred))*bw*n_obs,colour = "dark green") +
        scale_x_continuous("x")
}

# plotly barchart x axis=featureid, 2 variables=y, y_mlr
#
plot_barchart <- function(featureid,orig,pred) {

    library(ggplot2)
    library(reshape2)
    library(tidyr)
    
    df1<-data.frame(orig,pred,featureid)
    df2<-melt(df1,id.vars='featureid')
    g<-ggplot(df2,aes(x=featureid,y=value,fill=variable))+geom_bar(stat='identity',position='dodge')
    g<-g+theme(axis.text.x=element_text(angle=90,hjust=1))
    plotly::ggplotly(g)
}


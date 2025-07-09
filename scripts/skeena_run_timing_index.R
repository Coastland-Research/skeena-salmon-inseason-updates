library(data.table)
library(tidyverse)
library(ggplot2)
library(utilizer)
library(remotes)
library(zoo)

install.packages("remotes") 
remotes::install_gitlab("MichaelFolkes/utilizer") 

# df<-fread("tyee sockeye index values.csv",header=TRUE)
df<-fread("data/current_year/tyee data 2025.csv") %>%
  select(Date,"2025"=sockeye)

# comment -----------------------------------------------------------------

#Author: Michael Folkes (michael.folkes@dfo-mpo.gc.ca)
#source: https://gitlab.com/MichaelFolkes/inseason

#this will plot time series (x=year, y=date value), each facet is a percentile estimate of a date. 
# eg cpue by day and year. first calc the cumulative cpue by day and year then plots 
# percentiles of the cumulative values.

#how to run: source the function defs section below then jump to the examples section 
# and run those lines.

# setup -------------------------------------------------------------------

rm(list=ls())

# function defs -----------------------------------------------------------

#' Calculate daily proportions
#'
#' @param x A data frame. The data frame named 'data.daily', which is output of \code{\link{calcTyeeCPUEindex}} data.daily
#'
#' @return A data frame. Needs to include columns named: 'year' (even if data set has just one year) and 'value', which is the value that will have daily proportions estimated from.
#'
#' @export
#'
#' @examples
#' 
#' 
#' 

calcDailyProportions <- function(x, signif.dec=3){
  output <- list()
  data.yearly.daily <- x
  
  # x.cumsum <- by(x$cpue, x$year,FUN = function(x.sub) data.frame(prop=x.sub/sum(x.sub), prop.cumul=cumsum(x.sub)/sum(x.sub)))
  # results$cumulativesum <- cbind(x, do.call(rbind, x.cumsum))
  
  data.yearly.daily <- by(data.yearly.daily, data.yearly.daily$year,FUN = function(x) {
    dat.tmp <- data.frame(x, cpue.cumul=cumsum(x$cpue), prop=x$cpue/sum(x$cpue), prop.cumul=cumsum(x$cpue)/sum(x$cpue))
    row.names(dat.tmp) <- NULL
    dat.tmp
  })
  output$data.yearly.daily <- data.yearly.daily <- do.call(rbind, data.yearly.daily)
  
  #is it the daily mean of the yearly-daily cumulative prop, or the daily cumulative of the daily mean prop?
  #I assume the first, as that sums cleanly to 1.
  output$data.daily <- aggregate(prop.cumul~od, data=data.yearly.daily,function(x) {
    x[x==0] <- 0.0001
    x[x==1] <- 0.9999
    c(mean=mean(x), mean.logit=mean(logit(x)), sd.logit=sd(logit(x)))
  })
  
  #alternate method
  if(!is.null(signif.dec)){
    q.list <- by(x, x$year,FUN = function(x.sub) {
     
      dat.tmp <- rep(x.sub$od, round(x.sub$cpue * 10^signif.dec,0))
      probs <- seq(0.1, 0.9, by=0.1)
      quantiles <- quantile(dat.tmp, probs = probs,na.rm=TRUE)
      q.df <- data.frame(year=unique(x.sub$year), p.f=probs, quantiles=quantiles)
      
      counts <- table(dat.tmp)
      maxod <- names(counts)[which.max(counts)]
      mode.df <- data.frame(year=unique(x.sub$year), p.f="mode", quantiles=maxod)
      
      peak.day<-x.sub$od[which.max(rollapply(x.sub$cpue,5,mean,fill=NA,align="center"))]
      peak<-data.frame(year=unique(x.sub$year), p.f="peak", quantiles=peak.day)
      
      q.df <- rbind(q.df, mode.df,peak)
      q.df$p.f <- factor(q.df$p.f, levels=c("0.1","0.2","0.3","0.4","0.5","mode","peak","0.6","0.7","0.8","0.9"))
      #q.df$date.standard <- utilizer:::calcDate(paste0("2021-", q.df$quantiles), "%Y-%j")
      row.names(q.df) <- NULL
      q.df
      
    })
    output$quantiles <- do.call(rbind, q.list)
  }
  
  output
  
}#END calcDailyProportions

plotPercentileDates <- function(data.percentile.date, filename = NA){
  
  p <- ggplot(data=data.percentile.date, aes(year, date.standard))+
    #geom_vline(xintercept = c(1987.5, 2007.5), col='grey')+
    geom_point()+
    geom_line()+
    xlab("Year")+ylab("Standard Date")+
    #scale_y_date(date_breaks = "6 days", date_labels = "%b-%d")+
    facet_grid(vars(p.f), scales = "free_y")
  
  if(! is.na(filename)){
    #filename <- "../../plots/percentile_dates_timeseries_outliersexcluded.png"
    ggsave(filename,plot = p, height = 11, width=6,dpi=600)
  }else{
    print(p)
  }
  
}#END plotPercentileDates


logit <- function(x){
  log(x/(1-x))
}# END logit

logit.inverse <- function(x){
  return(1/(1+exp(-x)))
}# END logit.inverse

getDecimals <-function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1}

# example -----------------------------------------------------------------

#input
#data.daily must be a data frame with columns: year, od, cpue (if your not plotting cpue data maybe just call your data cpue anyhow, or you'll have to rename all the refs to cpue in the two functions)

#how to calc ordinal date from date obj:
#od=ordinal day
#fyi get od by:
x <- seq(as.Date("2020-1-1"), as.Date("2020-2-1"), by='day')
x
#note the'j', erroneously indicating julian:
as.integer(format(x, "%j"))


# my data:
daily<-fread("data/common/tyee_indices_sockeye_1956-2024.csv") %>%
  mutate_at(c("1956", "1972", "1974", "1979", "2022"), as.numeric)

current<-fread("data/current_year/tyee data 2025.csv") %>%
  select(Date,"2025"=sockeye) 

df<-left_join(daily,current,by="Date")%>%
  mutate(Date=as.Date(Date))

df2 <- df %>%
  pivot_longer(`1956`:`2025`, names_to="Year",values_to="index") %>%
  # pivot_longer(2:66,names_to = "year",values_to = "index") %>%
  mutate(od=as.numeric(format(as.Date(Date),"%j")),
         year=as.numeric(Year),
         cpue=index,
         cpue = replace_na(cpue, 0)) %>%
  select(-Date,-index)
#write.csv(df2,"tyee sockeye index data long.csv")


df2$cpue[is.na(df2$cpue)]<-0

# df4<-df3%>%
# names(df3)

#making a fake data set of year, od, and cpue
data.daily <- expand.grid(year=2000:2020, od=200:300)
data.daily$cpue <- runif(n = nrow(data.daily), min=1, max=20)

data.proportions <- calcDailyProportions(x = data.daily)
########

# df<-fread("tyee sockeye index values.csv",header=TRUE)%>%select(-`2021`)

index.data <- df %>%
  pivot_longer(`1956`:`2025`,names_to="Year",values_to="index") %>%
  mutate(od=as.numeric(format(as.Date(Date),"%j")),
         year=as.numeric(Year),
         cpue=index,
         cpue = replace_na(cpue, 0))%>%
  select(-Date,-index)

#index.data<-index.data%>%filter(year>2010)
data.proportions <- calcDailyProportions(x = df2)

data.proportions$quantiles$od<-as.numeric(data.proportions$quantiles$quantiles)
data.proportions$quantiles$date.standard<-as.Date(data.proportions$quantiles$od, origin = "2020-01-01")

plotPercentileDates(data.proportions$quantiles)


## myplot
ggplot(data.proportions$quantiles, aes(x = year, y = date.standard)) +
  geom_point()+
  geom_line()+
  # geom_vline(xintercept=2014,color="red",linetype='dashed')+
  facet_grid(vars(p.f),scales="free_y")+
  labs(title="Skeena sockeye: Tyee index timing quantiles")+
  ylab("Date") + xlab("Year") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1956, 2025, by = 5)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# takes index data (for sockeye) as input. plot date by year 
ggsave("skeena sockeye tyee index percentiles.png",dpi=600,height=10,width=7)  
  


some<-data.proportions$quantiles%>%filter(p.f%in%c("0.1","0.5","peak","0.9"))

ggplot(some,aes(x=year,y=date.standard))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=2014,color="red",linetype='dashed')+
  facet_grid(vars(p.f),scales="free_y")+
  labs(title="Skeena sockeye: Tyee index timing quantiles")+
  ylab("Date")+xlab("Year")+
  theme_bw()

ggsave("skeena sockeye tyee index some percentiles.png",dpi=600,height=7,width=7)  

ggh<-some%>%select(year,date.standard,p.f)%>%filter(p.f%in%c("0.1","0.9"))%>%
  pivot_wider(year,values_from=date.standard,names_from=p.f)%>%
  mutate(dif=`0.9`-`0.1`)

ggplot(ggh,aes(x=year,y=dif))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept=2014,color="red",linetype='dashed')+
  labs(title="Skeena sockeye: Length of Run",
       subtitle="Number of days between 10 and 90th quantiles")+
  ylab("# of Days")+xlab("Year")+
  theme_bw()

ggsave("skeena sockeye tyee index length of run.png",dpi=600,height=7,width=7)  


str(data.proportions$data.yearly.daily)

#if a filename is given, it writes the plot:
plotPercentileDates(data.proportions$quantiles, filename = "myplot.png")
shell.exec("myplot.png")

df


names(df3)
unique(df3$year)
y<-unique(df3$year)
qs<-factor

store<-data.frame(matrix(nrow=length(y)*length(qs),ncol=3))

for (i in 1:length(y)) {
  
}

quantile(df[,2],.5,na.rm=TRUE)

apply(df3)

mode(x)

x <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5)

length(x)

rollapplyr(x,3,mean,fill=NA,align="center")

quantile(x)

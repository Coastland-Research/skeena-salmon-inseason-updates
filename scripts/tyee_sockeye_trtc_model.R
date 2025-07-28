
tyee.sx.data<-fread("data/current_year/tyee data 2025.csv")%>%
  select(Date,Runtiming,esctyee)%>%
  filter(Date>=as.Date("2025-06-10"))%>%
  mutate(cumesc=cumsum(esctyee))

gncatch<-fread("data/current_year/commercial catch 2025-gillnet.csv")%>%
  select(Date,gncatch=`Sockeye (Kept)`) %>% 
  replace(is.na(.), 0)
  
sncatch<-fread("data/current_year/commercial catch 2025-seine.csv")%>%
  select(Date,sncatch=`Sockeye (Kept)`) %>% 
  replace(is.na(.), 0)

total.catch<-left_join(gncatch,sncatch)


sx.trtc.model.all <- left_join(tyee.sx.data, total.catch) %>%
  mutate(
    catch = gncatch + sncatch,
    rtlate = lag(Runtiming, 7),
    rtearly = lead(Runtiming, 7),
    adjusted_catch = rollmean(catch, k = 5, fill = NA, align = "right"),
    daily_trtc = (adjusted_catch + esctyee),
    daily_trtc = replace_na(daily_trtc,0),
    cum_trtc = cumsum(daily_trtc),
    Average = cum_trtc / Runtiming,
    Early = cum_trtc / rtearly,
    Late = cum_trtc / rtlate
  )


sx.trtc.model<-sx.trtc.model.all%>%
  select(Date,Early,Average,Late)%>%
  pivot_longer("Early":"Late",names_to="Timing",values_to="Estimate")%>%
  filter(Date<=tyee.day)

todays.trtc.estimates<-sx.trtc.model.all%>%filter(Date==tyee.day)

cumesctodate<-tyee.sx.data%>%
  filter(Date==tyee.day)%>%
  getElement("cumesc")
  
early=round(todays.trtc.estimates$Early,0)
average=round(todays.trtc.estimates$Average,0)
late=round(todays.trtc.estimates$Late,0)

rtearly<-paste0(round(todays.trtc.estimates$rtearly*100,1),"%")
rtaverage<-paste0(round(todays.trtc.estimates$Runtiming*100,1),"%")
rtlate<-paste0(round(todays.trtc.estimates$rtlate*100,1),"%")

trtc.table<-tibble("Run-timing"=c("Early","Average","Late"),
                   "Run to Tyee to Date"=cumesctodate,
                   "% of Run Through"=c(rtearly,rtaverage,rtlate),
                   "TRTC Estimate"=c(early,average,late))


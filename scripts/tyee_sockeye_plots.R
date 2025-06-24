#load Tyee data, add cum escapement, make escapement plots

make.tyee.sockeye.escapement.plots<-function() {

tyee.sx.data<-fread("data/current_year/tyee data 2025.csv")%>%
  select(Date,"2025"=esctyee)

hist.sx.data<-fread("data/common/tyee_daily_sockeye_escapement_1970-2024.csv")

all.sx.data<-left_join(hist.sx.data,tyee.sx.data,by="Date")%>%
  mutate(Date=as.Date(Date))

sx.daily<-all.sx.data%>%
  #mutate_if(is.character, as.numeric) %>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Fish,0))

sx.quants<-sx.daily %>%
  group_by(Date) %>%
  summarise(per10 = quantile(Fish,.1, na.rm = TRUE), 
            per25 = quantile(Fish,.25, na.rm = TRUE), 
            per50 = quantile(Fish,.50, na.rm = TRUE),
            per75 = quantile(Fish,.75, na.rm = TRUE),
            per90 = quantile(Fish,.9, na.rm = TRUE))%>%
  select(Date,per10,per25,per50,per75,per90)%>%
  pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
  group_by(Q)%>%
  mutate(Fish=(Index))%>%
  mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                          Q=="per25"|Q=="per75"~"25/75th",
                          Q=="per50"~"Median"))

sx.daily.cum<-all.sx.data%>%
  pivot_longer("1970":"2025",names_to="Year",values_to="Fish") %>%
  group_by(Year)%>%
  mutate(cum_sum=cumsum(replace_na(Fish,0)))

sx.cum.quants<-sx.daily.cum %>%
  group_by(Date) %>%
  summarise(per10 = quantile(Fish,.1, na.rm = TRUE), 
            per25 = quantile(Fish,.25, na.rm = TRUE), 
            per50 = quantile(Fish,.50, na.rm = TRUE),
            per75 = quantile(Fish,.75, na.rm = TRUE),
            per90 = quantile(Fish,.9, na.rm = TRUE))%>%
  select(Date,per10,per25,per50,per75,per90)%>%
  pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
  group_by(Q)%>%
  mutate(cum_sum=cumsum(Index))%>%
  mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                          Q=="per25"|Q=="per75"~"25/75th",
                          Q=="per50"~"Median"))

sx.esc.daily<-make.daily.esc.plot(sx.daily,sx.quants,0,75000,"2025-06-10","2025-06-30")
sx.esc.cum<-make.cum.esc.plot(sx.daily.cum,sx.cum.quants,50000,"2025-06-10","2025-06-30")

ggarrange(sx.esc.daily,sx.esc.cum,align="v",ncol=1,common.legend = TRUE,legend="bottom")

}

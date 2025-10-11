
library(lubridate)

make.babine.figures<-function(species.in,babine.figures.x.high,daily.yhigh,cum.yhigh) {

historical<-read_excel("data/common/babine fence counts 1946-2024 compiled 20240717.xlsx",
                       sheet=species.in)%>%
  mutate(Date=as.Date(Date))%>%
  mutate_at(vars("1946":"2024"), ~replace(., is.na(.), 0)) %>%
  mutate(Date=as.Date(paste("2025",month(Date),day(Date),sep="-")))

#str(historical)

current<-fread("data/current_year/babine fence 2025.csv") %>%
select(Date,"2025"=species.in)%>%
  mutate(Date=as.Date(paste("2025",month(Date),day(Date),sep="-")))

#str(current)

daily.index<-left_join(historical,current,by="Date")

gg.daily<-daily.index%>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(`1946`:`2025`,names_to="Year",values_to="Fish") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Fish,0))

gg.daily.quants<-gg.daily %>%
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

gg.daily.cum<-daily.index%>%
  pivot_longer(`1946`:`2025`,names_to="Year",values_to="Fish") %>%
  mutate(Fish=replace_na(Fish,0))%>%
  group_by(Year)%>%
  mutate(cum_sum=cumsum(Fish))

gg.daily.cum.quants<-gg.daily.cum %>%
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

babine.daily<-make.babine.daily.plot(gg.daily,gg.daily.quants,babine.figures.x.high,daily.yhigh)

babine.cum<-make.babine.cum.plot(gg.daily.cum,gg.daily.cum.quants,babine.figures.x.high,cum.yhigh)

ggarrange(babine.daily,babine.cum,align="v",ncol=1,common.legend = TRUE,legend="bottom")

}
#daily and cumulative index plots


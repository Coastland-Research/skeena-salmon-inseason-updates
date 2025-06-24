#create index figures with a filter for species

make.index.figures<-function(daily,current) {
  
daily.index<-left_join(daily,current,by="Date")%>%
  mutate(Date=as.Date(Date))

gg.daily<-daily.index%>%
  #mutate_if(is.character, as.numeric) %>%
  pivot_longer("1956":"2025",names_to="Year",values_to="Fish") %>%
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

years_out<-c("2008","2009","2010")

gg.daily<-gg.daily%>%filter(!Year%in%years_out)

gg.daily.cum<-daily.index%>%
  pivot_longer("1956":"2025",names_to="Year",values_to="Fish") %>%
  group_by(Year)%>%
  mutate(cum_sum=cumsum(replace_na(Fish,0)))

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

years_out<-c("2008","2009","2010")

gg.daily.cum<-gg.daily.cum%>%filter(!Year%in%years_out)

daily.index<-make.daily.index.plot(gg.daily,gg.daily.quants,0,20)
cum.index<-make.cum.index.plot(gg.daily.cum,gg.daily.cum.quants)

#Daily plot with 2025 and quants
ggarrange(daily.index,cum.index,align="v",ncol=1,common.legend = TRUE,legend="bottom")

}

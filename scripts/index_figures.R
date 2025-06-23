#create index figures with a filter for species


# Large Chinook -----------------------------------------------------------

daily<-read_excel("data/common/tyee_indices_56_2024_allspecies.xlsx", sheet = "Large Chinook")

cn.current<-read_csv("data/current_year/tyee 2025 index data.csv")%>%
  select(`month-day`,"2025"=`large chinook`)

daily.index<-left_join(daily,cn.current,by="month-day")%>%
  mutate(Date=as.Date(Date))

gg.daily<-daily.index%>%
  # mutate_if(is.character, as.numeric) %>%
  pivot_longer("1956":"2025",names_to="Year",values_to="Chinook") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Chinook,0))

gg.daily.quants<-gg.daily %>%
  group_by(Date) %>%
  summarise(per10 = quantile(Chinook,.1, na.rm = TRUE), 
            per25 = quantile(Chinook,.25, na.rm = TRUE), 
            per50 = quantile(Chinook,.50, na.rm = TRUE),
            per75 = quantile(Chinook,.75, na.rm = TRUE),
            per90 = quantile(Chinook,.9, na.rm = TRUE))%>%
  select(Date,per10,per25,per50,per75,per90)%>%
  pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
  group_by(Q)%>%
  mutate(Chinook=(Index))%>%
  mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                          Q=="per25"|Q=="per75"~"25/75th",
                          Q=="per50"~"Median"))

years_out<-c("2008","2009","2010")
gg.daily<-gg.daily%>%filter(!Year%in%years_out)

dailyplot_cn <- ggplot(gg.daily,aes(x=Date, y=Chinook,group=Date))+
  geom_line(data=gg.daily.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
  geom_line(data = gg.daily %>% filter(Year == 2025&Date<="2025-06-20"), aes(x = Date, y = Chinook, group = 1, color = "2025 Data"), linewidth = 1.5,alpha=.7)+
  geom_line(data = gg.daily %>% filter(Year <2025), aes(x = Date, y = Chinook,group=Year),
            linewidth = .5,alpha=.1)+
  scale_color_manual(values=c("grey75","purple","grey50","black"))+
  labs(y="Daily Index",color="")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylim(0,10)


# Large Sockeye -----------------------------------------------------------
daily<-read_excel("data/common/tyee_indices_56_2024_allspecies.xlsx", sheet = "Large Sockeye") %>%
  mutate(`month-day` = paste(MONTH, DAY, sep = "-")) # add month-day column


sk.current<-read_csv("data/current_year/tyee 2025 index data.csv")%>%
  select(`month-day`,"2025"=`large sockeye`)

daily.index<-left_join(daily,sk.current,by="month-day")%>%
  mutate(Date=as.Date(Date))

gg.daily<-daily.index%>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer("1956":"2025",names_to="Year",values_to="Sockeye") %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Index=replace_na(Sockeye,0))

gg.daily.quants<-gg.daily %>%
  group_by(Date) %>%
  summarise(per10 = quantile(Sockeye,.1, na.rm = TRUE), 
            per25 = quantile(Sockeye,.25, na.rm = TRUE), 
            per50 = quantile(Sockeye,.50, na.rm = TRUE),
            per75 = quantile(Sockeye,.75, na.rm = TRUE),
            per90 = quantile(Sockeye,.9, na.rm = TRUE))%>%
  select(Date,per10,per25,per50,per75,per90)%>%
  pivot_longer("per10":"per90",names_to="Q",values_to="Index")%>%
  group_by(Q)%>%
  mutate(Sockeye=(Index))%>%
  mutate(qgroup=case_when(Q=="per10"|Q=="per90"~"10/90th",
                          Q=="per25"|Q=="per75"~"25/75th",
                          Q=="per50"~"Median"))

years_out<-c("2008","2009","2010")
gg.daily<-gg.daily%>%filter(!Year%in%years_out)

dailyplot_sk <- ggplot(gg.daily,aes(x=Date, y=Sockeye,group=Date))+
  geom_line(data=gg.daily.quants,aes(colour=qgroup,group=Q),linetype="longdash",linewidth=1)+
  geom_line(data = gg.daily %>% filter(Year == 2025&Date<="2025-06-20"), aes(x = Date, y = Sockeye, group = 1, color = "2025 Data"), linewidth = 1.5,alpha=.7)+
  geom_line(data = gg.daily %>% filter(Year <2025), aes(x = Date, y = Sockeye,group=Year),
            linewidth = .5,alpha=.1)+
  scale_color_manual(values=c("grey75","purple","grey50","black"))+
  labs(y="Daily Index",color="")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylim(0,10)


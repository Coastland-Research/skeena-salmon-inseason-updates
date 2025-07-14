# read and compile First Nations demo catch information
#create a table with nation, date and total sockeye catch
#function for a figure showing all nations demo catches

demo.data<-fread("data/current_year/fns demo catches 2025.csv")

demo.totals<-demo.data%>%
  group_by(Nation=nation)%>%
  summarise("Total Demo Sockeye Catch"=sum(sockeye))

total.demo.catch=sum(demo.totals$`Total Demo Sockeye Catch`)

total.row<-tibble(Nation="Total","Total Demo Sockeye Catch"=total.demo.catch)

demo.table<-rbind(demo.totals,total.row)

plot.fns.demo.catch<-function(data) {
  
  ggplot(data,aes(x=date,y=sockeye,fill=nation))+
  geom_col()+
  labs(y="Number of Sockeye",x="Date",fill="Nation")+
  theme_bw()+
  xlim(c(as.Date("2025-07-01"),as.Date("2025-09-01")))

}

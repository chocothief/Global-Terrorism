#Load Packages
library(readr)
library(ggplot2)
library(dplyr)
library(rworldmap)
library(rworldxtra)
library(maps)
library(ggmap)
library(highcharter)


#Load Dataset
terrorism<-read.csv("C:\\Users\\SivaLalitha Chikkala\\Downloads\\Global Terrorism.csv")

#Data Cleaning
terrorism=rename(terrorism,id=eventid,year=iyear,nation=country_txt,Region=region_txt,attack=attacktype1_txt,
                 target=targtype1_txt,weapon=weaptype1_txt,Killed=nkill, wounded=nwound)

terrorism$Killed=as.integer(terrorism$Killed)
terrorism$wounded=as.integer(terrorism$wounded)

terrorism$Killed[which(is.na(terrorism$Killed))]=0
terrorism$wounded[which(is.na(terrorism$wounded))]=0


#Renaming
US<-filter(terrorism,nation =="United States")
US <- rename(terrorism, long=longitude, lat=latitude)
India<-filter(terrorism,nation=="India")
wEurope<-filter(terrorism,Region=="Western Europe")
Pakistan<-filter(terrorism,nation=="Pakistan")
SEAsia<-rbind(India,Pakistan)

countries<-filter(terrorism,nation %in% c("United States","India","Pakistan","Japan"))
countries_m<-rbind(countries,wEurope)

#Heatmap of terrorist attack deaths-2015
gtd <- read.csv("C:\\Users\\SivaLalitha Chikkala\\Downloads\\Global Terrorism.csv")
gtd2015 <- gtd[gtd$iyear==2015, ]
gtd2015 <- aggregate(nkill~country_txt,gtd2015,sum)
gtdMap <- joinCountryData2Map( gtd2015, 
                               nameJoinColumn="country_txt", 
                               joinCode="NAME" )

mapDevice('x11')
mapCountryData( gtdMap, 
                nameColumnToPlot='nkill', 
                catMethod='fixedWidth', 
                numCats=100 )

#Terrorist Attack Trends
by_year<-terrorism %>% group_by(year) %>% summarize(n=n())
ggplot(data=by_year,aes(x = year, y = n)) +
  geom_line(size = 2, alpha = 1, color = "aquamarine3") +
  geom_point(size = 1)+labs(title="Terrorist Attacks by Year",x="Year",y="Number of Terrorist Attacks")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"))

#Terrorist Attack Trends By Region n Year
by_region<-terrorism %>% group_by(Region,year) %>% summarize(n=n())
ggplot(data=by_region,aes(x=year,y=n,color=Region))+
  geom_line(size=1,alpha=1)+
  geom_point(size=1,alpha=1)+
  facet_wrap(~Region)+
  labs(title="Terrorist Attacks By Region and Year",x="Year")+
  theme_bw()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.position = "none",axis.text=element_text(size=8))


#Terrorist Attack Trends By Region
by_region_no_year<-terrorism %>% group_by(Region) %>% summarize(n=n())
ggplot(data=by_region_no_year,aes(x=reorder(Region,n),y=n))+geom_bar(stat = "identity",fill="black")+
  labs(title="Terrorist Attacks by Region",x="",y="")+
  coord_flip()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"))


#Tactics
by_attack<-terrorism %>% group_by(attack) %>% summarize(n=n())
ggplot(data=by_attack,aes(x=reorder(attack,n),y=n,fill=attack))+
  geom_bar(stat="identity")+
  labs(title="Terrorist Attack Tactics",x="",y="",fill="Tactics")+
  coord_flip()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray84"),legend.position = "bottom",legend.text = element_text(size = 8))


#Weapons
by_weapon<-terrorism %>% group_by(weapon) %>% summarize(n=n())
ggplot(data=by_weapon,aes(x=reorder(weapon,n),y=n,fill=weapon))+
  geom_bar(stat="identity")+
  labs(title="Terrorist Attack By Weapon",x="",y="",fill="Weapons")+
  coord_flip()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fil="gray84"),legend.position = "bottom")


#Target Type
attack2015<-terrorism[terrorism$year==2015,]
by_target<-attack2015 %>% group_by(target) %>% summarize(n=n())
by_target<-arrange(by_target,desc(n))
by_target
ggplot(data=by_target,aes(x=reorder(target,n),y=n,fill=target))+
  geom_bar(stat="identity")+
  labs(title="Terrorist Attack Targets/Victims, 2015",x="",y="",fill="Targeted Victims")+
  coord_flip()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray84"),legend.position = "bottom",axis.text.y = element_text(size=5),legend.text = element_text(size = 8),legend.title = element_text(size = 8))

#Casualties
terror_f=terrorism%>%
  group_by(nation,Region)%>%
  summarise(Killed=sum(Killed),wounded=sum(wounded))%>%
  mutate(casualties=Killed+wounded)%>%
  mutate(nation=ifelse(nation=="India","India",ifelse(nation=="Pakistan","Pakistan",ifelse(nation=="Iraq","Iraq",ifelse(nation=="Bangladesh","Bangladesh","")))))

posnjd <- position_jitterdodge(jitter.width = 1, dodge.width = 1)

ggplot(terror_f,aes(x=wounded,y=Killed,colour="blue", size=casualties))+
  geom_point(position = posnjd,alpha=0.6, show.legend = TRUE)+
  labs(title="Casualties of Terror Attacks",x="Killed",y="Wounded",fill="Casualties")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray84"),legend.position = "bottom")+
  geom_text(aes(label=nation,hjust=-.25, colour="red"))+
  scale_x_continuous("wounded") + 
  scale_y_continuous("Killed")

#Countries with Most Terrorist Attacks
by_country<-terrorism %>% group_by(nation) %>% summarize(n=n())
by_country<-arrange(by_country,desc(n))
top10<-head(by_country,10)
top10
ggplot(data=top10,aes(x=reorder(nation,n),y=n,fill=nation))+
  geom_bar(stat="identity")+
  labs(title="Countries with the Most Terrorist Attacks",x="Country",y="Number of Terrorist Attacks",fill="Nation")+
  coord_flip()+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.position = "bottom",legend.background = element_rect(fill="gray84"))

#Which countries/cities were the most dangerous in 2015?
attack2015_bycity<-attack2015 %>% group_by(nation,city) %>% summarise(n=n())
attack2015_bycity<-arrange(attack2015_bycity,desc(n))
top10_city_2015 <- head(attack2015_bycity, 20)
top10_city_2015


#Terrorist Attacks in Baghdad
baghdad <- terrorism[terrorism$city=='Baghdad', ]
baghdad_year <- baghdad %>% group_by(year) %>% summarise(n=n())
ggplot(data=baghdad_year,aes(x=year,y = n))+
  geom_line(size = 2, alpha = 1, color = "aquamarine3") +
  geom_point(size = 1)+
  labs(title="Terrorist Attacks in Baghdad",x="Year",y="Number of terrorist Attacks")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"))

#Attack type in Baghdad
baghdad_type <- baghdad %>% group_by(attack, year) %>% summarise(n=n())
ggplot(data=baghdad_type,aes(x=year,y=n,fill=attack))+
  geom_bar(stat="identity")+
  labs(title="Attack Type in Baghdad",x="year",y="",fill="Attack Type")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray85"),legend.position = "bottom",legend.text=element_text(size=8),legend.title = element_text(size = 8))


#Killed In India
IndiaK=filter(India,Killed!=0)
india_map<-getMap(resolution="high")
plot(india_map, xlim = c(72,75), ylim = c(8, 35), asp = 1,main = "Killed by Terror Attacks")
points(IndiaK$longitude,IndiaK$latitude,col="red",cex=.6)

#Wounded in India
IndiaW<-filter(India,wounded!=0)
india_map<-getMap(resolution="high")
plot(india_map, xlim = c(73,75), ylim = c(8, 35), asp = 1,main = "Wounded by Terror Attacks")
points(IndiaW$longitude,IndiaW$latitude,col="blue",cex=.6)


#India vs Pakistan
ggplot(SEAsia,aes(x=attack,fill=attack))+
  geom_bar(position = "dodge")+
  labs(title="India vs Pakistan",y="Number of Attacks",x="Type of Attacks",fill="Attacks")+
  facet_grid(~nation)+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.position = "bottom",legend.background = element_rect(fill="gray84"),legend.text =element_text(size = 8), legend.title = element_text(size = 8))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Attacks in Selected Countries
countries_f<-countries%>%
  group_by(nation,target)%>%
  summarise(Killed=sum(Killed),wounded=sum(wounded))%>%
  mutate(casualties = Killed+wounded)


ggplot(countries_f,aes(x=target,y=casualties,fill=target))+
  geom_bar(width = 1,stat="identity")+ 
  facet_wrap(~nation)+
  labs(title="Casualties in Attacks",y="Casualties",fill="Target")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray84"),legend.position = "bottom",legend.text = element_text(size=8))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Casualties in Selected Countries
countries_s<-countries%>%
  group_by(nation,year)%>%
  summarise(Killed=sum(Killed),wounded=sum(wounded))%>%
  mutate(casualties=Killed + wounded)


ggplot(countries_s,aes(x=year,y=casualties,group=nation))+
  geom_line(aes(color=nation),size=1)+ 
  labs(title="Casualties in Selected Countries",x="Year",y="Casualities",color="Nation")+
  theme(panel.background = element_rect(fill="gray85"),plot.title = element_text(hjust=0.5,face="bold",color="black"),legend.background = element_rect(fill="gray84"),legend.position = "bottom")
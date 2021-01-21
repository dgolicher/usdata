data("States_COVID19")

library(tidyverse)
theme_set(theme_bw())
d<-States_COVID19
d$population<-aqm::clean(d$population)
d$month<-month(d$date)
d$year<-year(d$date)
d$covid_month<-d$month+(12*(d$year-2020))

d %>% group_by(State, population, covid_month) %>%
  summarise(covid_deaths=max(deaths,na.rm=TRUE)) %>% mutate(covid_cdr=covid_deaths/(population/10000))-> cvd



library(RColorBrewer)
colors <- brewer.pal(7, "Reds")

mn=3
cvd %>% filter(covid_month==mn) -> cvd1
cvd_geo<-merge(us,cvd1)
s<-as(cvd_geo,"Spatial")
nb <- poly2nb(s, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
a<-moran.test(s$covid_cdr,lw, alternative="greater")

d<-data.frame(month=mn,moran=a$estimate[1])

for (mn in 4:13){


cvd %>% filter(covid_month==mn) -> cvd1

cvd_geo<-merge(us,cvd1)
s<-as(cvd_geo,"Spatial")
nb <- poly2nb(s, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
a<-moran.test(s$covid_cdr,lw, alternative="greater")

d1<-data.frame(month=mn,moran=a$estimate[1])
d<-rbind(d,d1)

ttl<-sprintf("Month %s Moran's I = %s", mn-3, round(a$estimate,2))

covid_map1 <- tm_shape(cvd_geo)+ tm_style_white() +
  tm_fill("covid_cdr",
          breaks = c(0,1,3,8,10,12,15,20),
          style = "fixed",
          textNA = "No data",
          colorNA = "white",
          palette = colors )+ tm_text("State", size=0.3) +
  tm_borders() + tm_legend(outside=TRUE) +tm_layout(main.title=ttl)
covid_map1

tmap_save(covid_map1, sprintf("month%02d.png",mn-3))
}

ggplot(d,aes(x=month-3,y=moran)) +geom_point() +geom_line() +
  scale_x_continuous(breaks = 0:10) ->g1
g1 + labs(title = "",
     caption = "",
     x = "Month since epidemic began in the USA",
     y = "Moran's I",
     y="") -> g1


ggsave("morans.jpg", g1)
system("convert -delay 300 -loop 0 *.png moran.gif")
system ("rm *.png")

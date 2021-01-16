#' Title
#'
#' @return
#' @export
#'
#' @examples
get_covid<- function()
{
  gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  States_COVID19<- covid19("US", gmr=gmr,level = 2)

  States_COVID19<-States_COVID19[,-c(2,3,25:30,32:41)]
  States_COVID19$State<-States_COVID19$administrative_area_level_2
  d<- States_COVID19
  d %>% group_by(administrative_area_level_2) -> d
  d %>%arrange(date) %>% mutate(New_cases = confirmed - lag(confirmed, default = first(confirmed))) -> d
  d %>% mutate(New_deaths = deaths - lag(deaths, default = first(deaths))) %>% arrange(deaths) -> d
  d %>% mutate(New_tests = tests - lag(tests, default = first(tests))) %>% arrange(tests) -> d

  States_COVID19d<-d
  save(States_COVID19,file="~/webpages/epidemiology/cdc/usdata/data/States_COVID19.rda")
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_measures<- function()
{
  gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  d<- covid19("US", gmr=gmr,level = 2)
  d %>% group_by(administrative_area_level_2) %>%
    summarise(retail=mean(retail_and_recreation_percent_change_from_baseline,na.rm=TRUE),
              transit=mean(transit_stations_percent_change_from_baseline,na.rm=TRUE),
              stringency=mean(stringency_index,na.rm=TRUE),
              schools=sum(school_closing>2,na.rm=TRUE), gatherings =sum(gatherings_restrictions>2,na.rm=TRUE),Population=min(population) ) -> measures
  names(measures)[1]<-"State"
  save(measures,file="/webpages/epidemiology/cdc/usdata/data/measures.rda")
}

make_census <- function(){
  d<- read_csv(system.file("extdata", "nst-est2019-alldata.csv", package="usdata"))

  d %>% pivot_longer(-c(1:5)) %>% separate(name,sep=5, into=c("Variable","Year")) %>%
    mutate(Year=aqm::clean(Year)) %>%
    filter(Year>2010) %>% pivot_wider(names_from="Variable", values_from="value")-> dd

  dd<-dd[,-c(1:4)]
  dd<-dd[,1:10]
  names(dd)<-c("State","Year","Population", "Change","Births","Deaths","Natural_Inc", "Inter_Migration","Dom_Migration", "Net_Migration")

  census<-dd
  save(census,file="/webpages/epidemiology/cdc/usdata/data/census.rda")
}





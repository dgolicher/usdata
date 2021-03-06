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


#' Pulls data from acs using tidycensus package
#'
#' @return
#' @export
#'
#' @examples
get_acs<-function() {
  ## Not run:
  library(tidycensus)
  library(tidyverse)
  library(viridis)
  library(sf)
  #census_api_key("00ac321d5b7acade2459e3c416a89a9e36d6b4c7",install=TRUE)

  # B19013_001 = household_income
  # B01001_001 = total population
  # B01001_025 = male over 85
  # B01001_024  = male 80 to 84
  # B01001_023  = male 75 to 79
  # B01001_022  = male 70 to 74
  # B01001_049  = female over 85
  # B01001_048  = female over 85

  over_70 <- get_acs(geography = "state", year= 2019,
                     variables = c("B01001_049", "B01001_048", "B01001_047","B01001_046","B01001_025","B01001_024","B01001_023", "B01001_022"))


  over_70 %>% group_by(NAME) %>% summarise(over_70= sum(estimate)) -> over_70

  household <- get_acs(geography = "state", year= 2019,
                       variables = c("B19013_001"))

  household %>% group_by(NAME) %>% summarise(household_income= sum(estimate)) -> household

  population <- get_acs(geography = "state", year= 2019,
                        variables = c("B01001_001"))

  population %>% group_by(NAME) %>% summarise(epopulation= sum(estimate)) -> population

  d<-merge(over_70,household)
  d<-merge(d,population)

  d$percent_over_70<-100*(d$over_70/d$epopulation)

  names(d)[1]<-"State"
  acs<-d
  save(acs, file="acs.rda")
}


#' Get deaths by age clss from cdcd
#'
#' @return
#' @export
#'
#' @examples
get_age_data<-function() {
  library(tidyverse)
  d<- read_csv("https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD")
  library(lubridate)
  d$date<-mdy(d$`End Week`)
  separate(d, `Age Group`, into =c("Start", "End")) -> d
  gsub("years", "100", d$End) -> d$End
  gsub("Under", "0", d$Start) -> d$Start

  d$End<-as.numeric(as.character(d$End))
  d$Start<-as.numeric(as.character(d$Start))
  gsub(" ", "_", names(d)) -> names(d)
  gsub("-", "_", names(d)) -> names(d)
  tolower(names(d))->names(d)
  d %>% filter(date < max(date)-20) -> d ## Take out data at the end due to long reporting delay
  d$non_covid<-d$total_deaths-d$covid_19_deaths
  d %>% filter(d$sex == "All Sex") -> d
  totals_by_age<-d
  save(totals_by_age,file="~/webpages/epidemiology/cdc/usdata/data/totals_by_age.rda")
  totals_by_age
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
yearly_totals<-function(){

  fpath <- system.file("extdata", "Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv", package="usdata")
  d <- read_csv(fpath)[,1:19]
  data(pop)
  d %>% filter(`Week Ending Date` < max(d$`Week Ending Date`)-15) -> d
  names(d)<- gsub(" ","_",names(d))
  d$Non_Natural <-d$All_Cause-d$Natural_Cause
  names(d)
  d %>% pivot_longer(cols=5:20) ->d

  d$State<-d$Jurisdiction_of_Occurrence
  d<-merge(d,pop)
  d$value_per_100k<-d$value/(d$pop/100000)

  Jurisdiction<-unique(d$Jurisdiction_of_Occurrence)
  Cause<-unique(d$name)

  d$Year<-d$MMWR_Year

  weekly_counts<-d

  save(weekly_counts,file="~/webpages/epidemiology/cdc/usdata/data/weekly_counts.rda")

  d  %>%
    group_by(Jurisdiction_of_Occurrence,name,Year) %>%
    summarise(value=sum(value, na.rm = TRUE)) -> tot

  save(tot,file="~/webpages/epidemiology/cdc/usdata/data/total_by_year.rda")

}


all_causes<-function () {
  fpath <- system.file("extdata", "Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv", package="usdata")
  d<- read_csv(fpath)[,-c(9:11)]

  names(d)<- gsub(" ","_",names(d))
  d$date<-mdy(d$Week_Ending_Date)
  d$Year<-as.factor(year(d$date))
  d$Week<-week(d$date)

  d %>% filter(date<(max(date)-20)) %>% filter(Age_Group !="Under 25 years") -> all_causes

  save(all_causes,file="~/webpages/epidemiology/cdc/usdata/data/all_causes.rda")
}



multiplot<- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



dt<-function(d) {DT::datatable(d,
                               filter = "top",
                               extensions = c('Buttons'), options = list(
                                 dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                               ))}




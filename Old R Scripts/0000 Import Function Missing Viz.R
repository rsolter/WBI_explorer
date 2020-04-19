# 0000 Import Function - Missing Viz

library(ggplot2)
library(RColorBrewer)
library(plotly)
library(tibble)

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\WDI R Data\\") 


load(file="WDI_dict.rdata")
load(file="af_WBI_60_16.rdata")

dim(WDI_dict) # - 1580 Indicators

table(WDI_dict$Subject)

# Econoimc Policy & Debt - 507 Indicators
econ <- WDI_dict %>% filter(Subject=="Economic Policy & Debt")
table(econ$WBI_Topic)
#   External Debt
#   National Accounts
#     Local currency at Constant Prices
#     US$ at Constant 2010 prices
#     All others

# Education - 151 Indicators
# Environment - 138 Indicators
# Financial Sector - 54 Indicators
# Gender - 21 Indicators
# Health - 227 Indicators
# Infrastructure - 41 Indicators 
# Poverty - 24 Indicators
# Private Sector & Trade - 173 Indicators
# Public Sector - 83 Indicators
# Social Protection & Labor - 161 Indicators


all<-af_WBI_60_16
all<-as.tibble(all)

allL <- all %>% gather(metric,value,-iso2c,-country,-year)


# plotting overall missing rate of data over time

year_m<-allL %>% 
  group_by(year) %>% 
  summarise(non_na_count = sum(!is.na(value)),
            perc_non_na = 100*sum(!is.na(value))/n()) %>% 
  arrange(year)

ggplot(year_m, aes(x=year, y=100-perc_non_na)) + 
  geom_path() + 
  theme_minimal() + 
  ylab("Percent Missing") + xlab("Year") + 
  ggtitle("Percent of Missing Data Over Time") + 
  theme(plot.title = element_text(hjust = 0.5))


# plotting interactive with plotly
library(plotly)

country_year_m<-allL %>% 
  group_by(country,year) %>% 
  summarise(non_na_count = sum(!is.na(value)),
            perc_non_na = 100*sum(!is.na(value))/n()) %>% 
  arrange(perc_non_na)

plotly::ggplotly(ggplot(country_year_m, aes(x=year, y=100-perc_non_na, colour=country)) + 
                   geom_line() + 
                   theme_minimal() + 
                   ylab("Percent Missing") + xlab("Year") + 
                   ggtitle("Percent of Missing Data Over Time") + 
                   theme(plot.title = element_text(hjust = 0.5)))




metric_mc<-allL %>% 
  group_by(metric,country) %>% 
  summarise(non_na_count = sum(!is.na(value)),
            perc_non_na = 100*sum(!is.na(value))/n(),
            perc_missing = 100-100*sum(!is.na(value))/n()) %>% 
  arrange(-perc_non_na)

metric_mc<-left_join(metric_mc,WDI_dict,by=c("metric"="WBI"))

compl_metrics <- metric_mc %>% filter(non_na_count==27)

subjects <- WDI_dict %>% group_by(Subject) %>% tally() %>% select(Subject)
subject_counts <-WDI_dict %>% group_by(Subject) %>% tally()
compl_subject_counts <- metric_mc %>% group_by(Subject) %>% summarise(Total=n(),
                                                                      non_na_count=sum(!is.na(value))/n())

# 'metric_mc' records the number of observations that were present/absent per metric, per country
# max is 27 for non_na_count 1 (for 27 years of recorded data)


names(metric_mc) <- c("metric",
                      "Country",
                      "non_na_count",
                      "perc_non_na",
                      "Percent Missing",
                      "Subject",
                      "WBI_topic", 
                      "WBI_name")

plot_list <- list()

for (i in subjects$Subject){
  tmp_data <- metric_mc[ which(metric_mc$Subject==i), ]
  plot_list[[i]]<-ggplot(tmp_data,
                         aes(x=WBI_name, y=country)) + geom_tile(aes(fill=perc_missing)) +
    scale_x_discrete(limits=unique(tmp_data$WBI_name[order(tmp_data$perc_missing)])) +
    scale_y_discrete(limits=unique(tmp_data$country[order(tmp_data$perc_missing)])) +
    ggtitle(paste("Annual ", i, " metrics by Country for 1960-2016",sep="")) +
    theme(axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("") +
    scale_fill_continuous(name = "Percent Missing")
}



plot_list[[1]]

fin_sec<-plot_list[[4]]
#fin_sec <- left_join(fin_sec,WDI_dict)
  
ggplotly(fin_sec)

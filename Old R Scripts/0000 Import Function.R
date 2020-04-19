# 0000 Import WDI function
library(WDI)
library(stringr)

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\WDI Extract\\") 
WDI_dict <-read.csv("WDISeries.csv",header = T, stringsAsFactors = F)
WDI_dict<- WDI_dict[, c(1,2,3)]
names(WDI_dict) <- c("WBI","WBI_Topic","WBI_name")
WDI_dict$Subject <- str_extract(WDI_dict$WBI_Topic, "[^:]+")


WDI_dict <- WDI_dict[c(4,2,3,1)]


save(WDI_dict,file="WDI_dict.rdata")

# Let's try and pull them all for giggles


# for Nigeria, it takes 0.4 mB and roughly 6.5 minutes to pull
# That would mean 25 mb for all of africa, but roughly 5 and a half hours..

# all of the african countries for two years was 1.4 mB and took 8 minutes to pull
start<-proc.time()
test_all <- WDI::WDI(country = af_country_list, 
                     indicator = WDI_dict$WBI,start = 1960, end = 2016)
format(utils::object.size(test_all),units = "Mb")
proc.time()-start



test_tibble<-as.tibble(test_all)
af_WBI_60_16<-test_tibble

save(af_WBI_60_16,file="af_WBI_60_16.rdata")

load(file="C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\WDI R Data\\af_WBI_60_16.rdata")







## START HERE

### Checking for sparsity, completeness


load(file="af_WBI_90_16.rdata")

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





metric_m<-allL %>% 
  group_by(metric) %>% 
  summarise(non_na_count = sum(!is.na(value)),
            perc_non_na = 100*sum(!is.na(value))/n()) %>% 
  arrange(perc_non_na)




# Can be missing by country/metric or country/year or country




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




library(ggplot2)
library(RColorBrewer)

plot_list <- list()

for (i in subjects$Subject){
  tmp_data <- metric_mc[ which(metric_mc$Subject==i), ]
  plot_list[[i]]<-ggplot(tmp_data,
         aes(x=metric, y=country)) + geom_tile(aes(fill=perc_missing)) +
    scale_x_discrete(limits=unique(tmp_data$metric[order(tmp_data$perc_missing)])) +
    scale_y_discrete(limits=unique(tmp_data$country[order(tmp_data$perc_missing)])) +
    ggtitle(paste(i, " metrics by Country",sep="")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("")
}

### Notes 

# 1 - Economic Policy & Debt (507)

# 2 - Education (151)


# 3 - Environment (138)

# 4 - Financial Sector (54) 


# 5 - Gender (21)


# 6 - Health (227)

# 7 - Infrastructure (41)

# 8 - Poverty (24)

# 9 - Private Sector & Trade (173)

# 10 - Public Sector (83)

# 11 - Social Protection & Labor (161)



for (i in subjects$Subject){
  plot_list[[i]]<-ggplot(data=metric_mc[ which(metric_mc$Subject==i), ],
                         aes(x=metric, y=country)) + geom_tile(aes(fill=perc_missing)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(i)
}






WDI_dict_Gender <- WDI_dict

metric_mc %>% 
  filter(Subject=="Health") %>% 
  group_by(country) %>% 
  select(non_na_count) %>% 
  unique() %>% arrange(-non_na_count)

#geom_line(data=df[ which(df$measure=="RAW"), ]





# none of the rows are complete.. great
table(complete.cases(af_WBI_90_16))
# none are logical (all NAs)
table(apply(af_WBI_90_16, 2, is.logical))


## Hypothesis is that there are certain countries, years for which there is more sparsity of data

# look at questions first
af_wbi_l <- af_WBI_90_16 %>% gather(metric, value, -iso2c, -country, -year)

af_wbi_l <- af_wbi_l %>% group_by(metric) %>% nest()

sum_cc <= functioN(i){
  sum(complete.cases(af_wbi_l$data[[i]]))  
}

af_wbi_l$cc <- lapply(af_wbi_l, sum_cc)




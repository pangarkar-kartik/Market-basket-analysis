library(arules)
library(arulesViz)
library(magrittr)
library(RColorBrewer)

# first converting each record into transaction with read.transaction 

market_basket<-read.transactions(
  file ='C:/Users/Kartik/Downloads/market_basket.csv',
  sep =',',
  quote ="",
  format ='basket',
  rm.duplicates =TRUE,
  skip=1
)

summary(market_basket)

#total no of items purchased 
18440 * 22346 * 0.0009915565
transactions * items * density 

# first 5 transactions 
market_basket %>% head(n=5) %>% inspect

# top 10 items brought
itemFrequencyPlot(x = market_basket,
  topN = 10,
  # support = 
  type = 'absolute',
  horiz = TRUE,
  col = brewer.pal(10,'Spectral')
)

# Association rules 
# building apriori algorithm with support value= 0.005 and confidence value =0.8

rule1 <- market_basket  %>%
    apriori(parameter = list(supp = 0.005,conf = 0.8 ))  %>%
    sort(by = 'confidence')

summary(rule1)

# SUPPORT = 0.005 i.e out of all trans selecting those item sets which occurs at least .5% times 

# CONFIDENCE = all of the items which we have selected the consequence should be present at least 80 % times 

# top 5 rules = the confidence interval 1 i.e the combination items are bought 100 % the time .
rule1 %>% head(n=5) %>% inspect
# low 5 rules = the confindence interval is 0.8 where the combination items are bought at least 80 % of the time .
rule1 %>% head(n=5) %>% inspect    
    
rule1 <- rule1 %>% sort(by = "lift")
rule1 %>% head(n=5) %>% inspect
rule1 %>% head(n=5) %>% inspect  

# Plotting rule1
plot(rule1,engine = "htmlwidget")
plot(rule1,method = "two-key",engine = "htmlwidget")
plot(rule1,method = "graph",engine = "htmlwidget")

#Rule2

rule2 <- market_basket  %>%
  apriori(parameter = list(supp = 0.009,conf = 0.3 ))  %>%
  sort(by = 'confidence')

summary(rule2)

# top 5 rules 
rule2 %>% head(n=5) %>% inspect
rule2 %>% tail(n=5) %>% inspect

#Plotting rule2
plot(rule2,engine = "htmlwidget")
plot(rule2,method = "two-key",engine = "htmlwidget")
plot(rule2,method = "graph",engine = "htmlwidget")

#rule3

rule3 <- market_basket  %>%
  apriori(parameter = list(supp = 0.02,conf = 0.5 ))  %>%
  sort(by = 'confidence')

summary(rule3)

# top 5 rules 
rule3 %>% head(n=5) %>% inspect
rule3 %>% tail(n=5) %>% inspect

#Plotting rule3
plot(rule3,engine = "htmlwidget")
plot(rule3,method = "two-key",engine = "htmlwidget")
plot(rule3,method = "graph",engine = "htmlwidget")



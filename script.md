GyanMatrix HackerEarth solution
================
Sharad
October 11, 2016

Loading data

``` r
train= read.csv("ign.csv", header = T, na.strings = c("", " ", NA))
str(train)
```

    ## 'data.frame':    18625 obs. of  11 variables:
    ##  $ X             : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ score_phrase  : Factor w/ 11 levels "Amazing","Awful",..: 1 1 6 6 6 5 2 1 2 5 ...
    ##  $ title         : Factor w/ 12589 levels "'Splosion Man",..: 5702 5703 9767 7249 7249 11406 2908 4446 2908 11406 ...
    ##  $ url           : Factor w/ 18577 levels "/games/0-d-beat-drop/xbox-360-14342395",..: 8390 8387 14319 10813 10812 16931 4271 6526 4270 16932 ...
    ##  $ platform      : Factor w/ 59 levels "Android","Arcade",..: 39 39 15 58 36 20 58 33 36 33 ...
    ##  $ score         : num  9 9 8.5 8.5 8.5 7 3 9 3 7 ...
    ##  $ genre         : Factor w/ 112 levels "Action","Action, Adventure",..: 64 64 69 94 94 105 38 82 38 105 ...
    ##  $ editors_choice: Factor w/ 2 levels "N","Y": 2 2 1 1 1 1 1 2 1 1 ...
    ##  $ release_year  : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
    ##  $ release_month : int  9 9 9 9 9 9 9 9 9 9 ...
    ##  $ release_day   : int  12 12 12 11 11 11 11 11 11 11 ...

1. List of platforms with the most "Editor's choise"
----------------------------------------------------

``` r
library(ggplot2)
ggplot(train[train$editors_choice =="Y",], aes(as.factor(platform), fill="red")) + 
  stat_count(width = 0.5)+scale_y_continuous(breaks = seq(0,700,100))+ coord_flip()+
  ggtitle("Platforms with Awards Count")+ ylab("Platforms")
```

![](script_files/figure-markdown_github/unnamed-chunk-2-1.png)

from graph 100 seems like a good cut off

``` r
count=as.data.frame(as.matrix(table(train$platform, train$editors_choice=="Y")))
count= subset(count, count$Freq >100)
as.matrix(count$Var1[22:31])
```

    ##       [,1]           
    ##  [1,] "GameCube"     
    ##  [2,] "iPhone"       
    ##  [3,] "PC"           
    ##  [4,] "PlayStation"  
    ##  [5,] "PlayStation 2"
    ##  [6,] "PlayStation 3"
    ##  [7,] "Wii"          
    ##  [8,] "Wireless"     
    ##  [9,] "Xbox"         
    ## [10,] "Xbox 360"

2.Effect of platform in a given year on award
---------------------------------------------

``` r
library(data.table)
df = as.data.table(train)
convert =df[, .(title= title), by= .(release_year ,platform, editors_choice)]

ggplot(convert[convert$platform== "PC",], aes(as.factor(release_year), fill=editors_choice))+
  stat_count(width = 0.5)+ facet_wrap(~editors_choice, ncol = 1)+
  theme(axis.title.x= element_blank(), axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle("Number of games by PC for all years")
```

![](script_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(convert[convert$platform== "PlayStation",], aes(as.factor(release_year), fill=editors_choice))+
  stat_count(width = 0.5)+ facet_wrap(~editors_choice, ncol = 1)+
  theme(axis.title.x= element_blank(), axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle("Number of games by PlayStation for all years")
```

![](script_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
ggplot(convert[convert$platform== "Xbox 360",], aes(as.factor(release_year), fill=editors_choice))+
  stat_count(width = 0.5)+ facet_wrap(~editors_choice, ncol = 1)+
  theme(axis.title.x= element_blank(), axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle("Number of games by Xbox 360 for all years")
```

![](script_files/figure-markdown_github/unnamed-chunk-4-3.png) so for very few platform number of games have effect on award , in general they don't

3. Average Award count for Macintosh
------------------------------------

``` r
prop.table(table(train[train$platform == "Macintosh",8]))
```

    ## 
    ##         N         Y 
    ## 0.5061728 0.4938272

4. Optimal month for releasing game
-----------------------------------

``` r
train$release_month= factor(train$release_month, labels  = c("January", "February", "March", "April",
                                                            "May","June", "July", "August", "September",
                                                            "October", "November", "December"))


ggplot(train, aes(as.factor(release_month), fill= release_month)) + stat_count(width = 0.5) + facet_grid(~editors_choice)+
  theme(axis.title.x= element_blank(), axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle("Number of awards in each month")
```

![](script_files/figure-markdown_github/unnamed-chunk-6-1.png)

From above graph, November is the optimal month

5. Model
--------

We are going to use randomForest here for modelling but data has some missing values which must be imputed first.

We will impute these values with mode value in genre i.e. Action And divide trainingdata and testdata then, We can remove column url because it does not give us any additional information

``` r
train$genre[is.na(train$genre)]= "Action"

train$X=NULL
train$url=NULL
trainingdata= subset(train, release_year <2009)
testdata= subset(train, release_year >2008)
```

Finding out important variables for prediction via graphs

``` r
ggplot(train, aes(x= editors_choice, y= score, fill= editors_choice)) + geom_boxplot()
```

![](script_files/figure-markdown_github/unnamed-chunk-8-1.png)

this shows that titles with around 8 score got more awards

``` r
ggplot(train, aes(x=score_phrase, fill=editors_choice)) + stat_count(width = 0.5) +facet_grid(~editors_choice)+
  theme(axis.title.x= element_blank(), axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle("Effect of score_phrase on award")
```

![](script_files/figure-markdown_github/unnamed-chunk-9-1.png)

this shows titles with Amazing ,Great score phrase got more awards

### Now we will use randomForest

``` r
library(h2o)
```

    ## Loading required package: statmod

    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## Your next step is to start H2O:
    ##     > h2o.init()
    ## 
    ## For H2O package documentation, ask for help:
    ##     > ??h2o
    ## 
    ## After starting H2O, you can use the Web UI at http://localhost:54321
    ## For more information visit http://docs.h2o.ai
    ## 
    ## ----------------------------------------------------------------------

    ## 
    ## Attaching package: 'h2o'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, month, week, year

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cor, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %*%, %in%, &&, ||, apply, as.factor, as.numeric, colnames,
    ##     colnames<-, ifelse, is.character, is.factor, is.numeric, log,
    ##     log10, log1p, log2, round, signif, trunc

``` r
h2o.init()
```

    ##  Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         1 minutes 34 seconds 
    ##     H2O cluster version:        3.10.0.6 
    ##     H2O cluster version age:    1 month and 16 days  
    ##     H2O cluster name:           H2O_started_from_R_sharad_eat852 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   0.84 GB 
    ##     H2O cluster total cores:    4 
    ##     H2O cluster allowed cores:  2 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     R Version:                  R version 3.3.1 (2016-06-21)

``` r
train.h2o= as.h2o(trainingdata)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

``` r
test.h2o= as.h2o(testdata)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

``` r
colnames(train.h2o)
```

    ## [1] "score_phrase"   "title"          "platform"       "score"         
    ## [5] "genre"          "editors_choice" "release_year"   "release_month" 
    ## [9] "release_day"

``` r
y.dep= 6
x.dep= c(1:5, 7:9)
  rforest.model <- h2o.randomForest(y=y.dep, x=x.dep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=                                                                |   1%
      |                                                                       
      |=                                                                |   2%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |==========                                                       |  15%
      |                                                                       
      |===============                                                  |  23%
      |                                                                       
      |==================                                               |  27%
      |                                                                       
      |===================                                              |  30%
      |                                                                       
      |=====================                                            |  33%
      |                                                                       
      |=========================                                        |  38%
      |                                                                       
      |============================                                     |  44%
      |                                                                       
      |=================================                                |  50%
      |                                                                       
      |=====================================                            |  57%
      |                                                                       
      |===========================================                      |  65%
      |                                                                       
      |=================================================                |  75%
      |                                                                       
      |=======================================================          |  85%
      |                                                                       
      |==============================================================   |  95%
      |                                                                       
      |=================================================================| 100%

``` r
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

``` r
table(testdata$editors_choice, predict.rforest$predict)
```

    ##    
    ##        N    Y
    ##   N 4771  562
    ##   Y   61 1306

``` r
(4771+1306)/nrow(testdata)
```

    ## [1] 0.9070149

This model gives .90 accuracy

6. Important Variables for prediction
-------------------------------------

``` r
as.matrix(h2o.varimp(rforest.model)[1:6,1])
```

    ##      [,1]           
    ## [1,] "score"        
    ## [2,] "score_phrase" 
    ## [3,] "platform"     
    ## [4,] "genre"        
    ## [5,] "release_year" 
    ## [6,] "release_month"

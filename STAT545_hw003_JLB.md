STAT545\_hw03\_JLB
================

``` r
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(gapminder))
```

\*\* Task 1: Get the maximum and minimum of GDP per capita for all continents. \*\*

``` r
gapminder %>%
  group_by(continent) %>% 
  summarize(minGdp = min(gdpPercap),
            maxGdp = max(gdpPercap)) %>% 
  knitr::kable()
```

| continent |      minGdp|     maxGdp|
|:----------|-----------:|----------:|
| Africa    |    241.1659|   21951.21|
| Americas  |   1201.6372|   42951.65|
| Asia      |    331.0000|  113523.13|
| Europe    |    973.5332|   49357.19|
| Oceania   |  10039.5956|   34435.37|

\*\* Task 2 Look at the spread of GDP per capita within the continents. \*\*

\*\* Task 3 Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean. \*\*

``` r
#find the 5% trimmed mean of life expectancy. 
gapminder %>%
  group_by(year) %>%
  summarize(mean_lifeExp = mean(lifeExp, trim = .15)) %>% 
  knitr::kable()
```

|  year|  mean\_lifeExp|
|-----:|--------------:|
|  1952|       48.19613|
|  1957|       51.01132|
|  1962|       53.40731|
|  1967|       55.80129|
|  1972|       58.08110|
|  1977|       60.25699|
|  1982|       62.32432|
|  1987|       64.24004|
|  1992|       65.55227|
|  1997|       66.45447|
|  2002|       67.25753|
|  2007|       68.63648|

\*\* Task 4 How is life expectancy changing over time on different continents? \*\*

``` r
lag(c(1,2,3,4))
```

    ## [1] NA  1  2  3

``` r
#gapminder %>% 
 #group_by(continent) %>% 
 # mutate(gainLifeexp = lifeExp-lag(lifeExp)) %>% 
 # summarize(meanbycontinent = mean(gainLifeexp))
  #mutate(changeLEbyContinent = mean(gainLifeexp))
  
  
gapminder %>% 
 group_by(continent) %>% 
  mutate(meanLifeexp = mean(lifeExp))
```

    ## # A tibble: 1,704 x 7
    ## # Groups: continent [5]
    ##    country     continent  year lifeExp      pop gdpPercap meanLifeexp
    ##    <fctr>      <fctr>    <int>   <dbl>    <int>     <dbl>       <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333       779        60.1
    ##  2 Afghanistan Asia       1957    30.3  9240934       821        60.1
    ##  3 Afghanistan Asia       1962    32.0 10267083       853        60.1
    ##  4 Afghanistan Asia       1967    34.0 11537966       836        60.1
    ##  5 Afghanistan Asia       1972    36.1 13079460       740        60.1
    ##  6 Afghanistan Asia       1977    38.4 14880372       786        60.1
    ##  7 Afghanistan Asia       1982    39.9 12881816       978        60.1
    ##  8 Afghanistan Asia       1987    40.8 13867957       852        60.1
    ##  9 Afghanistan Asia       1992    41.7 16317921       649        60.1
    ## 10 Afghanistan Asia       1997    41.8 22227415       635        60.1
    ## # ... with 1,694 more rows

``` r
  #summarize(meanbycontinent = mean(gainLifeexp))
  #mutate(changeLEbyContinent = mean(gainLifeexp))
  
  
#mutate(change_lifeExp =lifeExp - lag(lifeExp))) 
```

\*\* Task 5 Report the absolute and/or relative abundance of countries with low life expectancy over time by continent: Compute some measure of worldwide life expectancy – you decide – a mean or median or some other quantile or perhaps your current age. Then determine how many countries on each continent have a life expectancy less than this benchmark, for each year. \*\*

\*\* Task 6 Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class. \*\*

STAT545\_hw03\_JLB
================

#### Jasmine's Homework 3 Assignment

##### Load Packages

Note: In this homework, I installed another library called gridExtra.
To dowload this library, you will need to run install.packages("gridExtra") in the console.

``` r
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(gridExtra))
```

In this homework, I tackle tasks 1-5.
I struggled with Task \#3, but showed my efforts for feedback. Please consider Tasks 1,2,4,5 for grading.
As part of the additional challenge, I tried my best to get plots and tables together:
I did not find that the link provided in the assignment instructions on how to do this was particularly useful. Instead, I found a [tutorial](https://magesblog.com/post/2015-04-14-plotting-tables-alsongside-charts-in-r/) online using the [gridExtra](https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html) package, which allows you to arrange multiple items (plots, text, tables) on a page.

**Task 1: Get the maximum and minimum of GDP per capita for all continents.**

``` r
max_minGDP = gapminder %>% #name a variable
  group_by(continent) %>% 
  summarize(min = round(min(gdpPercap),0), #find the max and min GDPs for each continent.
            max = round(max(gdpPercap),0))  #rounding to 0 decimal points (ie to the nearest dollar. )
  
tbl_maxminGDP = tableGrob(max_minGDP)  #use tableGrob from gridExtra package to make a nice looking table from the max_minGDP dataframe.

plot_maxminGDP = gapminder %>% #making a plot
  group_by(continent) %>% #need to regroup by continent here
  filter(gdpPercap == min(gdpPercap) | gdpPercap == max(gdpPercap)) %>% #filter and keep only those values with the max and min GDP per capita for each continent
  ggplot(aes(continent, gdpPercap)) + geom_point(aes(colour = gdpPercap)) + #add points
  scale_y_log10() + #convert gdpPercap to a log scale 
  theme_classic() + #adjust the look of the graph
  xlab("Continent") + #add axis titles
  ylab("GDP Per Capita")
  theme(plot.margin = unit(c(0, 2, 2, 0), "cm")) #add margins to the plot, so that when we use grid.arange the plot and table do not overlap. 
```

    ## List of 1
    ##  $ plot.margin:Class 'unit'  atomic [1:4] 0 2 2 0
    ##   .. ..- attr(*, "valid.unit")= int 1
    ##   .. ..- attr(*, "unit")= chr "cm"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

``` r
grid.arrange(top="Maximum and Minimum Historic GDP Per Capita by Continent",plot_maxminGDP, tbl_maxminGDP, #using grid.arrange to have the plot and table on the same graph.
             ncol=2,
             widths = c(40,25), #adjust widths of plot and table respectively
             as.table=TRUE,
             heights=c(25,5)) #adjust heights
```

![](STAT545_hw003_JLB_files/figure-markdown_github/max%20min%20GDP-1.png)

Interesting findings:
- Asia has the largest historic spread of GDP per capita out of all the continents. However, this data would be more meaningful if we were looking at a specific year.
- Oceania's two countries have the smallest historic spread in GDP per capita.

In order to get more relevant data, I filtered out the previous to select only data from the year 2007:

**Task 1 Continued. **

``` r
max_minGDP = gapminder %>% #name a variable
  filter(year =="2007") %>% # add a filter for a year.
  group_by(continent) %>% 
  summarize(min = round(min(gdpPercap),0), #find the max and min GDPs for each continent.
            max = round(max(gdpPercap),0))  #rounding to 0 decimal points (ie to the nearest dollar. )

  
tbl_maxminGDP = tableGrob(max_minGDP)  #use tableGrob from gridExtra package to make a nice looking table from the max_minGDP dataframe.

plot_maxminGDP = gapminder %>% #making a plot
  filter(year =="2007") %>% 
  group_by(continent) %>% #need to regroup by continent here
  filter(gdpPercap == min(gdpPercap) | gdpPercap == max(gdpPercap)) %>% #filter and keep only those values with the max and min GDP per capita for each continent
  ggplot(aes(continent, gdpPercap)) + geom_point(aes(colour = gdpPercap)) + #add points
  scale_y_log10() + #convert gdpPercap to a log scale 
  theme_classic() + #adjust the look of the graph
  xlab("Continent")+ #add labels to axes
  ylab("GDP Per Capita")
  theme(plot.margin = unit(c(0, 2, 2, 0), "cm")) #need to add margins to the plot, so that when we use grid.arange the plot and table do not overlap. 
```

    ## List of 1
    ##  $ plot.margin:Class 'unit'  atomic [1:4] 0 2 2 0
    ##   .. ..- attr(*, "valid.unit")= int 1
    ##   .. ..- attr(*, "unit")= chr "cm"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

``` r
grid.arrange(top="Maximum and Minimum Historic GDP Per Capita by Continent in 2007",plot_maxminGDP, tbl_maxminGDP, #using grid.arrange to have the plot and table on the same graph.
             ncol=2,
             widths = c(40,25), #adjust widths of plot and table respectively
             as.table=TRUE,
             heights=c(25,5)) #adjust heights
```

![](STAT545_hw003_JLB_files/figure-markdown_github/max%20min%20gdp%202007-1.png)

Conclusion: In 2007, we still see a large gap between the min and max GDP in Asia, however, a similar gap is also observed in Africa and in the Americas. The gap in Europe is much smaller.

**Task 2 Look at the spread of GDP per capita within the continents in 2007. **

The table shows data on the variance (var) and mean GDP per capita within the continents in the year 2007.
The first plot is a violin plot showing the distribution of GDP per capita by continent in 2007.
The second plot shows a histogram of the number of countries with various GDP per capita by continent.

``` r
task2= gapminder %>% #name a variable for task 2
  filter(year =="2007") %>% 
  group_by(continent) %>% 
  mutate(varGdp = var(gdpPercap),
         meanGdp = mean(gdpPercap))

tabletask2 = task2 %>% 
  select(continent, varGdp, meanGdp) %>% 
  unique() %>% #filter only unique rows
  knitr::kable()
tabletask2 #print table
```

| continent |     varGdp|    meanGdp|
|:----------|----------:|----------:|
| Asia      |  200362251|  12473.027|
| Europe    |  139248020|  25054.482|
| Africa    |   13091107|   3089.033|
| Americas  |   94346435|  11003.032|
| Oceania   |   42784565|  29810.188|

``` r
violin_task2 = task2 %>% #call task 2 and make a plot from it
  ggplot(aes(continent, gdpPercap)) + 
  geom_violin(fill = "#FF3366") + 
  geom_jitter(alpha = 0.3) + 
  scale_y_log10() + #log 10 scale for y axis 
  ylab("GDP Per Capita") + #adding axis labels
  xlab("Continent")
  

histogram_task2 = task2 %>% #call task 2 and make a histogram plot
  ggplot(aes(gdpPercap)) + 
  geom_histogram(bins=15, fill = "#FF3366", colour = "black") + 
  facet_wrap(~continent) + 
  scale_x_log10() + #put GDP per cap on log scale
  ylab("Count") + #add labels for axes
  xlab("GDP per Capita")
  

#create a grid to display graphs side by side.
grid.arrange(top = "Spread of GDP per Capita by Continent in 2007", violin_task2, histogram_task2, 
             ncol=2,
             widths = c(50,50),
             as.table=TRUE,
             heights=c(20,5))
```

![](STAT545_hw003_JLB_files/figure-markdown_github/Task2:%20Spread%20GDP-1.png)

Conclusion:
There are some interesting points to be taken away:
- In Africa, and Asia, there seem to be two groups of countries, one group with higher GDP, and one group with lower GDP.
- in the Americas, and Oceania (which only has two countries), there seems to be one main group, with some outliers on either end of the Americas.
- Europe shows a shift towards higher GDPs in all countries, though there is a group of countries with lower GDP.

The histogram is helpful for quantifying the number of countries with a particular GDP. For example, Europe has over 13 countries with a GDP over 10000.

**Task 3 Compute a trimmed mean of life expectancy for different years.**

I will find the 30% trimmed mean life expectancy for different years.

``` r
#normally I would remove redundancy between the data going into table and plot, but for clarity, I showed the entire process as I was having difficulty:

trimmedLE_tbl = gapminder %>% #make a table of the data using tableGrob function.
  group_by(year) %>%
  summarize(trim_meanLE = round(mean(lifeExp, trim = .3),1)) %>% 
  tableGrob() 
  
trimmedLE_plot = gapminder %>%  #make a plot of the data using ggplot.
  group_by(year) %>%
  #arrange(lifeExp) %>% 
  summarize(mean_lifeExptrim = round(mean(lifeExp, trim = .3),1),#trim data by .2 (20% of highest and lowest values will be "trimmed")
            lifeExpavg = mean(lifeExp)) %>% 
  ggplot(aes(year)) +
  geom_point(aes(y = mean_lifeExptrim), colour = "blue") +
  geom_point(aes(y = lifeExpavg), colour = "red") +theme_classic()

grid.arrange(top = "Global Mean Trimmed Life Expectancy Over Time",trimmedLE_plot, trimmedLE_tbl, #arrange plot and table together
             ncol=2,
             widths=c(12,5),
             as.table=TRUE,
             heights=c(14,3))
```

![](STAT545_hw003_JLB_files/figure-markdown_github/Trimmed%20LE-1.png)

In red is the global average life expectancy, untrimmed.
In blue is the global 30% trimmed average life expectancy.

Conclusion:
- I was unsure if the trim() function was trimming data in order of high to low years, or by life expectancy. If I added arrange(lifeExp) the values stay the same, suggesting that this is not the case.
- It appears from our data that the change in mean life expectancy is more affected by countries with the extremes (high or low) values in years starting at 1992.

**Task 4 How is life expectancy changing over time on different continents? **

The first plot shows the overall trend of average life expectancy over time per continent.
The second plot shows the **absolute change** in life expectancy year over year per continent

``` r
LE_over_time_plot1 = gapminder %>% 
  group_by(continent, year) %>% 
  mutate(meanLE = mean(lifeExp)) %>% #compute mean life life expectancy per continent for that year.
  ggplot(aes(year, meanLE)) + #call ggplot
  geom_point( aes(shape= continent), na.rm = TRUE) + #add data points
  geom_line(aes(colour = continent), na.rm = TRUE) + #add line through data
  ggtitle("Mean Life Expectancy ") +
  ylab("Mean Life Expectancy (Years)") +
  xlab("Year") +
  theme_classic()


LE_over_time_plot2 = gapminder %>% 
 group_by(continent,year) %>% 
  mutate(meanLE = mean(lifeExp)) %>% #take the mean LE of that continent in that year. 
  group_by(country) %>% #re-group only by country because otherwise the lag( ) function does not work as we want.
  mutate(changeLE = meanLE - lag(meanLE)) %>%  #take the difference in mean LE - this is done over time, as the data is organized as such.
  ggplot(aes(year, changeLE, ymin = -1)) + #call ggplot
  geom_point(na.rm = TRUE) + #add the data points
  geom_line(aes(colour = continent), na.rm = TRUE) + #add connecting lines
  facet_wrap(~continent) + 
  geom_line(y=0) +
  theme_classic() +
  ggtitle("Change in Life Expectancy by Year") +
  ylab("Change in Life Expectancy (Years)") +
  xlab("Year")


  grid.arrange(LE_over_time_plot1, LE_over_time_plot2, #use grid arrange the two plots together.
             ncol=2,
             widths=c(30,40),
             as.table=TRUE,
             heights=c(17,8))
```

![](STAT545_hw003_JLB_files/figure-markdown_github/Change%20in%20Life%20Expectancy%20over%20Time-1.png)

Conclusion:
- Across continents, all countries have seen an overall increase in life expectancy over time.
- Plotting absolute change in life expectancy over time gives an idea of the rate of change over time. For example, while life expectancy in the Americas has increased year over year, the rate at which it is doing so is decreasing.
- Meanwhile, in Europe and Oceania the rate of increase in life expectancy has stayed relatively constant around +1 year since 1962.
- The only continent that has seen a decrease in life expectancy year over year in this data set is Africa, where there was a decrease in average life expectancy in the year 2002.

**Task 5 Report the absolute and/or relative abundance of countries with low life expectancy over time by continent:**

I chose to compute the median worldwide life expectancy per year. I want to see how many countries on each continent have a life expectancy less than the median life expectancy (medLE) for each year.

``` r
#medLE is the median life expectancy worldwide for each year.
medLE = gapminder %>%
  group_by(year) %>% 
  summarize(medLE = median(lifeExp))

numBelow_medLE = gapminder %>% 
  group_by(year) %>% 
  mutate(medLE = median(lifeExp)) %>% 
  filter(lifeExp < medLE) %>% 
  group_by(continent, year) %>% 
  tally() %>% 
  knitr::kable()
 
rel_abundance_lowLEplot = gapminder %>% 
  group_by(year) %>% 
  mutate(medLE = median(lifeExp)) %>% 
  #filter(lifeExp < medLE) %>% 
  ggplot(aes(year, lifeExp)) + geom_jitter(aes(colour = lifeExp < medLE),alpha = 0.33) + facet_grid(continent ~.) +
  ggtitle("Life Expectancy Above and Below Worldwide Median") +
  ylab("Life Expectancy") +
  xlab("Year")
  
#print the plot and the table:
rel_abundance_lowLEplot
```

![](STAT545_hw003_JLB_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
numBelow_medLE
```

| continent |  year|    n|
|:----------|-----:|----:|
| Africa    |  1952|   47|
| Africa    |  1957|   47|
| Africa    |  1962|   47|
| Africa    |  1967|   48|
| Africa    |  1972|   50|
| Africa    |  1977|   49|
| Africa    |  1982|   49|
| Africa    |  1987|   48|
| Africa    |  1992|   47|
| Africa    |  1997|   48|
| Africa    |  2002|   47|
| Africa    |  2007|   47|
| Americas  |  1952|    6|
| Americas  |  1957|    6|
| Americas  |  1962|    6|
| Americas  |  1967|    6|
| Americas  |  1972|    6|
| Americas  |  1977|    7|
| Americas  |  1982|    7|
| Americas  |  1987|    8|
| Americas  |  1992|    8|
| Americas  |  1997|    7|
| Americas  |  2002|    8|
| Americas  |  2007|    8|
| Asia      |  1952|   17|
| Asia      |  1957|   17|
| Asia      |  1962|   18|
| Asia      |  1967|   17|
| Asia      |  1972|   15|
| Asia      |  1977|   14|
| Asia      |  1982|   14|
| Asia      |  1987|   14|
| Asia      |  1992|   15|
| Asia      |  1997|   15|
| Asia      |  2002|   16|
| Asia      |  2007|   15|
| Europe    |  1952|    1|
| Europe    |  1957|    1|
| Europe    |  1977|    1|
| Europe    |  1982|    1|
| Europe    |  1987|    1|
| Europe    |  1992|    1|
| Europe    |  1997|    1|
| Europe    |  2007|    1|

Conclusion:
- In blue are countries where the life expectancy is below the worldwide median for that year.
- Interestingly, over time, most countries in Europe and all in Oceania have an average life expectancy higher than the worldwide median.
- In contrast, most countries in Africa fall below the median life expectancy, and interestingly, the spread of life expectancy appears to have spread out over time.
- In America, the spread of life expectancy decreases, and the number of countries falling below the median stays relatively constant.

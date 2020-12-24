# EDA_Udacity_Alaa_Zarban

Red Wine EDA by Alaa Zarban
========================================================



```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using in your analysis in this code
# chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk. This
# prevents the code from displaying in the knitted HTML output. You should set
# echo=FALSE for all code chunks in your file, unless it makes sense for your
# report to show the code that generated a particular plot.

# The other parameters for "message" and "warning" should also be set to FALSE
# for other code chunks once you have verified that each plot comes out as you
# want it to. This will clean up the flow of your report.

library(ggplot2)
library(gridExtra)

```

```{r echo=FALSE, Load_the_Data}
# Load the Data
red_wine <- read.csv('wineQualityReds.csv')
```

```{r}
#head(red_wine)
lengths(red_wine)
```
### What is the structure of your dataset?
The provided data set consist of mutiple records of red wine and its charastrastics. The data set consist of 13 columns and 1599 records of wine. Each record provide the different feature and aspect of each record of wine. 

# Univariate Plots Section


```{r echo=FALSE, Univariate_Plots}
summary(red_wine)
```

From the above summary, we can see that the quality of the wine varies from 3 to 8. The mean of all red wine records is 5.6 and the median is 6.

I am curious to know what feature/charactrastic of the provided in the data set indicate a high quality red wine and the vise versa. Let dive deep into the data and find-out.

```{r}
qplot(x=quality , data = red_wine)
```





# Univariate Analysis


### What is/are the main feature(s) of interest in your dataset?
The red wine dataset consists of 13 column. eleven column are the features/characteristics of red wine that might affect the quality.



### What other features in the dataset do you think will help support your \
From the above summary, we can see that the quality of the wine varies from 3 to 8. The mean of all red wine records is 5.6 and the median is 6.

### Did you create any new variables from existing variables in the dataset?

yes. quality categories.

### Of the features you investigated, were there any unusual distributions? \
Did you perform any operations on the data to tidy, adjust, or change the form \
of the data? If so, why did you do this?

**Concern**
Do I have to check the histogram of each column whether it is normally distributed. If not, why do i have to transform it?




# Bivariate Plots Section


From the graphs above, we can see that most of the red wine score 5 and 6 in the quality, very few are having low quality and high quality as well.

```{r}
r1 <- qplot(x = quality , y = pH , data = red_wine) 
r2 <- qplot(x = quality , y = fixed.acidity , data = red_wine)
r3 <- qplot(x = quality , y = volatile.acidity , data = red_wine)
r4 <- qplot(x = quality , y = citric.acid , data = red_wine)
r5 <- qplot(x = quality , y = residual.sugar , data = red_wine)
r6 <- qplot(x = quality , y = chlorides , data = red_wine)
r7 <- qplot(x = quality , y = free.sulfur.dioxide , data = red_wine)
r8 <- qplot(x = quality , y = total.sulfur.dioxide , data = red_wine)
r9 <- qplot(x = quality , y = density , data = red_wine)
r10 <- qplot(x = quality , y = sulphates , data = red_wine)
r11 <- qplot(x = quality , y = alcohol , data = red_wine)

grid.arrange(r1,r2 ,r3, ncol=2)

grid.arrange(r4,r5,r6, ncol=2)

grid.arrange(r7,r8,r9, ncol=2)

grid.arrange(r10,r11, ncol=2)


```

Scatter plot is not the suitable graph to show clear relation between quality and other charastristics. Let's use another type.

First, let's create 4 categories for red wine categories.

```{r}
red_wine$quality.factor <- factor(red_wine$quality)
red_wine$quality.category <- NA
red_wine$quality.category <- ifelse(red_wine$quality>=7, 'good', NA)
red_wine$quality.category <- ifelse(red_wine$quality==5, 'medium', red_wine$quality.category)
red_wine$quality.category <- ifelse(red_wine$quality==6, 'medium', red_wine$quality.category)
red_wine$quality.category <- ifelse(red_wine$quality<=4, 'bad', red_wine$quality.category)

red_wine$quality.category <- factor(red_wine$quality.category, levels = c("bad", "medium", "good"))

head(red_wine)

```
Now, we can create our boxplot graph with the different categories created for the quality.

```{r}
r1 <- qplot(x = quality.category , y = pH , data = red_wine , geom = 'boxplot') 
r2 <- qplot(x = quality.category , y = fixed.acidity , data = red_wine, geom = 'boxplot')
r3 <- qplot(x = quality.category , y = volatile.acidity , data = red_wine, geom = 'boxplot')
r4 <- qplot(x = quality.category , y = citric.acid , data = red_wine, geom = 'boxplot')
r5 <- qplot(x = quality.category , y = residual.sugar , data = red_wine, geom = 'boxplot')
r6 <- qplot(x = quality.category , y = chlorides , data = red_wine, geom = 'boxplot')
r7 <- qplot(x = quality.category , y = free.sulfur.dioxide , data = red_wine, geom = 'boxplot')
r8 <- qplot(x = quality.category , y = total.sulfur.dioxide , data = red_wine, geom = 'boxplot')
r9 <- qplot(x = quality.category , y = density , data = red_wine, geom = 'boxplot')
r10 <- qplot(x = quality.category , y = sulphates , data = red_wine, geom = 'boxplot')
r11 <- qplot(x = quality.category , y = alcohol , data = red_wine, geom = 'boxplot')

grid.arrange(r1,r2 ,r3, ncol=2)

grid.arrange(r4,r5,r6, ncol=2)

grid.arrange(r7,r8,r9, ncol=2)

grid.arrange(r10,r11, ncol=2)


```

From the boxplots graph above we can summarize our findings as follows:
- As pH increases, the quality decreases.
- As volatile.acidity increases, the quality decreases.
- As fixed.acidity increases, the quality increases.
- As citric.acid increases, the quality increases.
- As sulphates increases, the quality increases.



From the below correlation test, no strong relation ( > 0.7 )  found between variables in the dataset.
```{r}
cor(red_wine[,2:13]) > 0.7

```

From the below correlation test, we have couple of moderate relations ( > 0.5 )  found between variables in the dataset, summarized as follows:

citric.acid <> fixed.acidity
density <> fixed.acidity
fixed.acidity <> citric.acid
total.sulfur.dioxide <> free.sulfur.dioxide



```{r}
cor(red_wine[,2:13]) > 0.5

```



# Bivariate Analysis


### Talk about some of the relationships you observed in this part of the \
investigation. How did the feature(s) of interest vary with other features in \
the dataset?

This has been answered above.

### Did you observe any interesting relationships between the other features \
(not the main feature(s) of interest)?

This is answered in the correlation section above.

### What was the strongest relationship you found?


From below code, the strongest relation is:
fixed.acidity <> pH             =   -0.68
fixed.acidity <> citric.acid    =   0.67



```{r}
# The following piece of code has been taken from:
#  http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}

library(Hmisc)
check <- rcorr(as.matrix(red_wine[,2:13]))
check_faltten <- flattenCorrMatrix(check$r, check$P)
sorted_data <- check_faltten[order(check_faltten$cor),]
sorted_data

```

# Multivariate Plots Section



From above analysis, the top charactrastics that affects quality are 

Negativly:
volatile.acidity

Postivly
alcohol
sulphates

```{r echo=FALSE, Multivariate_Plots_2}

ggplot(aes(x = sulphates, y = alcohol, colour = quality.factor), 
       data = red_wine) + 
  geom_point(aes(size = quality.factor)) 


```

The following is a perfect one from the internet. But i'm not conviced.

But i don't understand why do we transform ( take log10) of the data. By doing this, we are changing the actual value of the data provided. Why do we have to make the data normally distributed instead skewed.

```{r echo=FALSE, Multivariate_Plots}

ggplot(aes(x = log10(sulphates), y = alcohol, colour = quality.factor), 
       data = red_wine) + 
  geom_point(aes(size = quality.factor)) +
  scale_color_brewer(type = 'div', palette="Set1") +
  scale_x_continuous(lim=c(quantile(log10(red_wine$sulphates), 0.01),
                           quantile(log10(red_wine$sulphates), 0.99)))+
  scale_y_continuous(lim=c(quantile(red_wine$alcohol, 0.01),
                           quantile(red_wine$alcohol, 0.99))) 

```


# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the \
investigation. Were there features that strengthened each other in terms of \
looking at your feature(s) of interest?

From above multivariate plot, we can observe an increase in quality of red wine with the increase of both alcohol and sulphates. 

### Were there any interesting or surprising interactions between features?

### OPTIONAL: Did you create any models with your dataset? Discuss the \
strengths and limitations of your model.

------

# Final Plots and Summary

> **Tip**: You've done a lot of exploration and have built up an understanding
of the structure of and relationships between the variables in your dataset.
Here, you will select three plots from all of your previous exploration to
present here as a summary of some of your most interesting findings. Make sure
that you have refined your selected plots for good titling, axis labels (with
units), and good aesthetic choices (e.g. color, transparency). After each plot,
make sure you justify why you chose each plot by describing what it shows.

### Plot One
```{r echo=FALSE, Plot_One}
qplot(x=quality , data = red_wine , main = "Quality distribution") + 
  ylab("Count of wine")
```

### Description One

From the above , we can see that the quality of the wine varies from 3 to 8. The mean of all red wine records is 5.6. Most the the red wine records categorized as 5 and 6.


### Plot Two
```{r echo=FALSE, Plot_Two}
r1 <- qplot(x = quality.category , y = pH , data = red_wine , geom = 'boxplot') 
r3 <- qplot(x = quality.category , y = volatile.acidity , data = red_wine, geom = 'boxplot')
r2 <- qplot(x = quality.category , y = fixed.acidity , data = red_wine, geom = 'boxplot')
r4 <- qplot(x = quality.category , y = citric.acid , data = red_wine, geom = 'boxplot')
r10 <- qplot(x = quality.category , y = sulphates , data = red_wine, geom = 'boxplot')

grid.arrange(r1,r3, ncol=2)

grid.arrange(r4,r10,r2 , ncol=2)
```

### Description Two


### Plot Three
```{r echo=FALSE, Plot_Three}
ggplot(aes(x = sulphates, y = alcohol, colour = quality.factor  ), 
       data = red_wine ) +
  ggtitle("Sulphates and Alcohol relation with respect to Quality") +
  geom_point(aes(size = quality.factor)) 
```

### Description Three

This shows the relation between alcohol, sulphates and the quality of red wine. We can notice that an increase in both variables affects the quality of alcohol.

------

# Reflection


The challenges in this data set is that there is no definit variable that affects the quality of the wine massively. There are many non-strong correlations between variables and it was superising that sugar has almost no affect on quality evern though it makes drinks tastes good. Another challenge, wine is prohibited in our religion. Hence, it is more difficult to invistigate on something we are not familiar with. It would be easier if we understand and know the nature behind the thing we are experimenting. 

Plotting with ggplot2
合計点数 10


1. 質問 1

The function ggplot() requires that the value supplied to the 'data' argument be a 1点

- geom
-✔️ data frame
- list
- character vector


2. 質問 2

You run the following code and get the error printed directly after the code: 

```  
library(ggplot2) 
library(faraway) 
data(nepali) 
head(nepali, 3) 

## id sex wt ht mage lit died alive age 
## 1 120011 1 12.8 91.2 35 0 2 5 41 
## 2 120011 1 12.8 93.9 35 0 2 5 45 
## 3 120011 1 13.1 95.2 35 0 2 5 
```

What happened? 1点

-✔️ Because you put the + at the start of the second line, instead of at the end of the first line, R thought
the call was over and tried to run the first line by itself, and then tried to run the second line by itself,
without an associated ggplot object.
- You have not yet loaded the nepali data frame.
- You are trying to create a scatterplot for a categorical variable.
- The nepali dataset lacks one or both of the two columns (ht, wt) that you used in the aes statement of
the call.


3. 質問 3
You have a dataset of observations on study subjects which has height and weight for each subject measured at multiple time points. You want to create a scatterplot of height (x-axis) by weight (y-axis). Since there are multiple measurements per subject, you would like to plot the measurements for each subject in a
separate color. There is a column in the data called id that gives the unique id of each study subject; this column currently has the class “numeric”. 

Which of the following choices is a good strategy
for creating a plot where data points use color to identify the subject? 1点

-1@ Leave the id column of the data frame as a numeric, so a continuous color palette will be used, and use ggplot to create a scatterplot, then use + to add the element color(aes(id)).

Example code:
  
```
df %>%
  ggplot(aes(x = height, y = weight)) + 
  geom_point() + 
  color(aes(id))
```

-3@ Convert the id column of the data frame to a factor, so a discrete color palette will be used, then use ggplot to create a scatterplot and specify color
= id as the sole argument in the geom_point() call.

Example code: 
  
```
df %>% 
  mutate(id = factor(id)) %>%
  ggplot(aes(x = height, y = weight)) + 
  geom_point(color = id)
```

-2@ Leave the id column of the data frame as a numeric, so a continuous color palette will be used, then use ggplot2 to create a scatterplot and specify color = id in the aes() section of the ggplot() call.  

Example code:
  
```
df %>% 
  ggplot(aes(x = height, y = weight, color = id)) + 
  geom_point()
```

- Convert the id column of the data frame to a factor, so a discrete color palette will be used, then use ggplot2 to create a scatterplot and specify
color = id in the aes() section of the ggplot() call.  

Example code:
  
```
df %>% 
  mutate(id = factor(id)) %>%
  ggplot(aes(x = height, y = weight, color = id)) + 
  geom_point()
```


4. 質問 4

You want to save a figure from R as a pdf file named “MyFig.pdf”. Which of the following describes the
proper steps to take? 1点

- Wrap all your code to create the figure in a pdf statement. 

Example code: 
```
pdf("MyFig.pdf"){
  ggplot(df, aes(x = x, y = y)) + 
    geom_point()}
```

-✔️ First run pdf("MyFig.pdf") to open a pdf device, then run all your code to create the figure, then
use dev.off() to close the device.

Example code: 
```
pdf("MyFig.pdf")
ggplot(df, aes(x = x, y = y)) + 
  geom_point()
dev.off()
```

- First run dev.on("MyFig.pdf") to open a pdf device, then run all your code to create the figure, then use
dev.off() to close the device.

Example code:
```
dev.on("MyFig.pdf")
ggplot(df, aes(x = x, y = y)) + 
  geom_point()
dev.off()
```

- First run all your code to create the figure, then run pdf("MyFig.pdf") to open a pdf device, then use
pdf.off() to close the device.

Example code: 
  
```
ggplot(df, aes(x = x, y = y)) + 
  geom_point()
pdf("MyFig.pdf")
dev.off()
```


5. 質問 5

Consider the following dataset.

```
library(lattice)
data(barley)
head(barley)

#     yield   variety year            site
# 1 27.00000 Manchuria 1931 University Farm
# 2 48.86667 Manchuria 1931          Waseca
# 3 27.43334 Manchuria 1931          Morris
```

Which ggplot2 expression would make a scatterplot of year and yield while coloring each point according to its variety? 1点

- ggplot(data = barley, aes(year, yield)) + geom_point()
- ggplot(data = barley, aes(year, yield, color = variety))
-✔️ ggplot(data = barley, aes(year, yield)) + geom_point(aes(color = variety))
- ggplot(data = barley, aes(year, yield)) + geom_point(color = variety)


6. 質問 6

Consider the following dataset.

```
data(trees)
head(trees)

##  Girth Height Volume
##   8.3     70   10.3
##   8.6     65   10.3
##   8.8     63   10.2
##  10.5     72   16.4
##  10.7     81   18.8
##  10.8     83   19.7
```

You want to create a scatter plot showing Girth on the x-axis, Height on the y-axis, and Volume with point size.

Which of the following code would create that plot? 1点

-@ 
```
ggplot(trees, aes(x = Girth, y = Height, size = Volume)) +
  geom_point()
```

-@
```
trees %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point(size = Volume)
```

-@
```
ggplot() +
  geom_point(data = trees,
             aes(x = Girth, y = Height, size = Volume))
```

-@
```
trees %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point(aes(size = Volume))
```

-@
```
trees %>%
  ggplot() +
  geom_point(x = Girth, y = Height, size = Volume)
```

-@
```
ggplot() +
  geom_point(trees,
             aes(x = Girth, y = Height, size = Volume))
```

-@
```
trees %>%
  ggplot(aes(x = Girth, y = Height, size = Volume)) +
  geom_point()
```

-@
```
trees %>%
  ggplot() +
  geom_point(aes(x = Girth, y = Height, size = Volume))
```


7. 質問 7

How can you create small multiples using ggplot2? 1点

-✔️ Add facet_wrap to the geom, specifying the formula to use for faceting
-✔️ Add facet_grid to the geom, specifying one or two columns to use for faceting
- Add a scales element to decrease the range of the plot scales
- Add multiple geoms to the plot using the + syntax


8. 質問 8

What does geom_smooth(method = "lm") do when it is added to a ggplot object? 1点

-1@ It adds a palette of colors for plotting the points in a scatterplot
-2@ It adds a fitted linear regression line to the plot
- It adds transparency to the points to avoid over plotting
-3@ It adds a nonlinear smoother to the plot


9. 質問 9

When plotting multiple panels using faceting for a single variable, what controls the order in which the panels are plotted? 1点

-✔️ Panels are ordered by the alphabetical order or by the order of the levels of the faceting variable
- Panels are ordered randomly
- The order of panels must be specified by the user
- Panels are ordered by the number of data points in each panel


10. 質問 10

What function would you use to limit the range of the x-axis in a scatterplot? 1点

-✔️ scale_x_continuous()
- scale()
- geom_scale()
- scale_y_continuous()


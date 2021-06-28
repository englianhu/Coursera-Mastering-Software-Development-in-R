Basics of grid graphics
合計点数 10


1. 質問 1

You want to customize the color scale, theme elements, and include a mathematical equation in the title of a faceted scatterplot. Is it necessary to create the plot using grid graphics rather than ggplot2? 1点

- Yes, because because this code requires too much customization to plot using ggplot2, you should consider coding it using grid graphics. 
-✔️ No, you do not need to code this plot in grid graphics (although you could).


2. 質問 2

Which of the following types of objects are created with grid graphics, either directly or using a package built on grid graphics? 1点

- A map created with ggmap 
- A map created by running a plot call on a SpatialPoints object
-@0.6 A plot created using R's base graphics 
-@0.6 A grob object
-@0.6 A ggplot object


3. 質問 3

How can you customize the aesthetics of a grob, like color and line style, when creating the grob? 1点

- Add an aesthetic statement, using the aes, function, to the grob once it's been created. For example:
```
circleGrob(x = 0.5, y = 0.5, r = 0.5) + 
  aes = list(col = "grey")
```

-✔️ Use key-value pairs within a call to the gpar function as the input to the gp parameter of the grob family function. For example:
```
circleGrob(x = 0.5, y = 0.5, r = 0.5,
           gp = gpar(col = "grey"))
```

- Use key-value pairs within a call to the aes function as the input to the gp parameter of the grob family function. For example:
```
circleGrob(x = 0.5, y = 0.5, r = 0.5,
           gp = aes(col = "grey"))
```

- Use key-value pairs within a list as the input to the aes parameter of the grob family function. For example:
```
circleGrob(x = 0.5, y = 0.5, r = 0.5,
           aes = list(col = "grey"))
```


4. 質問 4

TRUE or FALSE: You use viewports to create new graphical elements when using grid graphics. 1点

- TRUE
-✔️ FALSE


5. 質問 5

Which of the following are graphical elements can you create in an R plot using one of the grob family of functions from the grid package? 1点

-✔️ Point
-✔️ Line
-✔️ Text
- Table
-✔️ Polygon
-✔️ x- and y- axes


6. 質問 6

What is the name of the plotting windows that you can move into and out of to customize plots using grid graphics? 1点

- Ports
- Popups
-✔️ Viewports
- Grobs


7. 質問 7

Which of the following tasks can be done using the grid.arrange function in the gridExtra package? 1点

-@0.6 Draw multiple base plots to the current graphics device.
- Draw a single grob to the current graphics device.
-@0.6 Draw multiple ggplot objects to the current graphics device.
- Rearrange the order of grobs in a gTree grob. 
-@0.6 Draw multiple grobs to the current graphics device.


8. 質問 8

Which of the following are possible units that can be used with grid graphics coordinate systems? 1点

-✔️ "cm"
- "viewport"
- "grob"
-✔️ "npc"
-✔️ "native"


9. 質問 9

If you have to integrate base graphics and grid graphics output, what is the name of the package that can be used to do that? 1点

-✔️ gridBase
- gridExtra
- grid
- ggplot2


10. 質問 10

What graphics systems is ggplot2 built on? 1点

-✔️ Grid graphics
- Base graphics
- Lattice graphics
- The tidyverse


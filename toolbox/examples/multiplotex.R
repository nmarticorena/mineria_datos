require(datasets)
require(ggplot2)
require(grid)

#Generate ggplots and a list
a=qplot(mpg, wt, data = mtcars, colour = cyl, main = "Plot a")
b=qplot(mpg, wt, data = mtcars, facets = vs ~ am, main="Plot b")
c=qplot(mpg, data = mtcars, main="Plot c",bins=30)
d=qplot(factor(cyl), wt, data = mtcars,
        geom = c("boxplot", "jitter"),main="Plot d")

plist=list(d,c)

#plot them without saving anywhere

multiplot(a,b,c,d) #one column

multiplot(a,b,c, cols=3) #three columns

multiplot(a,b,c,d, layout=rbind(c(1,1,2,3),c(1,1,4,4))) # complex layout

multiplot(a,b,plotlist= plist, cols=2) # pass c and d in a list.

# and storage them...

multiplot(a, b, plotlist= plist, file="multiplots.pdf", cols=2)

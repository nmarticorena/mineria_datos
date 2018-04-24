#To Quantile example
x=rnorm(1000) # 1000 random observations of a normal distribution

#to see the quartile of every observation can use:
table=data.frame(x,"Quartile"=toQuantiles(x,4))
head(table) # the second column correspond to the function

# omite the second parameter to separate by percentiles:
table=data.frame(x,"Percentile"=toQuantiles(x))
head(table)


x=rnorm(100000,mean=500,sd=150)
y=rnorm(100000,mean=2000,sd=100)
df=data.frame(col1=x,col2=y)
normalize=makeScale(df)

normalize$scale
normalize$unScale
normalize$cols_mean
normalize$cols_sd

#see the results
require(ggplot2)
dfn=normalize$scale(df)

a=ggplot(df, aes(col1),main="plot 1") +
  stat_bin(binwidth = 20)
b=ggplot(df, aes(col2),main="plot 2") +
  stat_bin(binwidth = 20)
c=ggplot(dfn, aes(col1),main="scaled plot 1") +
  stat_bin(binwidth = 0.02)
d=ggplot(dfn, aes(col2),main="scaled plot 2") +
  stat_bin(binwidth = 0.02)

multiplot(a,b,c,d,cols=2)

#Replace Null example:

x=c(1,1,2,3,5,NaN,Inf,8,NA,NULL,6)

#Ex.1 Replace nulls by a number like 4

replaceNull(x,4)

#Ex.2 Replace nulls by an array like (333,777)

replaceNull(x,c(333,777))

#Ex.3 Replace arbitrary Nulls, like Inf and 1, by a number like 0

replaceNull(x,0,c(Inf,1))



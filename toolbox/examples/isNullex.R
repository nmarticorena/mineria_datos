#Is Null examples:

x=c(1,2,NaN,Inf,5,2,8)

# Ex.1 By default the function recognize Nan and Inf as nulls,
# so the second input can be omited.

isNull(x)

#Ex. 2 What is a "null" can be specified.

isNull(x,c(NaN,NULL,NA))

#Ex.3 This function can also be used to find specifics values.

x[isNull(x,c(2,5))]

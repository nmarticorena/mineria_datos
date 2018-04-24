#Is Between example:
x=runif(20,0,30) #20 random numbers between 0 and 30
x
#Ex.1 Can be used to check the x's numbers which are in a range,
#like (8,22):

isBetween(x,8,22)

#Ex.2 It Can compare arrays' elements:
a=1:20
b=11:30

isBetween(x,a,b)

#Ex.3 Can ignore some elements of a and b, redifining the "nulls":

isBetween(x,a,b,c(10:20)) # so if a and b takes values from 10 to 20 at same position it would return true.

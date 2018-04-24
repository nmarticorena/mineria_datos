#Lagpad examples:
x=1:10
#Ex.1 : Displace x 3 positions and fill with zeros:
lagpad(x,3) # the third argument can be omited

#Ex.2 : Displace x the last 3 positions an fill with zeros:
lagpad(x,-3)

#Ex.3 : Displace x 5 positions and fill with 33:
lagpad(x,5,33)

#Ex.4 : Displace x 5 positions and fill with 33:
lagpad(x,5,101:105)


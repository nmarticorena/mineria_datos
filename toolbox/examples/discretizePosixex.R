#Discretize Posix example:
x=Sys.time()+ seq(0,24*3600,25*60) #this is an array with every 25 minutes
# for the next 24 hours

#to get it discretized with 30 minutes intervals:
discretizePosix(x,1800)

#to get it discretized with 15 minutes intervals:
discretizePosix(x,900)

#to get it discretized with 1 minute intervals:
discretizePosix(x,60)


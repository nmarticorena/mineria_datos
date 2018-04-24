x=data.frame(row=1:15,letter=rep(letters[1:3],times=5),number=rep(3:5,each=5))
y=data.frame(row=1:15,letter=rep(letters[1:5],each=3),number=rep(3:7,times=3))
x
y
#check the differences
merge(x,y,by="letter") #returns all possibles combinations for matching rows

uniqueMerge(x,y,by="letter") #return all not matching rows filled with NAs

uniqueMerge(x,y,by="letter", all=F, all.x=T) #return all x's not matching rows

uniqueMerge(x,y,by="letter", all=F, all.y=T) #return all y's not matching rows

uniqueMerge(x,y,by="letter", all=F) #return just the first matching rows of both

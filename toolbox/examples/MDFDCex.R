Nsample=rnorm(10000)#normal distribution sample
Usample=runif(10000)#uniform distribution sample
df=data.frame(norm=Nsample,Nprob=pnorm(Nsample),convertedQ=qexp(pnorm(Nsample)),unif=Usample,Uprob=punif(Usample),convertedQ2=qexp(punif(Usample))) 
#make a df with probabilities and the converted values to check later

conversor=makeDataFrameDistributionConversor(df,columns=c("norm","unif"),objectiveDistribution=qexp) 
#declare the conversor to exp distribution for the sample columns

#and check
dfconv=conversor(df)
head(dfconv)

#see a plot
df3=data.frame(df["norm"],dfconv["norm"],df["unif"],dfconv["unif"])
names(df3)=c("sample1","conversion1","sample2","conversion2")
a=qplot(x=sample1,y=conversion1,data=df3,main="norm to exp by function")
b=qplot(x=sample2,y=conversion2,data=df3,main="unif to exp by function")
c=qplot(x=norm,y=convertedQ,data=df,main="Real norm to exp",ylab = "exp")
d=qplot(x=unif,y=convertedQ2,data=df,main="Real unif to exp", ylab = "exp")
multiplot(a, b, c, d, cols=2)
###END

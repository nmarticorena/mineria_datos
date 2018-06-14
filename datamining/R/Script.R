source("R/init.R")

# Pre process -------------------------------------------------------------

files="data/gtd.csv"
featuresFile = "data/features.tab"

updateFeatures(dataset.file = files,
               features.file = featuresFile,
               ds.sep = ",",
               f.sep = "\t"
)

gtd=readDataset(files=files,features.list = getFeatures(features.file = featuresFile, sep = "\t"),sep=",")
gtd=forceClass(gtd)

gtdSA=gtd[gtd[,"region_txt"] == "South America",]
gtdSA2=gtdSA[gtdSA$iyear %in% 1975:2000,]
gtdSA2=limitFactors(gtdSA2,newlevel = "Otros")

gtdContinentes=ddply(gtd,.(region_txt,iyear),nrow)
gtdTotal=ddply(gtd,.(iyear),nrow)

gtdGrupos=ddply(gtd,.(gname),nrow)

america=get_map("Montevideo",zoom = 3 )

gtdEfectividadContinentes=ddply(gtd,.(region_txt,success),nrow)



gtdLetalidadContinentes=merge(aggregate(nkill ~ region_txt,gtd,FUN = sum),aggregate(nwound ~ region_txt,gtd,FUN = sum),by="region_txt")
gtdLetalidadContinentes=merge(gtdLetalidadContinentes,aggregate(V1~region_txt,gtdContinentes,FUN=sum),by="region_txt")
gtdLetalidadContinentes=transform( gtdLetalidadContinentes, Letalidad=nkill/V1, Masividad=(nkill+nwound)/V1)
gtdLetalidadContinentes=gtdLetalidadContinentes[,c("region_txt","Letalidad","Masividad")]
order=c(gtdLetalidadContinentes$Letalidad,rep(0,times=12))
gtdLetalidadContinentes=melt(gtdLetalidadContinentes, id.vars = "region_txt")
gtdLetalidadContinentes[,"order"]=order
rm(order)


# Visualizaciones ---------------------------------------------------------



plot1=ggplot(data = aggregate(data = gtdContinentes,V1~region_txt,FUN = sum),aes(x=reorder(region_txt,V1),y=V1))+
  geom_bar(stat = "identity",fill=c(rep("grey",times=9),"orange","grey","grey"))+
  coord_flip()+
  xlab("")+ylab("")+
  ggtitle("Cantidad total de atentados")+
  th

plot2=ggplot(gtdContinentes,aes(x= iyear, y= V1,colour=region_txt)) +
  geom_path(lineend = "round",linejoin = "mitre")+
  scale_color_discrete(name="Continente: ")+
  ylab("Cantidad de atentados")+
  xlab("")+xlim(1970,2017)+
  th

plot10=ggplot(gtdTotal,aes(x= iyear, y= V1)) +
  geom_path(lineend = "round",linejoin = "mitre")+
  scale_color_discrete(name="Continente: ")+
  ylab("Cantidad de atentados")+
  xlab("")+xlim(1970,2017)+
  th

plot3=ggplot(gtdContinentes[gtdContinentes$region_txt=="South America",],aes(x= iyear, y= V1,colour=region_txt)) +
  geom_path(lineend = "round",linejoin = "mitre")+
  scale_color_discrete(name="Continente: ")+
  ylab("Cantidad de atentados")+
  xlab("")+xlim(1970,2017)+
  th+theme(legend.position = "top")

plot4=ggplot( ddply(gtdSA,.(country_txt,iyear),nrow) ,aes(x= iyear, y= V1,colour=country_txt)) +
  geom_path(lineend = "round",linejoin = "mitre")+
  scale_color_discrete(name="País: ")+
  ylab("Cantidad de atentados")+
  xlab("")+xlim(1970,2017)+
  th+
  geom_vline(xintercept = 1975,col = "brown2",lwd = 0.5)+
  geom_vline(xintercept = 2000,col = "brown2",lwd = 0.5)

plot5=ggmap(america)+
  geom_point(data = gtdSA,mapping = aes(y=latitude,x=longitude,size=nkill),color="red",alpha=0.1)+
  ylim(c(-50,15))+xlim(c(-90,-30))+th+scale_size(name = "Muertes")


plot6=ggplot(ddply(gtdSA2,.(targtype1_txt),nrow),aes(x="",y=V1,fill=targtype1_txt))+
  geom_bar(width = 1 , stat = "identity")+
  scale_fill_discrete(name="Tipo de objetivo")+
  geom_text(aes(y=rep(sum(V1),times=length(V1))-V1/2-lagpad(cumsum(V1),1),label = percent(V1/sum(V1))))+
  blank_theme+ggtitle("Ataques en Sur América 1975-2000")
  theme(axis.text.x=element_blank())


plot7=ggplot(aggregate(nkill ~ region_txt,gtd,FUN = sum),aes(x=reorder(region_txt,nkill),y=nkill))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("")+ylab("")+
  ggtitle("Muertos")+
  th


plot8=ggplot(gtdLetalidadContinentes,aes(x=reorder(region_txt,order),y=value,fill=variable))+
  geom_bar(stat = "identity",position = position_jitterdodge())+
  coord_flip()+xlab("")+
  th

plot9=ggplot(gtd,aes(x=region_txt,y=nkill))+
  geom_boxplot()+
  coord_flip()+
  xlab("")+ylab("Muertos")+scale_y_log10()+
  ggtitle("Distribución de muertes por ataques")+th



plot10=ggplot(gtdEfectividadContinentes,aes(x=reorder(region_txt,success),y=V1,fill=success))+
  geom_bar(stat = "identity",position = position_jitterdodge())+
  coord_flip()+xlab("")+
  th
print(plot10)

plot11=ggplot(gtdEfectividadSA,aes(x=reorder(country_txt,success),y=V1,fill=success))+
  geom_bar(stat = "identity",position = position_jitterdodge())+
  coord_flip()+xlab("")+
  th
print(plot11)

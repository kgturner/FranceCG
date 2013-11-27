#Fr data
#with SK
#time series analysis

#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)

qplot(data=Frdatsk.l, m.date, lfl, color=Origin)+geom_line(group=Origin)
ggplot(Frdatsk.l,aes(m.date, lfl, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf length")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, lfw, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf width")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, lfc, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf count")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, rd, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("rosette diameter")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))
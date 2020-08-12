
library(ggplot2)
library(dplyr)

# read all text in folder
text = readtext::readtext('*.txt')
# remove newlines tabs etc
text$text = gsub("[\t\r\n]", "", text$text)
# get one long string
text = paste0(text$text,collapse='')
# get characters
text = strsplit(text,'')[[1]]


# function to calculate the distance between two samples in x/y
calcdist_df = function(d){
  d = d %>% mutate(
    gx_onelag = c(gx[-1], NA),
    gy_onelag = c(gy[-1], NA),
    dist = sqrt((gx-gx_onelag)^2+(gy-gy_onelag)^2))
  return(d)
}

# load the data and preprocess it 
preprocess = function(type){
  data = read.table(paste0('thesisart_',type,'.csv'),sep=",",header=TRUE)
  plotdata = data
  
  if(type=="ms"){
    plotdata = calcdist_df(plotdata) 
  }else if (type=="blink"){
    plotdata =calcdist_df(plotdata)
  }
  else{
  # subselect the number of samples iteratively in cases where the samples are too close together
    for(k in 1:20){
      plotdata = calcdist_df(plotdata)
      
      ix = which(plotdata$dist<0.1)
     
      print(length(ix))
      plotdata = plotdata[-ix[seq(1,length(ix),2)],]
   }
  }
  plotdata$type = type
  return(plotdata)
  
}

# preprocess the ET data & subselect
grid = preprocess('grid')%>%filter(block=="1")
fv = preprocess('fv')%>%filter(block=="2")

smooth = preprocess('smooth')

ms = preprocess('ms')%>%filter(block=="3")

blink = preprocess('blink')

# subselect blink
blink2 = blink[seq(1,dim(blink)[1],10),]
# x is time
blink2$gx = blink2$smpl_time
# y is the block - i.e. everything is in horizontal lines
blink2$gy = blink2$block + 0.3*(blink2$eyetracker=="pl")

# pupil dilation 
dil = preprocess("dil")
# subselect
dil2 = dil[seq(1,dim(dil)[1],5),]
# x is time
dil2$gx = dil2$smpl_time
# y is pupil size + a factor for block 
dil2$gy = dil2$pa + dil2$block*0.7

#subselect ms
ms2 = ms[seq(1,dim(ms)[1],5),]

# subselect smooth pursuit
smooth2 = smooth[seq(1,dim(smooth)[1],5),]

# combine them
plotdata = rbind(grid,smooth2,fv,ms2,blink2,dil2)
# init 
plotdata$plottext = ''
# fill in the text
plotdata[1:min(dim(plotdata)[1],length(text)),"plottext"] = text[1:dim(plotdata)[1]]

plotdata$type = factor(plotdata$type, levels = c("grid","smooth","freeview","ms","blink","dil"))

# ggplot :-)
ggplot(plotdata,aes(x=gx,y=gy,color=eyetracker,label=plottext))+
  geom_text()+
  facet_wrap(~type,ncol=2,scales = "free")+
  theme_void()

ggsave("v5.pdf")


# same plot as before but with "geom_point" instead of "geom_text"
ggplot(plotdata,aes(x=gx,y=gy,color=eyetracker,label=plottext))+
  geom_point()+
  facet_wrap(~type,ncol=2,scales = "free")+
  theme_void()

ggsave("v5_notext.pdf")

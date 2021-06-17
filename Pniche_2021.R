#### Smith et al. (2020) Contrasting Prochlorococcus temperature niches in the lab and across ocean basins

#### Packages ####
packages<-c('ncdf4','tidyr','RColorBrewer','ggplot2','class','fields','R.matlab',
            'readxl','gdata','propagate','nlstools','stringi','plyr','XML','biomod2',
            'grid', 'pwr','formattable','dplyr','zoo','lattice','gridExtra','gam','tidyr','cowplot',
            'nls2','rworldmap','readr','rgdal','sp','sm','mapproj','proj4','rstudioapi',
            'raster','maps','mapdata','rJava','maptools','jsonlite','yaml','stringr',
            'scales','reshape','gtable','ggExtra','KernSmooth','ks','np','purrr', 'boot')
funlist<-lapply(packages, function(x){if (x %in% rownames(installed.packages())){require(x, character.only=T)}else{
    install.packages(x,character.only=T);require(x,character.only = T)}})
#### Folders ####
datasets<-'~/Documents/PhD_Work/Research/Temp_Growth_Rate_Research/Data_Sets/'
outputfiles=('~/Documents/PhD_Work/Research/Temp_Growth_Rate_Research/Files/Lab_Output/')
Rds.outputfiles=('~/Documents/PhD_Work/Research/Temp_Growth_Rate_Research/Files/Rds_Saved/')
figureoutput=('~/Documents/PhD_Work/Research/Temp_Growth_Rate_Research/Figures/Final_Fixed/')
icloud<-c('~/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/PhD_Work/Research/Niche_Model/Output/Use/')
setwd('~/Library/Mobile\ Documents/com~apple~CloudDocs/')

##Functions####
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  x<- lapply(x,as.data.frame)
  names(x) <- sheets
  x
}
{
  # fieldstats_all_gaussian<-function (x3,y3,xx){
  #   #Take all they values and make and x value matrix cooresponding to he correct temperatures
  #   #If there are no zero values, input zero values
  #   #figure out a good z and w guess
  #   startmu<-mean((x3[!is.na(y3)]),na.rm=T)
  #   startsigma<-sd((x3[!is.na(y3)]),na.rm=T)
  #   
  #   nlsfitgaus<-tryCatch(nls(y3~(a*exp(-1/2*(x3-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
  #                            algorithm = 'port',
  #                            control=list(maxiter=5000,eval.max=5000),
  #                            start=list(a=max(y3,na.rm=T)-1,sigma=startsigma,mu=startmu),
  #                            upper=list(mu=startmu+1,sigma=12)),
  #                        error=function(e) print('TryCatch'), error=function(e) print('TryCatch'))
  #   if (all(nlsfitgaus=='TryCatch')){
  #     nlsfitgaus<-tryCatch(nls(y3~(a*exp(-1/2*(x3-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
  #                              algorithm = 'port',
  #                              control=list(maxiter=5000,eval.max=5000),
  #                              start=list(a=max(y3,na.rm=T)-1,sigma=startsigma,mu=startmu+1),
  #                              upper=list(mu=startmu+1,sigma=12)),
  #                          warning=function(w) return(w), error=function(e) print('TryCatch'))
  #     
  #   }
  #   
  #   pred_nls<-predictNLS(nlsfitgaus, newdata = data.frame(x3 = xx),interval='confidence',alpha=0.05)
  #   sum<-summary(nlsfitgaus)
  #   cof<-sum$coefficients
  #   SE<-cof[,'Std. Error']
  #   nfit=length(predict(nlsfitgaus))
  #   sduse<-SE*sqrt(nfit)
  #   mufit=coef(nlsfitgaus)['mu']
  #   sigmafit=coef(nlsfitgaus)['sigma']
  #   #Create a matrix of curves using each random z and w
  #   tt=seq(-2,40,by=0.1)
  #   ft2=(coef(nlsfitgaus)['a']*exp(-1/2*(tt-coef(nlsfitgaus)['mu'])^2/coef(nlsfitgaus)['sigma']^2))
  #   #ft2[ft2<0]=NA
  #   #Topt=Max of curve
  #   topt<-tt[which(ft2==max(ft2,na.rm=T))]
  #   ###CI for W
  #   WL=sigmafit-(1.96*SE['sigma'])
  #   WU=sigmafit+(1.96*SE['sigma'])
  #   TL=mufit-(1.96*SE['mu'])
  #   TU=mufit+1.96*SE['mu']
  #   #GOF
  #   BICgaus<-BIC(nlsfitgaus)
  #   #What+1.96*SEw upper
  #   WCI<-cbind(WL,WU)
  #   TCI<-cbind(TL,TU)
  #   results<-list(sigma=sigmafit,mu=mufit,pred_ci=pred_nls$summary,xx2=xx,pred=ft2,sum=sum,cof=cof,w_CI=WCI,t_CI=TCI,x=x3,y=y3,BIC=BICgaus)
  #   return(results)
  # }
}
field_gaussian<-function(x,y,iplot,ti,tot,alpha){
  df<-data.frame(x=x,y=y)
  ss<-c(floor(range(df$x,na.rm=T)[1]),ceiling(range(df$x,na.rm=T)[2]))
  #df<-df[complete.cases(df), ]
  gausout<-fieldstats_all_gaussian(df$x,df$y,seq(ss[1],ss[2],by=1))
  n.row<-round(tot/round(sqrt(tot)))
  n.col<-round(sqrt(tot))
  plot.mat<-matrix(seq(1:tot),nrow=n.row)
  plot.mat<-t(plot.mat)
  df2<-data.frame(x=seq(-2,40,by=0.1),y=gausout$pred)
  df2=df2[df2$x >=ss[1] & df2$x<=ss[2] ,]
  p.1<-ggplot() + 
    geom_point(data=df,aes(x, y),alpha=alpha)+
    xlab('')+
    ylab('')+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_rect(size=4), 
          #axis.line = element_line(colour = "black"),
          legend.key = element_blank(),
          text = element_text(size=12),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"))+
    geom_line(aes(x=df2$x,y=df2$y))+
    coord_cartesian(xlim=c(2,35))
  #geom_line(aes(x=c(gausout$mu-gausout$sigma,gausout$mu+gausout$sigma),y=c(0,0)),col='blue')+
  #geom_line(aes(x=c(xx[which.max(gausout$pred)],xx[which.max(gausout$pred)]),y=c(0,6)),col='red')
  
  if (iplot %in% plot.mat[,1]){
    p.1<-p.1+theme(
      axis.text.y = element_text(),
      axis.ticks.y = element_line())+
      ylab(bquote(Log[10]~Abundance~(cells~mL^-1)))
  }
  if (iplot %in% plot.mat[n.row,]){
    p.1<-p.1+theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line())+
      xlab(bquote(Temperature~(degree*C)))
  }
  p.1<-p.1+ggtitle(paste(ti))
  
  return(list(p.1,gausout))
}
ste.to.sd<-function(se){
  sd<-sqrt(length(se))*se
  return(sd)
} 


######## Load Files for Run Through ###########
load(file=paste(Rds.outputfiles,'cbs_gaus_plotlist.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'cbs_gaus_data.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_gaus_plot.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_real_results.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'raw_abundance_filtered.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_gaus_stats.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'strain_gaus_plot.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'strain_gaus_data.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_thomas_data.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'strain_thomas_data.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'gr_data_subset.Rdata',sep=''))

####### Fundamental Niche ##########
#### a) Loading in data ----
phytop=read_excel_allsheets('~/Downloads/Picophytoplankton Growth Rates(1).xlsx')
#Make each of the sheets a new data.frame
pico_growth=phytop$`Picophytoplankton Growth`
pico_n=phytop$`Picophytoplankton Growth (n val`
pico_sd=phytop$`Picophytoplankton Growth (Stand`
pico_error=phytop$Sheet4

#Fix the column names of the data.frames (from X_1 to T_0.0)
names=c('pico_growth','pico_n','pico_error','pico_sd')
dflist=list(gr=pico_growth,n=pico_n,er=pico_error,sd=pico_sd)
for (i in 1:2){
  x=dflist[[i]]
  x <- x[,colSums(is.na(x))<nrow(x)]
  start=which(colnames(x)=="Other")+1
  end=ncol(x)
  # if (i==1){
  colnames(x)[start:end]=paste('T_',x[1,start:end],sep='')
  # }else{
  #    colnames(x)[start:end]<-colnames(pico_growth)[start:end]
  #  }
  x=x[2:nrow(x),1:end]
  assign(names[i],x)
}

#Create a large array of all the fixed data.frames
picop=array(c(unlist(pico_growth), unlist(pico_n)), c(nrow(pico_growth), ncol(pico_growth), 2), 
            dimnames=list(rownames(pico_growth), colnames(pico_growth)))

listofnames=unique(picop[,1,1])[!is.na(unique(picop[,1,1]))]

picop2=picop[2:nrow(picop),str_detect(colnames(picop),'T_'),]
#If there is a NAN value for n but not for mu then make n 1
for (i in 1:nrow(picop2)){
  for (j in 1:ncol(picop2)){
    mu=picop2[i,j,1]
    n=picop2[i,j,2]
    if (!is.na(mu) && is.na(n)){
      picop2[i,j,2]=1
    }
  }
}

#### b) Remove data according to Thomas et al. (2012) standards ----
picoptab<-as.data.frame(picop[,,1],stringsAsFactors =F)
picoptab.n<-as.data.frame(picop[,,2],stringsAsFactors =F)
g.lvl<-levels(picoptab$`Genus/Species`)

picosubps<-subset(picoptab, `Genus/Species` %in% c("Prochlorococcus", "Synechococcus", "Prochlorococcus marinus","Synechococcus sp.")) 
picosubps.n<-subset(picoptab.n, `Genus/Species` %in% c("Prochlorococcus", "Synechococcus", "Prochlorococcus marinus","Synechococcus sp.")) 

picosublight<-subset(picosubps, `Light duration` %in% c("14:10 light:dark cycle", "12:12 light:dark cycle")) 
picosublight.n<-subset(picosubps.n, `Light duration` %in% c("14:10 light:dark cycle", "12:12 light:dark cycle")) 

light.lvl<-unique(picosublight$`Light level (µmol Q m¯² s¯¹)`)
light.lvl<-as.character(light.lvl)
light.lvl.2<-parse_number(light.lvl)
l.use<-light.lvl[which(light.lvl.2 >= 40)]
picosub.light.lvl<-subset(picosublight, `Light level (µmol Q m¯² s¯¹)` %in% c(l.use)) 
picosub.light.lvl.n<-subset(picosublight.n, `Light level (µmol Q m¯² s¯¹)` %in% c(l.use)) 

#### c) Subset to only the ecotypes we are interested in----
## Rename the ecotype thing bc can
ecotypes.use<-c('HL-I','HL-II','LL-I','LL-IV')
ecotypes.rename=c('eMED4','eMIT9312','eNATL2A','eMIT9313')
picosub.light.lvl$ecotype=NA
for (i in 1:4){
  picosub.light.lvl$ecotype[str_detect(picosub.light.lvl$`Ecotype/Group/Clade`, ecotypes.use[i])]=ecotypes.rename[i]
}
picosub.light.lvl$ecotype[which(picosub.light.lvl$`Ecotype/Group/Clade`=="LL-II (High B/A)")]=NA
picosub.light.lvl.n$ecotype=picosub.light.lvl$ecotype

picosub.light.lvl<-picosub.light.lvl[, c(which(colnames(picosub.light.lvl)=='ecotype'), (1:ncol(picosub.light.lvl))[-which(colnames(picosub.light.lvl)=='ecotype')])]
picosub.light.lvl.n<-picosub.light.lvl.n[, c(which(colnames(picosub.light.lvl.n)=='ecotype'), (1:ncol(picosub.light.lvl.n))[-which(colnames(picosub.light.lvl.n)=='ecotype')])]

picosubset<-subset(picosub.light.lvl,!is.na(picosub.light.lvl$ecotype))
picosubset.n<-subset(picosub.light.lvl.n,!is.na(picosub.light.lvl.n$ecotype))

save(picosubset,picosubset.n,file=paste(Rds.outputfiles,'gr_data_subset.Rdata',sep=''))

#### d) Run the Thomas curve stats on all the strains -----
strain.names<-unique(picosubset$`Our Naming`)
info_max_vett_new<-list()
info_max_vett_new_plot<-list()

for (i in 1:length(strain.names)){
  use=strain.names[i]
  y=picosubset[which(picosubset$`Our Naming`==use),grep('T',colnames(picosubset))]
  x=as.numeric(gsub('T_','',colnames(picosubset)[grep('T',colnames(picosubset))]))
  n=picosubset.n[which(picosubset.n$`Our Naming`==use),grep('T',colnames(picosubset.n))]
  y2=matrix(as.numeric(as.matrix(y)),nrow=nrow(y),ncol=ncol(y))
  x2=matrix(rep(x),nrow=nrow(y2),ncol=ncol(y2),byrow=T)
  n2=matrix(as.numeric(as.matrix(n)),nrow=nrow(y),ncol=ncol(y))
  y3=as.vector(y2)
  x3=as.vector(x2)
  n3=as.vector(n2)
  plot.df<-data.frame(x=x3,y=y3,n=n3)
  
  if (length(unique(x3[!is.na((y3))]))>4){
    
    # if (length(y3)>ncol(picop2)){
    startz=(x3[which(y3==max(y3,na.rm=T))][1])-2
    startw=x3[which(y3==tail(y3[!is.na(y3)],1))[length(which(y3==tail(y3[!is.na(y3)],1)))]]-10
    startw=startw-10
    plot.g<-ggplot(data=plot.df,aes(x=x,y=y))+
      geom_point()+
      stat_smooth(method='nls',formula=y~a*exp(x*0.0631)*(1-((x-z)/(w/2))^2),
                  method.args = list(start=c(a=0.01,z=startz,w=startw)),se=F)
    
    nlsfit1<-tryCatch(nls2(y3~(a*exp(x3*0.0631)*(1-((x3-z)/(w/2))^2)),
                           algorithm = 'port',weights=n3,
                           start=list(z=startz,w=startw,a=0.01)),warning=function(w) return(w),error=function(w) return(w))
    
    # control=list(eval.max=5000, iter.max=1000,warnOnly=T),
    # lower=list(z=-2,w=5,b=0.00001,a=0.00001),upper=list(z=40,w=35,b=Inf,a=Inf)),
    if ((is(nlsfit1,'warning')) | (is(nlsfit1,'error'))){
      
      nlsfit1<-tryCatch(nls(y3~(a*exp(x3*0.0631)*(1-((x3-z)/(w/2))^2)),
                            algorithm = 'port',weights=n3,
                            start=list(z=startz,w=startw,a=0.01)),warning=function(w) return(w),error=function(w) return(w))
      
    }
    bic=BIC(nlsfit1)
    
    if (length(ggplot_build(plot.g)[[1]][[2]])>0){
      plot=ggplot_build(plot.g)
      stats=list(fit=nlsfit1,bic=bic)
    }else{
      stats=plot.df
    }}else{
      stats=plot.df
    }
  info_max_vett_new[[i]]=stats
  info_max_vett_new_plot[[i]]=plot
}
names(info_max_vett_new)<-strain.names
names(info_max_vett_new_plot)<-strain.names

save(info_max_vett_new,info_max_vett_new_plot,file=paste(Rds.outputfiles,'strain_thomas_data.Rdata',sep=''))
#load(file=paste(Rds.outputfiles,'strain_thomas_data.Rdata',sep=''))


#### e) Run the Thomas curve stats on all the ecotypes -----
info_max_vett_ecotype_new<-list()
info_max_vett_ecotype_plot<-list()
for (i in 1:length(ecotypes.rename)){
  use<-which(picosubset$ecotype %in% ecotypes.rename[i])
  y=picosubset[use,grep('T',colnames(picosubset))]
  x=as.numeric(gsub('T_','',colnames(picosubset)[grep('T',colnames(picosubset))]))
  n=picosubset.n[use,grep('T',colnames(picosubset.n))]
  y2=matrix(as.numeric(as.matrix(y)),nrow=nrow(y),ncol=ncol(y))
  x2=matrix(rep(x),nrow=nrow(y2),ncol=ncol(y2),byrow=T)
  n2=matrix(as.numeric(as.matrix(n)),nrow=nrow(y),ncol=ncol(y))
  y3=as.vector(y2)
  x3=as.vector(x2)
  n3=as.vector(n2)
  
  plot.df<-data.frame(x=x3,y=y3,n=n3)
  # if (length(y3)>ncol(picop2)){
  startz=(x3[which(y3==max(y3,na.rm=T))][1])-2
  startw=x3[which(y3==tail(y3[!is.na(y3)],1))[length(which(y3==tail(y3[!is.na(y3)],1)))]]-10
  startw=startw-10
  plot.g<-ggplot(data=plot.df,aes(x=x,y=y))+
    geom_point()+
    stat_smooth(method='nls',formula=y~a*exp(x*0.0631)*(1-((x-z)/(w/2))^2),
                method.args = list(start=c(a=0.01,z=startz,w=startw)),se=F)
  
  
  nlsfit1<-tryCatch(nls2(y3~(a*exp(x3*0.0631)*(1-((x3-z)/(w/2))^2)),
                         algorithm = 'port',weights=n3,
                         start=list(z=startz,w=startw,a=0.01)),warning=function(w) return(w),error=function(w) return(w))
  
  # control=list(eval.max=5000, iter.max=1000,warnOnly=T),
  # lower=list(z=-2,w=5,b=0.00001,a=0.00001),upper=list(z=40,w=35,b=Inf,a=Inf)),
  if ((is(nlsfit1,'warning')) | (is(nlsfit1,'error'))){
    
    nlsfit1<-tryCatch(nls(y3~(a*exp(x3*0.0631)*(1-((x3-z)/(w/2))^2)),
                          algorithm = 'port',weights=n3,
                          start=list(z=startz,w=startw,a=0.01)),warning=function(w) return(w),error=function(w) return(w))
    
  }
  bic=BIC(nlsfit1)
  
  if (length(ggplot_build(plot.g)[[1]][[2]])>0){
    plot=ggplot_build(plot.g)
    stats=list(fit=nlsfit1,bic=bic)
  }else{
    stats=plot.df
  }
  info_max_vett_ecotype_new[[i]]=stats
  info_max_vett_ecotype_plot[[i]]=plot
  
}
names(info_max_vett_ecotype_new)<-ecotypes.rename
names(info_max_vett_ecotype_plot)<-ecotypes.rename

save(info_max_vett_ecotype_new,info_max_vett_ecotype_plot,file=paste(Rds.outputfiles,'ecotype_thomas_data.Rdata',sep=''))


#### f) Run the Gaussian curve stats on all the strains ----

plall_out_new <- list()
total<-length(info_max_vett_new)

for (i in 1:total){
  if (length(info_max_vett_new[[i]][[1]])==2){
    use=strain.names[i]
    n.1<-picosubset.n[which(picosubset.n$`Our Naming`==use),grep('T_',colnames(picosubset.n))]
    x.use<-info_max_vett_new[[i]]$data[[1]]$x
    y.use<-info_max_vett_new[[i]]$data[[1]]$y
    n.use<-as.numeric(as.vector(as.matrix(n.1)))
    
    plot.df<-data.frame(x=x.use,y=y.use,n=n.use)
    # if (length(y3)>ncol(picop2)){
    startmu<-mean(x3[y3>0],na.rm=T)
    startsigma<-sd(x3[y3>0],na.rm=T)
    
    # plot.g<-ggplot(data=plot.df,aes(x=x,y=y))+
    #   geom_point()+
    #   stat_smooth(method='nls',formula=y~(a*exp(-1/2*(x-mu)^2/sig^2)),
    #               method.args = list(start=c(a=max(plot.df$y,na.rm=T),mu=startmu,sig=startsigma)),se=F)
    nls.output<-with(plot.df,nls2(y~(a*exp(-1/2*(x-mu)^2/sig^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
                                  algorithm = 'port', weights=n,
                                  control=list(maxiter=5000,eval.max=5000),
                                  start=list(a=max(plot.df$y,na.rm=T),sig=startsigma,mu=startmu)))
    
    xx.pred=with(plot.df,seq(min(x[!is.na(y)],na.rm=T),max(x[!is.na(y)],na.rm=T),0.5))
    pred_nls<-predictNLS(nls.output, newdata = data.frame(x = xx.pred),interval='confidence',alpha=0.05)
    sum<-summary(nls.output)
    cof<-sum$coefficients
    SE<-cof[,'Std. Error']
    nfit=length(predict(nls.output))
    sduse<-SE*sqrt(nfit)
    mufit=coef(nls.output)['mu']
    sigmafit=coef(nls.output)['sig']
    tt=seq(-2,40,by=0.1)
    ft2=(coef(nls.output)['a']*exp(-1/2*(tt-coef(nls.output)['mu'])^2/coef(nls.output)['sig']^2))
    topt<-tt[which(ft2==max(ft2,na.rm=T))]
    mean.t<-sum(tt[tt>=min(xx.pred) & tt<=max(xx.pred)]*ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])/sum(ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])
    width<-sqrt(sum(((tt[tt>=min(xx.pred) & tt<=max(xx.pred)]-mean.t)^2)*ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])/sum(ft2[tt>=min(xx.pred) & tt<=max(xx.pred)]))*2
    w_ci=1.96*SE['sig']
    t_ci=1.96*SE['mu']
    BICgaus<-BIC(nls.output)
    results<-list(raw.data=plot.df,
                  full.pred=cbind(tt,ft2),
                  topt=topt,
                  mu=mean.t,
                  width=width,
                  CI_t=t_ci,
                  CI_w=w_ci,
                  BIC=BICgaus,
                  rmse=sum$sigma,
                  pred_ci=pred_nls$summary[,c('Prop.Mean.1','Prop.2.5%','Prop.97.5%')],
                  cof.output=cof,
                  range=xx.pred)
    
    if (length(nls.output)>0){
      stats=results
    }else{
      stats=plot.df
    }}else{
      stats=info_max_vett_new[[i]]
    }
  plall_out_new[[i]]=stats
  
}
names(plall_out_new)<-strain.names

save(plall_out_new,file=paste(Rds.outputfiles,'strain_gaus_data.Rdata',sep=''))


#### g) Plot gaussian strains ----
plall.out.2<-plall_out_new

total=length(strain.names)
n.row<-ceiling(total/round(sqrt(total)))
n.col<-round(sqrt(total))
plot.mat<-matrix(c(seq(1:total),rep(NA,(n.row*n.col)-(length(seq(1:total))))),nrow=n.row,byrow=T)

has.vals<-which(unlist(lapply(plall.out.2,function(x){length(x)}))>3)
no.vals<-which(unlist(lapply(plall.out.2,function(x){length(x)}))==3)

plall.order<-list()

plot.all<-lapply(1:length(plall.out.2),function(i){
  use.i<-c(has.vals,no.vals)[i]
  if (use.i %in% has.vals){
    preds <- data.frame(x = plall.out.2[[use.i]]$range)
    preds$mean <- plall.out.2[[use.i]]$pred_ci$Prop.Mean.1
    preds$lcl <- plall.out.2[[use.i]]$pred_ci$`Prop.2.5%`
    preds$ucl <- plall.out.2[[use.i]]$pred_ci$`Prop.97.5%`
    
    plall.order<-ggplot()+
      geom_point(aes(x=plall.out.2[[use.i]]$raw.data$x,
                     y=plall.out.2[[use.i]]$raw.data$y))+
      geom_line(data = preds, aes(x = x, y = mean), color= "black") +
      geom_ribbon(data = preds,
                  aes(x = x, y = mean, ymin = lcl, ymax = ucl),
                  color= "NA", alpha = .2)+
      coord_cartesian(ylim=c(0,1.5))
    
    
    #mu.pm<-abs(diff(c(plall.out.2[[use.i]]$mu,plall.out.2[[use.i]]$CI_t[1])))
    #w.pm<-abs(diff(c(plall.out.2[[use.i]]$width,plall.out.2[[use.i]]$CI_w[1])))
    
    grob <- grobTree(textGrob(bquote(RMSE==.(signif(plall.out.2[[use.i]]$rmse,2))), x=0,  y=0.9, hjust=-0.1,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
    grob1<- grobTree(textGrob(bquote(T[opt]==.(signif(plall.out.2[[use.i]]$mu,4))%+-%.(signif(plall.out.2[[use.i]]$CI_t,2))), x=0,  y=0.9-0.1, hjust=-0.1,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
    grob2 <- grobTree(textGrob(bquote(W==.(signif(plall.out.2[[use.i]]$width*2,4))%+-%.(signif(plall.out.2[[use.i]]$CI_w,2))), x=0,  y=0.9-0.2, hjust=-0.1,
                               gp=gpar(col="black", fontsize=9, fontface="italic")))
    plall.order<- plall.order+annotation_custom(grob)+annotation_custom(grob1)+annotation_custom(grob2)
  }else{
    plall.order<-ggplot()+
      geom_point(aes(x=plall.out.2[[use.i]]$x,
                     y=plall.out.2[[use.i]]$y))+
      coord_cartesian(ylim=c(0,1.5))
  }
  plall.order<-plall.order+theme_bw()+theme(
    plot.margin = unit(c(0.01,0,0, 0), "cm"),
    plot.title = element_text(hjust=0.5,face='bold'),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())+
    ylab('')+
    xlab('')+
    labs(title=strain.names[use.i],
         tag=paste(letters[i],'.',sep=''))
  x.plot<-plot.mat[n.row,]
  if (all(!is.na(x.plot))){
    x.plot<-x.plot
  }else{
    x.plot[is.na(x.plot)]=plot.mat[n.row-1,is.na(plot.mat[n.row,])]
  }
  if (i %in% plot.mat[,1]){
    plall.order<-plall.order+theme(
      axis.text.y = element_text(),
      axis.ticks.y = element_line())+
      ylab(bquote(Growth~Rate~(day^-1)))
  }
  if (i %in% x.plot){
    plall.order<-plall.order+theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line())+
      xlab(bquote(Temperature~(degree*C)))
  }
  plall.order<-plall.order+
    theme(text=element_text(size=12),
          axis.text = element_text(size=10),
          plot.margin = margin(2,1,0,2),
          plot.tag.position = c(0.22,0.95),
          plot.tag = element_text(face='bold'))
  return(plall.order)
})
new.plots<-lapply(plot.all,function(x){
  a=ggplotGrob(x);
  a$widths=ggplotGrob(plot.all[[plot.mat[n.row,1]]])$widths;
  a$heights=ggplotGrob(plot.all[[plot.mat[n.row,1]]])$heights;
  return(a)})


all.strains<-arrangeGrob(grobs=new.plots)

save(all.strains,file=paste(Rds.outputfiles,'strain_gaus_plot.Rdata',sep=''))

ggsave('~/Documents/all_strains_final.svg',all.strains,width=10,height=9)
ggsave('~/Documents/all_strains_final.pdf',all.strains,width=7.25,height=8)

#### h) Run the gaussian curve statistics on ecotypes ----


ecotype<-c('HL-I (Low B/A)','HL-II (Low B/A)','LL-IV (High B/A)','LL-I (High B/A)')
e.names<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')
plall_out_ecotype_new <- list()
eco.total=4
for (i in 1:eco.total){
  use=picosub.light.lvl$`Ecotype/Group/Clade` %in% ecotype[i]
  n.1<-picosub.light.lvl.n[use,grep('T_',colnames(picosub.light.lvl))]
  x.use<-info_max_vett_ecotype_plot[[i]]$data[[1]]$x
  y.use<-info_max_vett_ecotype_plot[[i]]$data[[1]]$y
  n.use<-as.numeric(as.vector(as.matrix(n.1)))
  
  plot.df<-data.frame(x=x.use,y=y.use,n=n.use)
  # if (length(y3)>ncol(picop2)){
  startmu<-mean(x3[y3>0],na.rm=T)
  startsigma<-sd(x3[y3>0],na.rm=T)
  
  # plot.g<-ggplot(data=plot.df,aes(x=x,y=y))+
  #   geom_point()+
  #   stat_smooth(method='nls',formula=y~(a*exp(-1/2*(x-mu)^2/sig^2)),
  #               method.args = list(start=c(a=max(plot.df$y,na.rm=T),mu=startmu,sig=startsigma)),se=F)
  nls.output<-with(plot.df,nls2(y~(a*exp(-1/2*(x-mu)^2/sig^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
                                algorithm = 'port', weights=n,
                                control=list(maxiter=5000,eval.max=5000),
                                start=list(a=max(plot.df$y,na.rm=T),sig=startsigma,mu=startmu)))
  
  xx.pred=with(plot.df,seq(min(x[!is.na(y)],na.rm=T),max(x[!is.na(y)],na.rm=T),0.5))
  pred_nls<-predictNLS(nls.output, newdata = data.frame(x = xx.pred),interval='confidence',alpha=0.05)
  sum<-summary(nls.output)
  cof<-sum$coefficients
  SE<-cof[,'Std. Error']
  nfit=length(predict(nls.output))
  sduse<-SE*sqrt(nfit)
  mufit=coef(nls.output)['mu']
  sigmafit=coef(nls.output)['sig']
  tt=seq(-2,40,by=0.1)
  ft2=(coef(nls.output)['a']*exp(-1/2*(tt-coef(nls.output)['mu'])^2/coef(nls.output)['sig']^2))
  topt<-tt[which(ft2==max(ft2,na.rm=T))]
  mean.t<-sum(tt[tt>=min(xx.pred) & tt<=max(xx.pred)]*ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])/sum(ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])
  width<-sqrt(sum(((tt[tt>=min(xx.pred) & tt<=max(xx.pred)]-mean.t)^2)*ft2[tt>=min(xx.pred) & tt<=max(xx.pred)])/sum(ft2[tt>=min(xx.pred) & tt<=max(xx.pred)]))*2
  w_ci=1.96*SE['sig']
  t_ci=1.96*SE['mu']
  BICgaus<-BIC(nls.output)
  results<-list(raw.data=plot.df,
                full.pred=cbind(tt,ft2),
                topt=topt,
                mu=mean.t,
                width=width,
                CI_t=t_ci,
                CI_w=w_ci,
                BIC=BICgaus,
                rmse=sum$sigma,
                pred_ci=pred_nls$summary[,c('Prop.Mean.1','Prop.2.5%','Prop.97.5%')],
                cof.output=cof,
                range=xx.pred)
  
  if (length(nls.output)>0){
    stats=results
  }else{
    stats=plot.df
  }
  plall_out_ecotype_new[[i]]=stats
  
  
} 
names(plall_out_ecotype_new)<-e.names

save(plall_out_ecotype_new,file=paste(Rds.outputfiles,'ecotype_gaus_stats.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_gaus_stats.Rdata',sep=''))

#### i) Plot the ecotype gaussian curves ----





plot.mat<-matrix(1:4,nrow=2,byrow=T)
n.row=2
plot.all_ecotype<-lapply(1:length(plall_out_ecotype_new),function(i){
  
  preds <- data.frame(x = plall_out_ecotype_new[[i]]$range)
  preds$mean <- plall_out_ecotype_new[[i]]$pred_ci$Prop.Mean.1
  preds$lcl <- plall_out_ecotype_new[[i]]$pred_ci$`Prop.2.5%`
  preds$ucl <- plall_out_ecotype_new[[i]]$pred_ci$`Prop.97.5%`
  
  plall.order<-ggplot()+
    geom_point(aes(x=plall_out_ecotype_new[[i]]$raw.data$x,
                   y=plall_out_ecotype_new[[i]]$raw.data$y))+
    geom_line(data = preds, aes(x = x, y = mean), color= "black") +
    geom_ribbon(data = preds,
                aes(x = x, y = mean, ymin = lcl, ymax = ucl),
                color= "NA", alpha = .2)+
    coord_cartesian(ylim=c(0,0.65))
  
  
  #mu.pm<-abs(diff(c(plall.out.2[[use.i]]$mu,plall.out.2[[use.i]]$CI_t[1])))
  #w.pm<-abs(diff(c(plall.out.2[[use.i]]$width,plall.out.2[[use.i]]$CI_w[1])))
  
  grob <- grobTree(textGrob(bquote(RMSE==.(signif(plall_out_ecotype_new[[i]]$rmse,2))), x=0,  y=0.9, hjust=-0.1,
                            gp=gpar(col="black", fontsize=9, fontface="italic")))
  grob1<- grobTree(textGrob(bquote(T[opt]==.(signif(plall_out_ecotype_new[[i]]$mu,4))%+-%.(signif(plall_out_ecotype_new[[i]]$CI_t,2))), x=0,  y=0.9-0.1, hjust=-0.1,
                            gp=gpar(col="black", fontsize=9, fontface="italic")))
  grob2 <- grobTree(textGrob(bquote(W==.(signif(plall_out_ecotype_new[[i]]$width*2,4))%+-%.(signif(plall_out_ecotype_new[[i]]$CI_w,2))), x=0,  y=0.9-0.2, hjust=-0.1,
                             gp=gpar(col="black", fontsize=9, fontface="italic")))
  plall.order<- plall.order+annotation_custom(grob)+annotation_custom(grob1)+annotation_custom(grob2)
  
  plall.order<-plall.order+theme_bw()+theme(
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    plot.title = element_text(hjust=0.5,face='bold'),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag.position = c(0.2,0.96),
    plot.tag = element_text(face='bold'))+
    ylab('')+
    xlab('')+
    labs(title=e.names[i],
         tag=paste(letters[i],'.',sep=''))
  x.plot<-plot.mat[n.row,]
  if (all(!is.na(x.plot))){
    x.plot<-x.plot
  }else{
    x.plot[is.na(x.plot)]=plot.mat[n.row-1,is.na(plot.mat[n.row,])]
  }
  if (i %in% plot.mat[,1]){
    plall.order<-plall.order+theme(
      axis.text.y = element_text(size=10,color='black'),
      axis.ticks.y = element_line(color='black'))+
      ylab(bquote(Growth~Rate~(day^-1)))
  }
  if (i %in% x.plot){
    plall.order<-plall.order+theme(
      axis.text.x = element_text(size=10,color='black'),
      axis.ticks.x = element_line(color='black'))+
      xlab(bquote(Temperature~(degree*C)))
  }
  plall.order<-plall.order+
    theme(text=element_text(size=12,colour = 'black'))
  return(plall.order)
})
new.plots.ecotype<-lapply(plot.all_ecotype,function(x){
  a=ggplotGrob(x);
  a$widths=ggplotGrob(plot.all_ecotype[[3]])$widths;
  a$heights=ggplotGrob(plot.all_ecotype[[3]])$heights
  return(a)})


all.eco<-grid.arrange(grobs=new.plots.ecotype)

save(all.eco,file=paste(Rds.outputfiles,'ecotype_gaus_plot.Rdata',sep=''))


ggsave('~/Documents/all_ecotypes_final.svg',all.eco,width=7.25,height=7.25)
ggsave('~/Documents/all_ecotypes_final.pdf',all.eco,width=7.54,height=6.5)


######## Realized Niche ###########
#### a) Load in the data ####
vals<-read_excel_allsheets(paste(datasets,'Zinser Cruise QPCR Data.xlsx',sep=''))
rawdata<-vals$`all depths`
colnames(rawdata)[colnames(rawdata)=='Temp. (C)']='Temp'
mldmeans<-vals$`Mixed layer mean`
colnames(rawdata)[colnames(rawdata)=='Temp. (C)']='Temp'
onlymldraw<-rawdata #rawdata[rawdata$Depth<=rawdata$`Mixed Layer Depth`,]

open<-nc_open('~/Documents/PhD_Work/Research/Temp_Growth_Rate_Research/Data_Sets/hot_bats_2014.netcdf')
ecotype<-ncvar_get(open,'ecotype')
site<-ncvar_get(open,'site')
lat<-as.numeric(ncvar_get(open,'lat'))
lon<-as.numeric(ncvar_get(open,'lon'))
date<-ncvar_get(open,'date')
yrday<-ncvar_get(open,'yrday_gmt')
temp<-as.numeric(ncvar_get(open,'temp'))
sal<-as.numeric(ncvar_get(open,'sal'))
depth<-as.numeric(ncvar_get(open,'depth'))
abun<-as.numeric(ncvar_get(open,'abundance'))
##Combine all the columns
chb_2014=cbind(ecotype,site)
chb_2014=cbind(chb_2014,lat)
chb_2014=cbind(chb_2014,lon)
chb_2014=cbind(chb_2014,date)
chb_2014=cbind(chb_2014,temp)
chb_2014=cbind(chb_2014,sal)
chb_2014=cbind(chb_2014,depth)
chb_2014=cbind(chb_2014,abun)
chb_2014=cbind(chb_2014,yrday)

##Create a dataframe about the data
chb_2014=data.frame(chb_2014,stringsAsFactors = F)
chb_2014$ptime=as.numeric(as.POSIXct(chb_2014$date,origin='1970-1-1',format='%D'))
chb_2014$temp=as.numeric(chb_2014$temp)
chb_2014$lat=as.numeric(chb_2014$lat)
chb_2014$lon=as.numeric(chb_2014$lon)
chb_2014$depth=as.numeric(chb_2014$depth)
chb_2014$sal=as.numeric(chb_2014$sal)
chb_2014$abun=as.numeric(chb_2014$abun)
chb_2014$loga=log10(chb_2014$abun+1)

#### b) Filter dataset----
#onlymldraw<-subset(onlymldraw,!(eMED4<=0.65 | eMIT9312<=0.65 | eNATL2A<=0.65 | eMIT9313<=0.65))
threshold=0.65
onlymldraw<-onlymldraw[2:nrow(onlymldraw),]
onlymldraw<-onlymldraw %>% 
  drop_na(Cruise)
for (i in 1:length(onlymldraw$Cruise)){
  cruisenumval<-which((unique(onlymldraw$Cruise)) %in% onlymldraw$Cruise[i])
  onlymldraw$cruisenum[i]<-cruisenumval
}


onlymldraw<-onlymldraw %>%
  #filter(Depth <= `Mixed Layer Depth`) %>%
  gather(Ecotype,Abundance,c(eMED4,eMIT9312,eMIT9313,eNATL2A)) %>%
  mutate(Abundance = replace(Abundance, Abundance <= threshold, 0)) %>%
  spread(Ecotype,Abundance)

onlymldraw=onlymldraw[,names(unlist(apply(onlymldraw,2,function(x){which(sum(!is.na(x))>0)})))]

hot<-chb_2014 %>%
  filter(site=='HOT') %>%
  mutate(abun = replace(abun, abun <= threshold, NA)) %>%
  #filter(depth<=100) %>%
  spread(ecotype,abun)
colnames(hot)<-c('Cruise','Lat.','Long.','GMT','Temp',
                 'Salinity','Depth','JD','ptime','loga',
                 'eMED4','eMIT9312','eMIT9313','eNATL2A','SS120')
hot<-hot %>%
  mutate(cruisenum=max(onlymldraw$cruisenum)+1)


onlymldraw<-rbind.data.frame(onlymldraw[,which(colnames(onlymldraw)%in%colnames(hot))],hot[,which(colnames(hot)%in%colnames(onlymldraw))])
onlymldraw<-onlymldraw[onlymldraw$Depth<=200,]
# save(onlymldraw,file=paste(Rds.outputfiles,'raw_abundance_filtered.Rdata',sep=''))
# load(file=paste(Rds.outputfiles,'raw_abundance_filtered.Rdata',sep=''))

#### c) Plot the rainbow trees ----
# f<-onlymldraw %>%
#   gather(Ecotype,Abundance,c(eMED4,eMIT9312,eMIT9313,eNATL2A))
# cols.r<-c('red','blue','green','cyan')
# b<-ggplot()+
#   geom_point(data=f,aes(x=Depth, y=Abundance,color=Ecotype),alpha=0.5)+
#   stat_smooth(data=f,aes(x=Depth,y=Abundance,color=Ecotype),method = lm, formula = y ~ poly(x,3))+
#   scale_y_continuous(trans='log10',name=bquote(log[10]~Abundance~(cells~mL^-1)),
#                      breaks=as.numeric(paste(1,'E',seq(0,6,by=1),sep='')),
#                      labels=c(seq(0,6,by=1)),
#                      limits=c(1,1E6),
#                      position='right')+
#   coord_flip()+
#   scale_x_reverse(limits=c(200,0),name='Depth (m)')+
#   scale_color_manual(values=cols.r)+
#   theme(legend.key = element_rect(fill = "white",color=NA),
#         legend.background = element_rect(fill = "white",color=NA),
#         legend.position = 'bottom')
# #print(b)
# ggsave(paste(newfigs,fig_name,'_',threshold,'_rainbowtrees.pdf',sep=''),b)

#### d) Plot the gaussian curve fit for the realized niche----
fieldnames<-which(colnames(onlymldraw) %in% c('eMED4','eMIT9312','eNATL2A','eMIT9313'))
fieldind<-colnames(onlymldraw)[which(colnames(onlymldraw) %in% c('eMED4','eMIT9312','eNATL2A','eMIT9313'))]

field_ecotype<-list()
for (i in 1:4){
  x3=onlymldraw$Temp
  y3=log10(onlymldraw[,fieldind[i]]+1)
  
  startmu<-mean((x3[!is.na(y3)]),na.rm=T)
  startsigma<-sd((x3[!is.na(y3)]),na.rm=T)
  df_real<-data.frame(x=x3,y=y3)
  nlsfitgaus<-tryCatch(nls(y3~(a*exp(-1/2*(x3-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
                           algorithm = 'port',
                           control=list(maxiter=5000,eval.max=5000),
                           start=list(a=max(y3,na.rm=T)-1,sigma=startsigma,mu=startmu),
                           upper=list(mu=startmu+1,sigma=20)),
                       warning=function(w) return(w), error=function(e) print('TryCatch'))
  if (all(nlsfitgaus=='TryCatch')){
    nlsfitgaus<-tryCatch(nls(y3~(a*exp(-1/2*(x3-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
                             algorithm = 'port',
                             control=list(maxiter=5000,eval.max=5000),
                             start=list(a=max(y3,na.rm=T)-1,sigma=startsigma,mu=startmu+1),
                             upper=list(mu=startmu+1,sigma=30)),
                         warning=function(w) return(w), error=function(e) print('TryCatch'))
    
  }
  ss<-c(floor(range(df_real$x,na.rm=T)[1]),ceiling(range(df_real$x,na.rm=T)[2]))
  xx=seq(ss[1],ss[2],by=0.5)
  pred_nls<-predictNLS(nlsfitgaus, newdata = data.frame(x3 = xx),interval='confidence',alpha=0.05)
  df2<-data.frame(x=xx,y=pred_nls$summary$Prop.Mean.1)
  df2=df2[df2$x >=ss[1] & df2$x<=ss[2] ,]
  
  sum<-summary(nlsfitgaus)
  cof<-sum$coefficients
  SE<-cof[,'Std. Error']
  nfit=length(predict(nlsfitgaus))
  sduse<-SE*sqrt(nfit)
  mufit=coef(nlsfitgaus)['mu']
  sigmafit=coef(nlsfitgaus)['sigma']
  mean.test<-sum(df2$x*df2$y)/sum(df2$y)
  max.test<-df2$x[which.max(df2$y)]
  width.test<-sqrt(sum(((df2$x-mean.test)^2)*df2$y)/sum(df2$y))*2
  
  #Create a matrix of curves using each random z and w
  tt=seq(-2,40,by=0.5)
  ft2=(coef(nlsfitgaus)['a']*exp(-1/2*(tt-coef(nlsfitgaus)['mu'])^2/coef(nlsfitgaus)['sigma']^2))
  #ft2[ft2<0]=NA
  #Topt=Max of curve
  topt<-tt[which(ft2==max(ft2,na.rm=T))]
  ###CI for W
  w_ci=1.96*SE['sigma']
  t_ci=1.96*SE['mu']
  #GOF
  BICgaus<-BIC(nlsfitgaus)
  #What+1.96*SEw upper
  results<-list(mu=mean.test,sig=width.test,sigma=sigmafit,rmse=sum$sigma,topt=topt,w_ci=w_ci,t_ci=t_ci,pred_ci=pred_nls$summary,xx2=xx,pred=ft2,sum=sum,cof=cof,x=x3,y=y3,BIC=BICgaus)
  field_ecotype[[i]]<-results
}
names(field_ecotype)<-fieldind[1:4]
save(field_ecotype,file=paste(Rds.outputfiles,'ecotype_real_results.Rdata',sep=''))


tot=4
n.row<-round(tot/round(sqrt(tot)))
n.col<-round(sqrt(tot))
plot.mat<-matrix(seq(1:tot),nrow=n.row)
plot.mat<-t(plot.mat)



pf_out2<-field_ecotype

plitlist_f<-lapply(1:4,function(i){
  preds <- data.frame(x = pf_out2[[i]]$xx2)
  preds$mean <- pf_out2[[i]]$pred_ci$Prop.Mean.1
  preds$lcl <- pf_out2[[i]]$pred_ci$`Prop.2.5%`
  preds$ucl <- pf_out2[[i]]$pred_ci$`Prop.97.5%`
  plot.weight<-ggplot()+
    geom_point(aes(x=pf_out2[[i]]$x,y=pf_out2[[i]]$y),alpha=0.1)+
    geom_line(data=preds,aes(x=x,y=mean))+
    geom_ribbon(data = preds, 
                aes(x = x, ymin = lcl, ymax = ucl), 
                color= "NA", alpha = .2)+
    #pladd.2[[i]]<-pladd.2[[i]]+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.title = element_text(size=12,color='black'),
          axis.text = element_text(size=12,color='black'),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          plot.title = element_text(hjust=0.5, face='bold'))+
    scale_y_continuous(limits=c(0,6),name=bquote(Log[10]*Abundance~(cells~mL^-1)))+
    scale_x_continuous(name=bquote(Temperature~(degree*C)))
  if (i==1 | i==2){
    plot.weight<-plot.weight+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  if (i==2 | i==4){
    plot.weight<-plot.weight+
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  axis.values<-layer_scales(plot.weight)$y$range$range
  
  grob <- grobTree(textGrob(bquote(RMSE==.(signif(pf_out2[[i]]$rmse,2))), x=0,  y=0.9, hjust=-0.1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  grob1<- grobTree(textGrob(bquote(bar(T)[obs]==.(signif(pf_out2[[i]]$mu,4))%+-%.(signif(pf_out2[[i]]$t_ci,2))), x=0,  y=0.9-0.1, hjust=-0.1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  grob2 <- grobTree(textGrob(bquote(bar(W)==.(signif(pf_out2[[i]]$sigma,4))%+-%.(signif(pf_out2[[i]]$w_ci,2))), x=0,  y=0.9-0.2, hjust=-0.1,
                             gp=gpar(col="black", fontsize=11, fontface="italic")))
  plot.weight<- plot.weight+annotation_custom(grob)+annotation_custom(grob1)+annotation_custom(grob2)
  
  
  plot.weight<-plot.weight+labs(title=names(pf_out2)[i],tag=paste(letters[i],'.',sep=''))+
    theme(plot.title = element_text(hjust = 0.5,vjust=0),
          plot.tag.position = c(0.18,0.97),
          plot.tag = element_text(face='bold'),
          plot.margin = margin(0,0,0,0),
          text=element_text(family='sans'))+
    coord_cartesian(xlim=c(0,35))
  return(plot.weight)
})



##Fix plots so they are the same size
new.plots.real<-lapply(plitlist_f,function(x){
  a=ggplotGrob(x);
  a$widths=ggplotGrob(plitlist_f[[3]])$widths;
  a$heights=ggplotGrob(plitlist_f[[3]])$heights
  return(a)})

ga.real<-grid.arrange(grobs=new.plots.real)


save(ga.real,file=paste(Rds.outputfiles,'ecotype_gaus_plot.Rdata',sep=''))

ggsave('~/Documents/real_final.svg',ga.real,width=10,height=8)
ggsave('~/Documents/real_final_200.pdf',ga.real,width=7.25,height=7,units='in')

#### e) Split the data into CBs ----
#load(file=paste(outputfiles,'realized_gaus_CBs','.Rdata',sep=''))

nag<-onlymldraw %>%
  filter(Long. >=-100 & Long.<=50) %>%
  filter(Lat.>=0) %>%
  mutate(CB = 'NA')
sag<-onlymldraw %>%
  filter(Long. >=-100 & Long.<=50) %>%
  filter(Lat.<0)  %>%
  mutate(CB = 'SA')
npg<-onlymldraw %>%
  filter(Long. <=-100 | Long.>=100) %>%
  filter(Lat.>=0) %>%
  mutate(CB = 'NP')
spg<-onlymldraw %>%
  filter(Long. <=-100 | Long.>=100)%>%
  filter(Lat.<0) %>%
  mutate(CB = 'SP')


cbgrouped<-rbind(nag,sag)
cbgrouped<-rbind(cbgrouped,npg)
cbgrouped<-rbind(cbgrouped,spg)
cbs_gaus_output<-list()
cbsind_out<-list()
#xx=seq(-2,40,by=0.5)
for (i in 1:4){
  var<-seq(1,length(unique(cbgrouped$CB)),1)
  for (j in 1:length(var)){
    cb.use<-unique(cbgrouped$CB)[j]
    x<-cbgrouped$Temp[cbgrouped$CB==cb.use]
    y<-log10(cbgrouped[cbgrouped$CB==cb.use,fieldind[i]]+1)
    
    startmu<-mean((x[!is.na(y)]),na.rm=T)
    startsigma<-sd((x[!is.na(y)]),na.rm=T)
    
    nlsfitgaus<-tryCatch(nls(y~(a*exp(-1/2*(x-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
                             algorithm = 'port',
                             control=list(maxiter=5000,eval.max=5000),
                             start=list(a=max(df2$y,na.rm=T)-1,sigma=startsigma,mu=startmu),
                             upper=list(mu=startmu+1,sigma=20)),
                         warning=function(w) return(w))
    #   if (all(nlsfitgaus=='TryCatch')){
    #     nlsfitgaus<-tryCatch(nls(y3~(a*exp(-1/2*(x3-mu)^2/sigma^2)), #(a*exp(-(x3-c)^2/(2*b^2))),
    #                              algorithm = 'port',
    #                              control=list(maxiter=5000,eval.max=5000),
    #                              start=list(a=max(y3,na.rm=T)-1,sigma=startsigma,mu=startmu+1),
    #                              upper=list(mu=startmu+1,sigma=12)),
    #                          warning=function(w) return(w), error=function(e) print('TryCatch'))
    #     
    #   }
    #   
    ss<-c(floor(range(x,na.rm=T)[1]),ceiling(range(x,na.rm=T)[2]))
    xx=seq(ss[1],ss[2],by=1)
    pred_nls<-predictNLS(nlsfitgaus, newdata = data.frame(x = xx),interval='confidence',alpha=0.05)
    df2<-data.frame(x=xx,y=pred_nls$summary$Prop.Mean.1)
    df2=df2[df2$x >=ss[1] & df2$x<=ss[2] ,]
    sum<-summary(nlsfitgaus)
    cof<-sum$coefficients
    SE<-cof[,'Std. Error']
    nfit=length(predict(nlsfitgaus))
    sduse<-SE*sqrt(nfit)
    mufit=coef(nlsfitgaus)['mu']
    sigmafit=coef(nlsfitgaus)['sigma']
    #Create a matrix of curves using each random z and w
    tt=seq(-2,40,by=0.1)
    ft2=(coef(nlsfitgaus)['a']*exp(-1/2*(tt-coef(nlsfitgaus)['mu'])^2/coef(nlsfitgaus)['sigma']^2))
    #ft2[ft2<0]=NA
    #Topt=Max of curve
    topt<-tt[which(ft2==max(ft2,na.rm=T))]
    ###CI for W
    w_ci=1.96*SE['sigma']
    t_ci=1.96*SE['mu']
    #GOF
    
    mean.test<-sum(df2$x*df2$y)/sum(df2$y)
    max.test<-df2$x[which.max(df2$y)]
    width.test<-sqrt(sum(((df2$x-mean.test)^2)*df2$y)/sum(df2$y))*2
    
    BICgaus<-BIC(nlsfitgaus)
    #What+1.96*SEw upper
    
    results<-list(mu=mean.test,w=width.test,sigma=sigmafit,topt=topt,pred_ci=pred_nls$summary,xx2=xx,pred=ft2,sum=sum,cof=cof,w_CI=w_ci,t_CI=t_ci,x=x,y=y,BIC=BICgaus)
    #   return(results)
    cbsind_out[[j]]<-results
  }
  names(cbsind_out)<-unique(cbgrouped$CB)
  cbs_gaus_output[[i]]<-cbsind_out
}
names(cbs_gaus_output)<-fieldind[1:4]


save(cbs_gaus_output,file=paste(Rds.outputfiles,'cbs_gaus_data_named.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'cbs_gaus_data_named.Rdata',sep=''))









#### f) Map the CBs----
#e6e8ed
open2<-nc_open('~/Documents/PhD_Work/Research/Niche_Model/Data_Sets/sst.day.mean.nc')
#summary.ncdf(open2)
tlon<-ncvar_get(open2,'lon')
tlat<-ncvar_get(open2,'lat')
ttime<-ncvar_get(open2,'time')
tsst<-ncvar_get(open2,'sst')

world <- fortify(spTransform(getMap(), CRS("+proj=robin")))
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=12),
                         legend.position = 'bottom',
                         legend.background = element_rect(fill=NA),
                         legend.key = element_rect(fill = "white", colour = "white"),
                         text = element_text(size=22)))
grat <- readOGR("~/Downloads/ne_110m_graticules_all", layer="ne_110m_graticules_15")
grat_df <- fortify(grat)

bbox <- readOGR("~/Downloads/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box")
bbox_df<- fortify(bbox)

grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
prj.coord <- project(cbind(cbgrouped$Long., cbgrouped$Lat.), proj=PROJ)
Y.gr <- cbind(prj.coord, cbgrouped)
names(Y.gr)[1:2] <- c("X.prj","Y.prj")

Y.gr<-Y.gr %>%
  drop_na(Cruise)

group.colors <- c(colorspace::diverge_hcl(7)[c(1:3,7:5)])
group.colors<-gsub('.*#',"#",sort(paste(unique(Y.gr$CB),group.colors)))
solid.colors <- c(colorspace::diverge_hcl(3)[c(1,3)])
solid.colors<-rep(solid.colors,each=3)
solid.colors<-gsub('.*#',"#",sort(paste(unique(Y.gr$CB),solid.colors)))
solid.colors<-c('blue','red','blue','red')
manual.sort<-c('NA','NP','SA','SP')
manual.names<-c("North Atlantic", "North Pacific",
                # "Equatorial Atlantic", "Equatorial Pacific",
                "South Atlantic","South Pacific")
#c(unique(Y.gr$CB))
arr <- c(17,6,17,6)
group.arr<-as.numeric(gsub('.*#',"",sort(paste(unique(Y.gr$CB),arr,sep='#'))))


ggmap.plot<-ggplot(bbox_robin_df, aes(long,lat)) +
  geom_polygon(fill="#e6e8ed",color='black')+
  geom_polygon(data=world, aes(long,lat, group=group),color='black',fill='black') +
  #geom_raster(data = sst.proj,  aes(x = x, y = y, fill = sst),interpolate=T)+  
  geom_path(aes(group=group))+
  # geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  coord_equal() +
  theme_opts +
  scale_fill_manual(values=c("black", "black"), guide="none")+
  geom_point(data=Y.gr,aes(X.prj, Y.prj, shape=CB,color=CB))+
  scale_shape_manual(name='',values = group.arr,
                     breaks = manual.sort,
                     labels = manual.names)+
  scale_color_manual(name='',values = solid.colors,
                     breaks = manual.sort,
                     labels = manual.names)+
  guides(color = guide_legend(override.aes = list(size=4)))+
  theme(legend.text=element_text(size=12),
        legend.justification = 'center',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin = unit(c(0,0,0,0),'cm'))

ggsave("~/Documents/cb_map_final.pdf", ggmap.plot,width=8,height=8, units='in')
ggsave("~/Documents/cb_map_final.svg", ggmap.plot,width=8,height=8, units='in')

#### g) Plot the CB plots -----

total<-4
n.row<-2 #ceiling(total/round(sqrt(total)))
n.col<-2 #round(sqrt(total))
plot.mat<-matrix(c(seq(1:total),rep(NA,(n.row*n.col)-(length(seq(1:total))))),nrow=n.row,byrow=T)
unique(cbgrouped$CB)
titles<-c('North Atlantic','South Atlantic','North Pacific','South Pacific')
pltlist<-list()
pltlist_all<-list()

pltlist<-function(i,j){
  df2<-data.frame(x=cbs_gaus_output[[i]][[j]]$x,y=cbs_gaus_output[[i]][[j]]$y)
  xrange<-c(outer(cbs_gaus_output[[i]][[j]]$mu,cbs_gaus_output[[i]][[j]]$sigma,'-'),outer(cbs_gaus_output[[i]][[j]]$mu,cbs_gaus_output[[i]][[j]]$sigma,'+'))
  pred.x<-seq(floor(range(df2[,1],na.rm=T)[1]),ceiling(range(df[,1],na.rm=T)[2]),by=1)
  preds <- data.frame(x = cbs_gaus_output[[i]][[j]]$xx2)
  preds$mean <- cbs_gaus_output[[i]][[j]]$pred_ci$Prop.Mean.1
  preds$lcl <- cbs_gaus_output[[i]][[j]]$pred_ci$`Prop.2.5%`
  preds$ucl <- cbs_gaus_output[[i]][[j]]$pred_ci$`Prop.97.5%`
  cb.use<-unique(cbgrouped$CB)[j]
  x<-cbgrouped$Temp[cbgrouped$CB==cb.use]
  y<-log10(cbgrouped[cbgrouped$CB==cb.use,fieldind[i]]+1)
  
  cb.one<-ggplot()+
    geom_point(aes(x=x,y=y),
               color='black',alpha=0.1)+
    geom_line(data = preds, aes(x = x, y = mean), color= "black") +
    geom_ribbon(data = preds, aes(x = x, ymin = lcl, ymax = ucl),
                color= "NA", alpha = .2,linetype='dashed')+
    #scale_y_continuous(limits=c(-1,6))+
    #scale_x_continuous(limits=c(0,35))+
    coord_cartesian(xlim=c(0,35),ylim=c(0,6))+
    #geom_line(aes(x=xrange,y=c(0,0)),color='blue')+
    #geom_line(aes(x=c(cbs_gaus_output[[i]][[j]]$mu,cbs_gaus_output[[i]][[j]]$mu),y=c(0,6)),color='red')+
    xlab('')+
    ylab('')+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.line = element_blank(),
          legend.key = element_blank(),
          text = element_text(size=12),
          plot.title = element_text(hjust=0.5,size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=12),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  grob <- grobTree(textGrob(bquote(RMSE==.(signif(cbs_gaus_output[[i]][[j]]$sum$sigma,4))), x=0,  y=0.9, hjust=-0.1,
                            gp=gpar(col="black", fontsize=12, fontface="italic")))
  grob1<- grobTree(textGrob(bquote(bar(T)[obs]==.(signif(cbs_gaus_output[[i]][[j]]$mu,4))%+-%.(signif(cbs_gaus_output[[i]][[j]]$t_CI,2))), x=0,  y=0.9-0.1, hjust=-0.1,
                            gp=gpar(col="black", fontsize=12, fontface="italic")))
  grob2 <- grobTree(textGrob(bquote(bar(W)==.(signif(cbs_gaus_output[[i]][[j]]$sigma*2,4))%+-%.(signif(cbs_gaus_output[[i]][[j]]$w_CI,2))), x=0,  y=0.9-0.2, hjust=-0.1,
                             gp=gpar(col="black", fontsize=12, fontface="italic")))
  cb.one<-cb.one+annotation_custom(grob)+annotation_custom(grob1)+annotation_custom(grob2)
  
  
  x.plot<-plot.mat[n.row,]
  if (all(!is.na(x.plot))){
    x.plot<-x.plot
  }else{
    x.plot[is.na(x.plot)]=plot.mat[n.row-1,is.na(plot.mat[n.row,])]
  }
  if (i*j %in% plot.mat[,1]){
    cb.one<-cb.one+theme(
      axis.text.y = element_text(color='black'),
      axis.ticks.y = element_line(color='black'))+
      ylab(bquote(Log[10]~Abundance~(cells~mL^-1)))
  }
  if (i*j %in% x.plot){
    cb.one<-cb.one+theme(
      axis.text.x = element_text(color='black'),
      axis.ticks.x = element_line(color='black'))+
      xlab(bquote(Temperature~(degree*C)))
  }
  cb.one<-cb.one+labs(title=titles[j],tag=paste(letters[j],'.',sep=''))+
    theme(plot.title = element_text(hjust = 0.5,vjust=0,size=12),
          plot.tag.position = c(0.12,0.97),
          plot.tag = element_text(face='bold'),
          plot.margin = unit(c(0,0.1,0,0),'cm'),
          text=element_text(family='sans'))
  return(cb.one)
  
}
pltlist_all_all<-list()
for (l in 1:4){
  for (k in 1:4){
    pltlist_all[[k]]<-pltlist(i=l,j=k)
  }
  pm<-arrangeGrob(pltlist_all[[1]],pltlist_all[[2]],pltlist_all[[3]],pltlist_all[[4]],top=fieldind[l],nrow=2,ncol=2)
  ggsave(paste('~/Documents/',fieldind[l],'_cbfits_thomas_threshold_',threshold,'_2.pdf',sep=''),pm,width=10,height=8)
  ggsave(paste('~/Documents/',fieldind[l],'_cbfits_thomas_threshold_',threshold,'_2.svg',sep=''),pm,width=10,height=8)
  
  pltlist_all_all[[l]]<-pltlist_all
}

save(pltlist_all_all,file=paste(Rds.outputfiles,'cbs_gaus_plotlist.Rdata',sep=''))



new.plots.real.cb<-list()
for (l in 1:4){
  ##Fix plots so they are the same size
  new.plots.real.cb[[l]]<-lapply(pltlist_all_all[[l]],function(x){
    a=ggplotGrob(x);
    a$widths=ggplotGrob(pltlist_all_all[[l]][[3]])$widths;
    a$heights=ggplotGrob(pltlist_all_all[[l]][[3]])$heights
    return(a)})
  
  pm<-arrangeGrob(grobs=list(new.plots.real.cb[[l]][[1]],
                             new.plots.real.cb[[l]][[2]],
                             new.plots.real.cb[[l]][[3]],
                             new.plots.real.cb[[l]][[4]]),top=fieldind[l],nrow=2,ncol=2)
  ggsave(paste('~/Documents/',fieldind[l],'_cbfits_thomas_threshold_',threshold,'_2.pdf',sep=''),pm,width=7.45,height=7,units='in')
  ggsave(paste('~/Documents/',fieldind[l],'_cbfits_thomas_threshold_',threshold,'_2.svg',sep=''),pm,width=7.45,height=7,units='in')
  
}

#### h) Compare the Thomas and the gaussian curve fits----
smooth.lab.names<-unique(allnames)
smooth.lab.gauss.names<-strain.names
pp<-list()
labnames.1<-rownames(picoagg)[which(!unlist(lapply(info_max_vett,function(x){all(is.na(x))})))][1:8]
labinds<-which(!unlist(lapply(info_max_vett,function(x){all(is.na(x))})))[1:8]

n.row<-round(9/round(sqrt(9)))
n.col<-round(sqrt(9))
plot.mat<-matrix(seq(1:9),nrow=n.row)
plot.mat<-t(plot.mat)
plot.mat<-matrix(c(seq(1,7,1),c(8,6)),nrow=3,byrow=T)

for (i in 1:length(has.vals)){
  y.i<-has.vals[i]
  dfmu<-plall_out_new[[y.i]]$raw.data
  dfga<-data.frame(xpred = plall_out_new[[y.i]]$range,
                   ypred = plall_out_new[[y.i]]$pred$Prop.Mean.1)
  dfmt<-data.frame(
    xmt = info_max_vett_new_plot[[y.i]]$data[[2]]$x,
    ymt = info_max_vett_new_plot[[y.i]]$data[[2]]$y)#name.use.2[i]]])
  
  pp[[i]]<-ggplot()+
    geom_point(data=dfmu,aes(x=x,y=y))+
    geom_line(data=dfga,aes(x=xpred,y=ypred))+
    geom_line(data=dfmt,aes(x=xmt,y=ymt),linetype='dashed')+
    xlab('')+
    ylab('')+
    theme_bw()+
    theme(panel.grid=element_blank(),
          legend.key = element_blank(),
          text = element_text(size=16),
          plot.title = element_text(size = 14, face = "bold",hjust = 0.5,vjust=-1),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(0,r=2,0,0),
          plot.tag.position = c(0.22,0.98),
          plot.tag = element_text(face='bold'))+
    scale_y_continuous(limits=c(0,0.7))+
    labs(title=strain.names[y.i],tag=paste(letters[i],'.',sep=''))
  
  
  if (i %in% plot.mat[,1]){
    pp[[i]]<-pp[[i]]+theme(
      axis.text.y = element_text(size=10,color='black'),
      axis.ticks.y = element_line(color='black'))+
      ylab(bquote(Growth~Rate~(day^-1)))
    
  }
  if (i %in% plot.mat[n.row,]){
    pp[[i]]<-pp[[i]]+theme(
      axis.text.x = element_text(color='black',size=10),
      axis.ticks.x = element_line(color='black'))+
      xlab(bquote(Temperature~(degree*C)))
    
  }
  grob0<-grobTree(textGrob(bquote(BIC[Thomas]==.(signif(info_max_vett_new[[y.i]]$bic,6))), x=0,  y=0.9, hjust=-0.1,
                           gp=gpar(col="black", fontsize=11, fontface="italic")))
  grob1<- grobTree(textGrob(bquote(BIC[Gaussian]==.(signif(plall.out.2[[y.i]]$BIC,6))), x=0,  y=0.9-0.1, hjust=-0.1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  
  pp[[i]]<-pp[[i]]+annotation_custom(grob0)+annotation_custom(grob1)
  
}
new.plots.compare.strain<-lapply(pp,function(x){
  a=ggplotGrob(x);
  a$widths=ggplotGrob(pp[[7]])$widths;
  a$heights=ggplotGrob(pp[[7]])$heights
  return(a)})

ppgrid<-arrangeGrob(grobs=new.plots.compare.strain)
ggsave('~/Documents/thomas_gaus_strain_compare_final.pdf',ppgrid,width=10,height=10,units='in')
ggsave('~/Documents/thomas_gaus_strain_compare_final.svg',ppgrid,width=10,height=10,units='in')


#### i) Ecotype BIC compare-----
pp<-list()
#labnames.1<-rownames(picoagg)[which(!unlist(lapply(info_max_vett,function(x){all(is.na(x))})))][1:7]
labinds<-which(!unlist(lapply(info_max_vett,function(x){all(is.na(x))})))

n.row<-round(4/round(sqrt(4)))
n.col<-round(sqrt(4))
plot.mat<-matrix(seq(1:4),nrow=n.row)
plot.mat<-t(plot.mat)
#plot.mat<-matrix(c(seq(1,7,1),c(5,6)),nrow=3,byrow=T)

for (i in 1:4){
  dfmu<-plall_out_ecotype_new[[i]]$raw.data
  dfga<-data.frame(xpred = plall_out_ecotype_new[[i]]$range,
                   ypred = plall_out_ecotype_new[[i]]$pred_ci$Prop.Mean.1)
  dfmt<-data.frame(
    xmt = info_max_vett_ecotype_plot[[i]]$data[[2]]$x,
    ymt = info_max_vett_ecotype_plot[[i]]$data[[2]]$y)#name.use.2[i]]])
  
  pp[[i]]<-ggplot()+
    geom_point(data=dfmu,aes(x=x,y=y))+
    geom_line(data=dfga,aes(x=xpred,y=ypred))+
    geom_line(data=dfmt,aes(x=xmt,y=ymt),linetype='dashed')+
    xlab('')+
    ylab('')+
    theme_bw()+
    theme(panel.grid=element_blank(),
          legend.key = element_blank(),
          text = element_text(size=16),
          plot.title = element_text(size = 14, face = "bold",hjust = 0.5,vjust=-1),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(0,r=2,0,0),
          title = element_text(size=14),
          plot.tag.position = c(0.13,0.98),
          plot.tag = element_text(face='bold'))+
    scale_y_continuous(limits=c(0,0.7))+
    labs(title=e.names[i],tag=paste(letters[i],'.',sep=''))
  
  
  
  if (i %in% plot.mat[,1]){
    pp[[i]]<-pp[[i]]+theme(
      axis.text.y = element_text(color='black',size=12),
      axis.ticks.y = element_line(color='black'))+
      ylab(bquote(Growth~rate~(day^-1)))
    
  }
  if (i %in% plot.mat[n.row,]){
    pp[[i]]<-pp[[i]]+theme(
      axis.text.x = element_text(size=12,color='black'),
      axis.ticks.x = element_line(color='black'))+
      xlab(bquote(Temperature~(degree*C)))
    
  }
  grob0<-grobTree(textGrob(bquote(BIC[Thomas]==.(signif(info_max_vett_ecotype_new[[i]]$bic,6))), x=0,  y=0.9, hjust=-0.1,
                           gp=gpar(col="black", fontsize=12, fontface="italic")))
  grob1<- grobTree(textGrob(bquote(BIC[Gaussian]==.(signif(plall_out_ecotype_new[[i]]$BIC,6))), x=0,  y=0.9-0.1, hjust=-0.1,
                            gp=gpar(col="black", fontsize=12, fontface="italic")))
  
  pp[[i]]<-pp[[i]]+annotation_custom(grob0)+annotation_custom(grob1)
  
}
new.plots.compare.ecotype<-lapply(pp,function(x){
  a=ggplotGrob(x);
  a$widths=ggplotGrob(pp[[3]])$widths;
  a$heights=ggplotGrob(pp[[3]])$heights
  return(a)})
ppgrid<-arrangeGrob(grobs=new.plots.compare.ecotype)
ggsave('~/Documents/thomas_gaus_ecotype_compare_final.pdf',ppgrid,width=10,height=8)
ggsave('~/Documents/thomas_gaus_ecotype_compare_final.svg',ppgrid,width=10,height=8)

# 
# 


#### j) Gaussian CBS vs. Gaussian Lab -----
colstr<-c('blue','blue','red','red')
fill.col<-c('blue','white','red','white')
arr<-c(24,25,24,25)

gpplt_gaus<-function(i){
  gp.one<-ggplot()+geom_line(aes(x=plall_out_ecotype_new[[i]]$range,y=plall_out_ecotype_new[[i]]$pred_ci$Prop.Mean.1))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=12),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=0),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.right = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y.left=element_blank(),
          plot.tag.position = c(0.09,0.97),
          plot.tag = element_text(face='bold'))+
    labs(title=e.names[i],tag=paste(letters[i],'.',sep=''))+
    coord_cartesian(xlim=c(-2,34))+
    scale_y_continuous(limits=c(0,0.6),
                       breaks=seq(0,0.6,0.1),
                       labels=seq(0,6,1),
                       sec.axis = sec_axis(~.*6/6 , name=bquote(Growth~rate~(Day^-1))))
  
  for (j in 1:4){
    xx<-seq(-2,40,by=0.1)
    if (length(cbs_gaus_output[[i]][[j]]$pred)==0){next
    }else{
      xsp=which(xx>=range(cbgrouped$Temp[cbgrouped$CB==unique(cbgrouped$CB)[j]],na.rm=T)[1] & xx<=range(cbgrouped$Temp[cbgrouped$CB==unique(cbgrouped$CB)[j]],na.rm=T)[2])
      fielddf<-data.frame(x=xx[xsp],y=cbs_gaus_output[[i]][[j]]$pred[xsp])
      gp.one<-gp.one+geom_line(data=fielddf,aes(x=x,y=(y*0.5)/5),color=colstr[j])+
        geom_point(data=fielddf[seq(1,nrow(fielddf),length.out=8),],aes(x=x,y=(y*0.5)/5),color=colstr[j],shape=arr[j],size=3, fill=fill.col[j])
    }}
  if (i>=3){
    gp.one<-gp.one+theme(
      axis.text.x = element_text(size = 12),
      axis.ticks.x = element_line(),
      axis.title.x = element_text())+
      xlab(bquote(Temperature~(degree*C)))
  }
  if (i==1 | i==3){
    gp.one<-gp.one+theme(
      axis.text.y = element_text(size = 12),
      axis.ticks.y   = element_line(),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right   = element_blank(),
      axis.title.y.left = element_text())+
      ylab(bquote(Log[10]~Abundance~(cells~mL^-1)))
    
  }
  if (i==2 | i==4){
    gp.one<-gp.one+theme(
      axis.text.y.right  = element_text(),
      axis.ticks.y.right = element_line(),
      axis.title.y.right = element_text())
  }
  
  return(gp.one)
}
gplist<-list()
for (i in 1:4){
  gplist[[i]]<-gpplt_gaus(i)
}
gplist2<-gplist
new.plots.real.cb.all<-lapply(gplist2,function(x){
  a=ggplotGrob(x);
  #a$widths=ggplotGrob(gplist2[[4]])$widths;
  a$heights=ggplotGrob(gplist2[[3]])$heights
  return(a)})
legend_gp <- get_legend(ggmap.plot+theme(legend.text=element_text(size=12)))
ga.cb<-arrangeGrob(new.plots.real.cb.all[[1]],new.plots.real.cb.all[[2]],new.plots.real.cb.all[[3]],new.plots.real.cb.all[[4]], legend_gp,
                   ncol=3, nrow = 3,
                   layout_matrix = rbind(c(1,NA,2), c(3,NA,4), c(NA,5,NA)),
                   widths = c(2.7,0.01, 3), heights = c(1, 1, 0.2))
ggsave(paste('~/Documents/Gaussian_CB_vs_mu_final.pdf',sep=''),ga.cb,width=7.45,height=7,units='in')
ggsave(paste('~/Documents/Gaussian_CB_vs_mu_final.svg',sep=''),ga.cb,width=7.45,height=7,units='in')


#### k) Mu vs. Sig----
#Gaussian
df.gaussian.fund=data.frame(mean=unlist(lapply(plall_out_ecotype_new,function(x){x$mu})),
                            sig=unlist(lapply(plall_out_ecotype_new,function(x){x$width})),
                            niche=c('Lab','Lab','Lab','Lab'))

df.gaussian.fund$mu_error_U=df.gaussian.fund$mean+unlist(lapply(plall_out_ecotype_new,function(x){x$CI_t}))
df.gaussian.fund$mu_error_L=df.gaussian.fund$mean-unlist(lapply(plall_out_ecotype_new,function(x){x$CI_t}))
df.gaussian.fund$sig_error_U=df.gaussian.fund$sig+unlist(lapply(plall_out_ecotype_new,function(x){x$CI_w}))
df.gaussian.fund$sig_error_L=df.gaussian.fund$sig-unlist(lapply(plall_out_ecotype_new,function(x){x$CI_w}))

for (i in 1:4){
  mean<-unlist(lapply(cbs_gaus_output[[i]],function(x){unlist(x$mu)}))
  sig=unlist(lapply(cbs_gaus_output[[i]],function(x){x$w}))
  mu_error_L=mean-unlist(lapply(cbs_gaus_output[[i]],function(x){x$t_CI}))
  mu_error_U=mean+unlist(lapply(cbs_gaus_output[[i]],function(x){x$t_CI}))
  sig_error_L=sig-unlist(lapply(cbs_gaus_output[[i]],function(x){x$w_CI}))
  sig_error_U=sig+unlist(lapply(cbs_gaus_output[[i]],function(x){x$w_CI}))
  niche=rep('Field',length(mean))
  ecotype=rep(names(cbs_gaus_output)[i],length(mean))
  CB<-names(cbs_gaus_output[[i]]) #titles[unlist(lapply(lapply(cbs_gaus_output[[i]],function(x){unlist(x$mu)}),function(y){(length(y)>0)}))]
  df_do<-data.frame(cbind(mean,sig,mu_error_U,mu_error_L,sig_error_U,sig_error_L,niche,ecotype,CB))
  if (i==1){
    df4.gaus<-df_do
  }else{
    df4.gaus<-rbind(df4.gaus,df_do)
  }
}

df3.gaus<-df.gaussian.fund
df3.gaus$ecotype=rownames(df3.gaus)
df3.gaus$CB<-rep('Lab',4)
df3.gaus<-rbind(df3.gaus,df4.gaus)
for (i in 1:4){
  rr<-nrow(df3.gaus[df3.gaus$niche=='Field' & df3.gaus$ecotype==fieldind[i],])
  if (i==1){
    dff.gaus<-as.data.frame(lapply(df3.gaus[df3.gaus$niche=='Lab'& df3.gaus$ecotype==fieldind[i],],rep,rr),ncol=ncol(df3.gaus))
  }else{
    dd.gaus<-as.data.frame(lapply(df3.gaus[df3.gaus$niche=='Lab'& df3.gaus$ecotype==fieldind[i],],rep,rr),ncol=ncol(df3.gaus))
    dff.gaus<-rbind(dff.gaus,dd.gaus)
  }
}
dff4.gaus<-df4.gaus
dff4.gaus[,1:6]<-apply(dff4.gaus[,1:6],2,function(x){as.numeric(as.vector(x))})
df3.gaus[,c(1,2,4:7)]<-apply(df3.gaus[,c(1,2,4:7)],2,function(x){as.numeric(as.vector(x))})
dff.gaus[,c(1,2,4:7)]<-apply(dff.gaus[,c(1,2,4:7)],2,function(x){as.numeric(as.vector(x))})

#dff4.gaus<-separate(df4.gaus, CB, c('Dir','Oc'))
CB.values<- c(
  'NA' = "North Atlantic",
  'NP' = "North Pacific",
  'SA' = "South Atlantic",
  'SP' = "South Pacific"
)


musig3.gaus<-ggplot(data=dff4.gaus)+
  geom_segment(data=dff4.gaus,aes(x=sig,xend=sig,y=mu_error_L,yend=mu_error_U))+
  geom_segment(data=dff4.gaus,aes(x=sig_error_L,xend=sig_error_U,y=mean,yend=mean))+
  geom_segment(data=dff.gaus,aes(x = sig,y =  mean, xend =  dff4.gaus$sig,yend =  dff4.gaus$mean))+
  geom_point(data=df3.gaus,aes(x=sig,y=mean,shape=factor(CB),color=factor(CB),fill=factor(CB)),size=6)+
  geom_point(data=df3.gaus[df3.gaus$niche=='Lab',],aes(x=sig,y=mean,shape=factor(niche),color=factor(niche)),size=6)+
  scale_x_continuous(name=bquote(Niche~width~(degree*C)))+
  scale_y_continuous(name=bquote(Mean~temperature~(degree*C)))+
  coord_cartesian(ylim=c(min(df3.gaus$mean)-1, max(df3.gaus$mean)+1),xlim=c(min(df3.gaus$sig)-1, max(df3.gaus$sig)+2))+
  #  theme_opts+
  theme_bw(16)+
  scale_shape_manual(name='',
                     values = c('Lab' = 16, 'NA' = 24,
                                'SA'= 25,
                                'NP' = 24,
                                'SP'= 25),
                     labels=CB.values)+
  #theme(legend.position="bottom",legend.box = "vertical")+
  scale_color_manual(name='',values=c(Lab='black',
                                      'NP'='red',
                                      'NA'='blue',
                                      'SP'='red',
                                      'SA'='blue'),
                     labels=CB.values)+
  scale_fill_manual(name='',values=c(Lab='black',
                                     'NP'='red',
                                     'NA'='blue',
                                     'SP'='white',
                                     'SA'='white'),
                    labels=CB.values)+
  theme(aspect.ratio=1)+
  facet_wrap(.~factor(ecotype,levels=c('eMED4','eMIT9312','eNATL2A','eMIT9313')))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="bottom",legend.box = "vertical",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.5, "lines"), 
        panel.spacing.y=unit(0,"lines"))
gaus.musig <- ggplotGrob(musig3.gaus)
strips <- gaus.musig$layout[grep("strip-t", gaus.musig$layout$name), ][c(2,1,4,3),]
titles <- lapply(rev(paste0(letters[seq_len(nrow(strips))], ".")), 
                 function(rr){textGrob(rr, gp=gpar(fontface = 'bold'), x = 0, hjust = 0, vjust = 1)})
gaus.musig <- gtable_add_grob(gaus.musig, grobs = titles, 
                              t = strips$t, b = strips$b, 
                              l = strips$l, r = strips$r)
gaus.musig2<-plot_grid(gaus.musig)

ggsave('~/Desktop/musig_compare.tiff',gaus.musig2,dpi=300,width=8,height=6)

#ggsave(paste('~/Documents/compare_musig_splitup_gaus_final.svg',sep=''),musig3.gaus,width=10,height=8)



#####Raw Values-----
matrix(df3.gaus[df3.gaus$niche=='Field','mean'],ncol=4,byrow=F)-df3.gaus[df3.gaus$niche=='Lab','mean']

diff.mat<-t(t(matrix(df3.gaus[df3.gaus$niche=='Field','mean'],ncol=4,byrow=F))-df3.gaus[df3.gaus$niche=='Lab','mean'])
diff.mat.w<-t(t(matrix(df3.gaus[df3.gaus$niche=='Field','sig'],ncol=4,byrow=F))-df3.gaus[df3.gaus$niche=='Lab','sig'])

diff.mat<-as.data.frame(diff.mat)
colnames(diff.mat)<-e.names
diff.mat$cb<-unique(df3.gaus$CB)[2:5]

diff.mat.w<-as.data.frame(diff.mat.w)
colnames(diff.mat.w)<-e.names
diff.mat.w$cb<-unique(df3.gaus$CB)[2:5]

mean(apply(diff.mat[,1:4],2,mean))
sd(apply(diff.mat[,1:4],2,sd)/sqrt(4))/sqrt(4)
mean(apply(diff.mat.w[,1:4],2,mean))
sd(apply(diff.mat.w[,1:4],2,sd)/sqrt(4))/sqrt(4)



dif.f<-matrix(nrow=4,ncol=4)
dif.w<-matrix(nrow=4,ncol=4)

for (i in 1:4){
  m.u<-plall_out_ecotype_new[[i]]$mu
  val.s<-signif(unlist(lapply(cbs_gaus_output[[i]],function(x){x$mu})),4)
  dif.f[i,]<-val.s-m.u
  w.<-plall_out_ecotype_new[[i]]$width
  val.w<-signif(unlist(lapply(cbs_gaus_output[[i]],function(x){x$w})),4)
  dif.w[i,]<-val.w-w.
}

rownames(dif.f)<-fieldind
colnames(dif.f)<-unique(cbgrouped$CB)
apply(dif.f,2,mean)
apply(dif.f,2,sd)/sqrt(4)
apply(dif.f,1,mean)
apply(dif.f,1,sd)/sqrt(4)

rownames(dif.w)<-fieldind
colnames(dif.w)<-unique(cbgrouped$CB)
apply(dif.w,2,mean)
apply(dif.w,2,sd)/sqrt(4)
apply(dif.w,1,mean)
apply(dif.w,1,sd)/sqrt(4)


e.m<-unlist(lapply(pf_out2,function(x){x$mu}))-unlist(lapply(plall_out_ecotype_new,function(x){x$mu}))
e.w<-signif(unlist(lapply(pf_out2,function(x){x$sigma})),4)

r.w.diff<-e.w-signif(unlist(lapply(plall_out_ecotype_new,function(x){x$width})),4)

mean(r.w.diff)








##### Reviewer Edits -----
##### Fundamental temperature niche ---- new method
###Load in fundamental niche data ------
load(file=paste(Rds.outputfiles,'cbs_gaus_data_named_zeros.Rdata',sep=''))
load(file=paste(Rds.outputfiles,'ecotype_gaus_stats.Rdata',sep=''))

for (i in 1:4){
  if (i==1){
    ecotype.fund.df<-data.frame(plall_out_ecotype_new[[i]]$raw.data,variable=names(plall_out_ecotype_new)[i])
    zero.vals<-ecotype.fund.df[which(ecotype.fund.df$y==0),'x']
    a.df<-data.frame(plall_out_ecotype_new[[i]]$full.pred,variable=names(plall_out_ecotype_new)[i])
    ecotype.fund.fit<-a.df
    ecotype.fund.vals<-data.frame(mean=plall_out_ecotype_new[[i]]$mu,width=plall_out_ecotype_new[[i]]$width,variable=names(plall_out_ecotype_new)[i])
  }else{
    use<-data.frame(plall_out_ecotype_new[[i]]$raw.data,variable=names(plall_out_ecotype_new)[i])
    ecotype.fund.df<-rbind(ecotype.fund.df,data.frame(plall_out_ecotype_new[[i]]$raw.data,variable=names(plall_out_ecotype_new)[i]))
    zero.vals<-use[which(use$y==0),'x']
    a.df<-data.frame(plall_out_ecotype_new[[i]]$full.pred,variable=names(plall_out_ecotype_new)[i])
    ecotype.fund.fit<-rbind(ecotype.fund.fit,a.df)
    ecotype.fund.vals<-rbind(ecotype.fund.vals,data.frame(mean=plall_out_ecotype_new[[i]]$mu,width=plall_out_ecotype_new[[i]]$width,variable=names(plall_out_ecotype_new)[i]))
  }
}
letters.vals<-ecotype.fund.vals
letters.vals$labels<-paste(letters[1:4],".",sep='')
letters.vals<-letters.vals%>%
  dplyr::select(-mean,-width)


#### Testing NLME on fund ----
## Define the Gaussian model
Gauss<- function(a,mu,sig,x) a*exp(-1/2*(x-mu)^2/sig^2)

#### Set up data.frame

lm.df<-ecotype.fund.df[complete.cases(ecotype.fund.df[,1:2]),]


lm.df.grouped <-  nlme::groupedData(y ~ x | variable,
                                    data=lm.df,
                                    labels = list( x = "Temperature",
                                                   y = "Abundance" ),
                                    units = list( x = "(degC)", y = "(cell/ml)"))

plot(lm.df.grouped)
require(nlme)
fund.list.mod<-list()
fund.list.mod[[1]] <- nlme(y ~ Gauss(a,mu,sig,x), # list all parameters that make up ‘Gauss’, separated by comma – I am just assuming four now for simplicity’s sake 
                    data = lm.df.grouped, 
                    fixed = list(a+mu+sig ~ variable), #e.g. ocean basin, ecotype..
                    random =  nlme::pdDiag(a+mu+sig ~ 1), 
                    start = c(1,3,3,3,
                              20,3,3,3,
                              5,3,3,3), # par1 etc are starting values for the four (?) parameters that describe your Gauss distribution. These can be anything within reason, and just serve to give the model somewhere to start. 0 needs to be the number of levels of your explaining variable -1, i.e. 2 ocean basins = one 0. 
                    na.action = na.omit, 
                    method = 'ML', 
                    control = nlme::nlmeControl(pnlsTol = 1E2,
                                                minScale = 1E-6)) 
fund.list.mod[[2]]<- update(fund.list.mod[[1]], fixed = list(a ~ 1, mu + sig ~ 1 +  variable),start = c(1,
                                                                                   20,3,3,3,
                                                                                   5,3,3,3))
fund.list.mod[[3]]<- update(fund.list.mod[[1]], fixed = list(a +mu ~ 1, sig ~ 1 +  variable),start = c(1,
                                                                                   20,
                                                                                   5,3,3,3))
fund.list.mod[[4]]<- update(fund.list.mod[[1]], fixed = list(mu~ 1, a+ sig ~ 1 +  variable),start = c(20,
                                                                                       1,3,3,3,
                                                                                       5,3,3,3))
fund.list.mod[[5]]<- update(fund.list.mod[[1]], fixed = list(mu + sig~ 1, a~ 1 +  variable),start = c(20,5,
                                                                                      1,3,3,3))
fund.list.mod[[6]]<- update(fund.list.mod[[1]], fixed = list(sig~ 1, a+mu~ 1 +  variable),start = c(5,
                                                                                     1,3,3,3,
                                                                                    20,3,3,3))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(fund.list.mod), sep = " ")

##generate AICc table
require(AICcmodavg)
res.table<-aictab(fund.list.mod, modnames = Modnames, sort = TRUE)





###Load in realized niche data ------
for (i in 1:4){
  for (j in 1:4){
    raw.data.real.one<-data.frame(x=cbs_gaus_output_zero[[i]][[j]]$x,y=cbs_gaus_output_zero[[i]][[j]]$y)
    raw.data.real.sub<-raw.data.real.one
    raw.data.real.sub$variable<-names(cbs_gaus_output_zero)[i]
    raw.data.real.sub$CB<-names(cbs_gaus_output_zero[[i]])[j]
    if (i==1 & j==1){
      raw.data.real<-raw.data.real.sub
    }else{
      raw.data.real<-rbind(raw.data.real,raw.data.real.sub)
    }
  }
}
letters.real<-raw.data.real %>%
  group_by(variable,CB)%>%
  summarise(mean=mean(y,na.rm=T),.groups='keep')

letters.real$mean<-paste(letters[1:nrow(letters.real)][c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)],'.',sep='')
letters.real<-letters.real[,c(1,2,3)]
letters.real<-as.data.frame(letters.real)


lm.df<-raw.data.real[complete.cases(raw.data.real),]


lm.df.grouped <-  nlme::groupedData(y ~ x | CB/variable,
                                    data=lm.df,
                                    labels = list( x = "Temperature",
                                                   y = "Abundance" ),
                                    units = list( x = "(degC)", y = "(cell/ml)"))
lm.df.grouped$variable <- as.factor(lm.df.grouped$variable)
lm.df.grouped$CB <- as.factor(lm.df.grouped$CB)

plot(lm.df.grouped)
require(nlme)
real.list.mod<-list()
real.list.mod[[1]] <- nlme(y ~ Gauss(a,mu,sig,x), # list all parameters that make up ‘Gauss’, separated by comma – I am just assuming four now for simplicity’s sake 
                           data = lm.df.grouped, 
                           fixed = list(a+mu+sig ~ CB/variable), #e.g. ocean basin, ecotype..
                           random =  nlme::pdDiag(a+mu+sig ~ 1), 
                           start = c(1,rep(3,16),
                                     20,rep(3,16),
                                     5,rep(3,16)), # par1 etc are starting values for the four (?) parameters that describe your Gauss distribution. These can be anything within reason, and just serve to give the model somewhere to start. 0 needs to be the number of levels of your explaining variable -1, i.e. 2 ocean basins = one 0. 
                           na.action = na.omit, 
                           method = 'ML', 
                           control = nlme::nlmeControl(pnlsTol = 1E2,
                                                       minScale = 1E-6)) 
real.list.mod[[2]]<- update(real.list.mod[[1]], fixed = list(a ~ 1, mu + sig ~ 1 +  CB),start = c(1,
                                                                                                        20,3,3,3,
                                                                                                        5,3,3,3))
real.list.mod[[3]]<- update(real.list.mod[[1]], fixed = list(a +mu ~ 1, sig ~ 1 +  CB),start = c(1,
                                                                                                       20,
                                                                                                       5,3,3,3))
real.list.mod[[4]]<- update(real.list.mod[[1]], fixed = list(mu~ 1, a+ sig ~ 1 +  CB),start = c(20,
                                                                                                      1,3,3,3,
                                                                                                      5,3,3,3))
real.list.mod[[5]]<- update(real.list.mod[[1]], fixed = list(mu + sig ~ 1, a ~ 1 +  CB),start = c(20,5,
                                                                                                      1,3,3,3))
real.list.mod[[6]]<- update(real.list.mod[[1]], fixed = list(sig~ 1, a+mu~ 1 +  CB),start = c(5,
                                                                                                    1,3,3,3,
                                                                                                    20,3,3,3))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(real.list.mod), sep = " ")

##generate AICc table
require(AICcmodavg)
res.table<-aictab(real.list.mod, modnames = Modnames, sort = TRUE)





########## Find the fundamental temperature niche values ------
# abs.vals<-ecotype.fund.df%>%
#   group_by(variable)%>%
#   summarise(val=range(x[which(y==0)]),.groups='keep')%>%
#   summarise(val=val,diff=diff(val))%>%
#   group_modify(~{
#     if(.$diff[1]<2){
#       .$val[1]<--Inf
#     }
#     return(.)
#   })%>%
#   dplyr::select(-diff)
# 
# for (i in 1:4){
#   var.s=c('eMED4','eMIT9312','eNATL2A','eMIT9313')[i]
#   subdat=subset(ecotype.fund.fit,variable==var.s)
#   subdat<-subset(subdat,tt>=min(abs.vals[abs.vals$variable==var.s,'val']) & tt<=max(abs.vals[abs.vals$variable==var.s,'val']))
#   
#   max.val=max(subdat$tt)
#   if (min(abs.vals[abs.vals$variable==var.s,'val'])==-Inf){
#     subdat%>%
#       arrange(tt)%>%
#       summarise(x=tt,y=cumsum(ft2)/sum(ft2))%>%
#       summarise(min.1=x[which.min(abs(y-0.01))])%>%
#       pull(min.1) -> min.val
#   }else{
#     min.val<-min(subdat$tt)
#   }
#   mean.val<- quantile(na.omit(ecotype.fund.df[ecotype.fund.df$variable==var.s,c('x','y')])$x,c(0.5))
#   if (i==1){
#     full.subset<-data.frame(variable=var.s,q.mean=mean.val,q.max=max.val,q.min=min.val)
#     line.subset<-subdat
#   }else{
#     full.subset=rbind(full.subset,data.frame(variable=var.s,q.mean=mean.val,q.max=max.val,q.min=min.val))
#     line.subset<-rbind(line.subset,subdat)
#   }
# }
# full.quant.vals<-full.subset
ecotype.fund.df%>%
  drop_na(y)%>%
  filter(variable=='eMED4')
testing.thomas<-ecotype.fund.df%>%
  drop_na(y)%>%
  group_by(variable)%>%
  group_map(~{
    nls.pass2<-nls2(formula= y~(a*exp(x*0.0631)*(1-((x-z)/(w/2))^2)),
         data=.x,
         start=list(z=quantile(.x$x,0.5),
                    w=diff(quantile(.x$x,c(0.05,0.95))),
                    a=max(.x$y,na.rm=T)),
         algorithm = 'port',
        control=list(eval.max=5000, iter.max=1000,warnOnly=T),
        lower=list(z=-2,w=2,a=0.1),upper=list(z=40,w=35,a=1))
  
    
  z.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['z','Estimate'],summary(nls.pass2)$parameters['z','Std. Error']*sqrt(nrow(.x)))
  w.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['w','Estimate'],summary(nls.pass2)$parameters['w','Std. Error']*sqrt(nrow(.x)))
  a.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['a','Estimate'],summary(nls.pass2)$parameters['a','Std. Error']*sqrt(nrow(.x)))

  x.out<-seq(-2,40,by=0.01)
  mu.y<-mean(a.bs)*exp(x.out*0.0631)*(1-((x.out-mean(z.bs))/(mean(w.bs)/2))^2)
  mu.y[mu.y<0]=NA
  
  return(data.frame(x=x.out,y=mu.y))

  },.groups='keep')%>%
  setNames(group_keys(ecotype.fund.df%>%
                        drop_na(y)%>%
                        group_by(variable))%>%pull(variable))

do.call('rbind',lapply(testing.thomas,function(c){
  c%>%
    drop_na(y)%>%
    summarise(x=.$x,y=cumsum(.$x)/sum(.$x))%>%
    summarise(mean=.$x[which.min(abs(.$y-0.5))],
              min=.$x[which.min(abs(.$y-0.01))],
              max=.$x[which.min(abs(.$y-0.99))])
}))%>%
  mutate(variable=rownames(.))%>%{.->>thomas.vals}

do.call('rbind',testing.thomas)%>%
  mutate(variable=rownames(.))%>%
  separate(variable,into=c('variable'))%>%{.->>thomas.line}

fund.quant.vals<-thomas.vals

colnames(fund.quant.vals)[1:3]<-c('q.mean','q.min','q.max')
fund.quant.vals$dat<-'fund'
fund.quant.vals$CB<-'Lab'



thomas.flip<-pivot_longer(thomas.vals,cols=c('mean','min','max'))
fund.thomas.plot<-ggplot()+
  geom_point(data=ecotype.fund.df,aes(x=x,y=y))+
  geom_line(data=thomas.line,aes(x=x,y=y))+
  geom_vline(data=thomas.flip,aes(xintercept=value,col=name),linetype='dashed')+
  scale_color_manual(name='',values=c(max='blue',min='blue',mean='red'))+
  scale_x_continuous(name=bquote(Temperature~(degree*C)))+
  scale_y_continuous(name=bquote(Growth~rate~(day^-1)))+
  theme_bw(16)+
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="none",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.9, "lines"), 
        panel.spacing.y=unit(0.9,"lines"))+
  coord_cartesian(ylim = c(0,0.75),clip = 'off')+
  facet_wrap(~variable)+
  geom_text(data=letters.vals,aes(x = -2, y = 0.815, label = labels),fontface='bold')
ggsave('~/Desktop/thomas-fundamental.tiff',fund.thomas.plot,dpi=600,width=7,height=7.1)

### Comparison Thomas -----
do.call('rbind',raw.data.real%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
          group_map(~{group_by(.,CB)%>%
              group_map(~{
                quantile(.$x,c(0.01,0.5,0.99))
              },.groups='keep')%>%
              setNames(group_keys(raw.data.real%>%
                                    drop_na(c(x,y))%>%
                                    group_by(CB))%>%pull(CB))%>%
              do.call('rbind',.)%>%
              as.data.frame(.)%>%
              mutate(CB=rownames(.))
          })%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%
  separate(variable,c('variable','ex'))%>%
  dplyr::select(-ex)%>% {. ->> real.CB.quant.vals}
colnames(real.CB.quant.vals)[1:3]<-c('q.min','q.mean','q.max')
real.CB.quant.vals$dat='real'

all.quant.vals<-rbind(real.CB.quant.vals,do.call('rbind',replicate(4,fund.quant.vals,simplify=F)))
all.quant.vals<-all.quant.vals[order(all.quant.vals$variable),]
all.quant.vals$width<-all.quant.vals$q.max-all.quant.vals$q.min

values.signif<-ggplot()+
  geom_segment(data=subset(all.quant.vals,dat=='fund'),
               aes(x=subset(all.quant.vals,dat=='fund')$width,
                   y=subset(all.quant.vals,dat=='fund')$q.mean,
                   xend=subset(all.quant.vals,dat=='real')$width,
                   yend=subset(all.quant.vals,dat=='real')$q.mean),
               arrow = arrow(length = unit(0.1,"cm")))+
  # geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,ymin=CI.5,ymax=CI.95))+
  # geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,xmin=CI.5.w,xmax=CI.95.w))+
  geom_point(data=all.quant.vals,aes(x=width,y=q.mean,shape=CB,col=CB,fill=CB),size=4)+
  # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NA')),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable!='eMIT9313'),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable=='eMIT9313'),aes(x=width+1.7,y=q.mean+0.8,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NP','SA')),aes(x=width+1.5,y=q.mean+0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  scale_x_continuous(name=bquote(Niche~width~(degree*C)))+
  scale_y_continuous(name=bquote(Mean~temperature~(degree*C)))+
  scale_shape_manual(name='',
                     values = c('Lab' = 16, 'NA' = 24,
                                'SA'= 25,
                                'NP' = 24,
                                'SP'= 25))+
  scale_color_manual(name='',values=c('Lab'='black',
                                      'NP'='red',
                                      'NA'='blue',
                                      'SP'='red',
                                      'SA'='blue'))+
  scale_fill_manual(name='',values=c('Lab'='black',
                                     'NP'='red',
                                     'NA'='blue',
                                     'SP'='white',
                                     'SA'='white'))+
  theme_bw(16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="bottom",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.5, "lines"), 
        panel.spacing.y=unit(0,"lines"))+
  facet_wrap(.~factor(variable,levels=c('eMED4','eMIT9312','eMIT9313','eNATL2A')))+
  coord_cartesian(xlim=c(7,23),y=c(16,25),clip='off')+
  geom_text(data=letters.vals,aes(x = 7, y = 26, label = labels),fontface='bold')

ggsave('~/Desktop/thomas-rawdata.tiff',values.signif,dpi=600,width=7,height=7.1)


all.quant.2<-all.quant.vals[all.quant.vals$dat=='real',]
all.quant.2$fund.mean<-all.quant.vals[all.quant.vals$dat=='fund','q.mean']
all.quant.2$fund.width<-all.quant.vals[all.quant.vals$dat=='fund','width']
all.quant.2$diff.mean<-all.quant.2$fund.mean-all.quant.2$q.mean
all.quant.2$diff.width<-all.quant.2$fund.width-all.quant.2$width

apply(all.quant.2[,c('diff.mean','diff.width')],2,mean)
#### Bootstrap it ----
test
df<-data.frame(x=x.out,y=mu.y)
df%>%
  drop_na(y)%>%
  summarise(x=.$x,y=cumsum(.$x)/sum(.$x))%>%
  summarise(mean=.$x[which.min(abs(.$y-0.5))],
            min=.$x[which.min(abs(.$y-0.01))],
            max=.$x[which.min(abs(.$y-0.99))])




do.call('rbind',ecotype.fund.df%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
              group_map(~{
                quantile(.$x,c(0.01,0.5,0.99))
              },.groups='keep')%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>% {. ->> fund.quant.vals}
colnames(fund.quant.vals)[1:3]<-c('q.min','q.mean','q.max')
fund.quant.vals$dat<-'fund'
fund.quant.vals$CB<-'Lab'



fund.gaus.fit<-ggplot()+
  geom_point(data=ecotype.fund.df,aes(x=x,y=y))+
  #geom_line(data=line.subset,aes(x=tt,y=ft2))+
  geom_vline(data=fund.quant.vals,aes(xintercept=q.mean),col='red',linetype='dashed')+
  geom_vline(data=fund.quant.vals,aes(xintercept=q.min),col='blue',linetype='dashed')+
  geom_vline(data=fund.quant.vals,aes(xintercept=q.max),col='blue',linetype='dashed')+
  scale_x_continuous(name=bquote(Temperature~(degree*C)))+
  scale_y_continuous(name=bquote(Growth~rate~(day^-1)))+
  theme_bw(16)+
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="none",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.9, "lines"), 
        panel.spacing.y=unit(0.9,"lines"))+
  coord_cartesian(ylim = c(0,0.75),clip = 'off')+
  facet_wrap(~variable)+
  geom_text(data=letters.vals,aes(x = -2, y = 0.815, label = labels),fontface='bold')

ggsave('~/Desktop/fundamental_new_no-line.tiff',fund.gaus.fit,dpi=600,width=5,height=5.5)


######### Find the realized temperature niches values ------
do.call('rbind',raw.data.real%>%
          drop_na(c(x,y))%>%
          filter(y>0)%>%
          group_by(variable)%>%
          group_map(~{
                quantile(.$x,c(0.01,0.5,0.99))
              },.groups='keep')%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>% {. ->> real.ecotype.quant.vals}
colnames(real.ecotype.quant.vals)[1:3]<-c('q.min','q.mean','q.max')

real.ecotype.niche<-ggplot()+
  geom_point(data=raw.data.real,aes(x=x,y=y),alpha=0.1)+
  geom_vline(data=real.ecotype.quant.vals,aes(xintercept=q.mean),col='red',linetype='dashed')+
  geom_vline(data=real.ecotype.quant.vals,aes(xintercept=q.min),col='blue',linetype='dashed')+
  geom_vline(data=real.ecotype.quant.vals,aes(xintercept=q.max),col='blue',linetype='dashed')+
  scale_x_continuous(name=bquote(Temperature~(degree*C)))+
  scale_y_continuous(name=bquote(Log[10]~Abundance~(cell*mL^-1)))+
  theme_bw(16)+
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="none",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.9, "lines"), 
        panel.spacing.y=unit(0.9,"lines"))+
  facet_wrap(~variable)+
  coord_cartesian(ylim = c(0,6),clip = 'off')+
  geom_text(data=letters.vals,aes(x = -0.1, y = 6.7, label = labels),fontface='bold')



ggsave('~/Desktop/realized_new_ecotype.tiff',real.ecotype.niche,dpi=600,width=6,height=6)



######## REalized niches split into bioregions -------
do.call('rbind',raw.data.real%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
          group_map(~{group_by(.,CB)%>%
              group_map(~{
                quantile(.$x,c(0.01,0.5,0.99))
              },.groups='keep')%>%
              setNames(group_keys(raw.data.real%>%
                                    drop_na(c(x,y))%>%
                                    group_by(CB))%>%pull(CB))%>%
              do.call('rbind',.)%>%
              as.data.frame(.)%>%
              mutate(CB=rownames(.))
          })%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%
  separate(variable,c('variable','ex'))%>%
  dplyr::select(-ex)%>% {. ->> real.CB.quant.vals}
  colnames(real.CB.quant.vals)[1:3]<-c('q.min','q.mean','q.max')
real.CB.quant.vals$dat='real'

##### Skewness ----
do.call('rbind',raw.data.real%>%
  drop_na(c(x,y))%>%
  group_by(variable)%>%
  group_map(~{.x%>%group_by(CB)%>%
      group_map(~{
        skewness(.x$x)
      },.groups='keep')%>%
      setNames(group_keys(raw.data.real%>%
                            drop_na(c(x,y))%>%
                            group_by(CB))%>%pull(CB))%>%
      do.call('rbind',.)%>%
      as.data.frame(.)%>%
      mutate(CB=rownames(.))
  })%>%
  setNames(group_keys(raw.data.real%>%
                        drop_na(c(x,y))%>%
                        group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%{.->>skewness.CB}
skewness.CB$signif<-'skewed'
skewness.CB$signif[skewness.CB$V1<=0.5 & skewness.CB$V1>=-0.5]='normal'


do.call('rbind',raw.data.real%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
          group_map(~{.x%>%group_by(CB)%>%
              group_map(~{
                kurtosis(.x$x)
              },.groups='keep')%>%
              setNames(group_keys(raw.data.real%>%
                                    drop_na(c(x,y))%>%
                                    group_by(CB))%>%pull(CB))%>%
              do.call('rbind',.)%>%
              as.data.frame(.)%>%
              mutate(CB=rownames(.))
          })%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%{.->>kurtosis.CB}

kurtosis.CB[order(kurtosis.CB$CB),]

CB.values<- c(
  'NA' = "North Atlantic",
  'NP' = "North Pacific",
  'SA' = "South Atlantic",
  'SP' = "South Pacific"
)
raw.real.niche<-ggplot()+
  geom_point(data=raw.data.real,aes(x=x,y=y),alpha=0.1)+
  geom_vline(data=real.CB.quant.vals,aes(xintercept=q.mean),col='red',linetype='dashed')+
  geom_vline(data=real.CB.quant.vals,aes(xintercept=q.min),col='blue',linetype='dashed')+
  geom_vline(data=real.CB.quant.vals,aes(xintercept=q.max),col='blue',linetype='dashed')+
  scale_x_continuous(name=bquote(Temperature~(degree*C)))+
  scale_y_continuous(name=bquote(Log[10]~Abundance~(cell*mL^-1)))+
  theme_bw(16)+
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="none",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.9, "lines"), 
        panel.spacing.y=unit(0.9,"lines"))+
  facet_grid(CB~variable,labeller = labeller(CB = CB.values))+
  coord_cartesian(ylim = c(0,6),clip = 'off')+
  geom_text(data=letters.real,aes(x = -0.1, y = 6.7, label = mean),fontface='bold')



ggsave('~/Desktop/realized_new.tiff',raw.real.niche,dpi=600,width=6,height=7.5)

do.call('rbind',ecotype.fund.df%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
              group_map(~{
                skewness(.x$x)
              },.groups='keep')%>%
              setNames(group_keys(ecotype.fund.df%>%
                                    drop_na(c(x,y))%>%
                                    group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%{.->>skewness.fund}
skewness.fund$signif<-'skewed'
skewness.fund$signif[skewness.fund$V1<=0.5 & skewness.fund$V1>=-0.5]='normal'

do.call('rbind',ecotype.fund.df%>%
          drop_na(c(x,y))%>%
          group_by(variable)%>%
          group_map(~{
            kurtosis(.x$x)
          },.groups='keep')%>%
          setNames(group_keys(ecotype.fund.df%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>%{.->>kurtosis.fund}
skewness.fund$signif<-'skewed'
skewness.fund$signif[skewness.fund$V1<=0.5 & skewness.fund$V1>=-0.5]='normal'
skewness.fund
skewness.CB

####### Stats -------
###### Bootstrapping -----
####Fundamental 
    ###### Too little data to bootstrap the fundamental niche ######


ecotype.fund.df%>%
  group_by(variable)%>%
  drop_na(c(x,y))%>%
  filter(variable='eMED4')
  group_map(~{
    boot(ecotype.fund.df%>%
           group_by(variable)%>%
           drop_na(c(x,y))%>%
           filter(variable=='eMED4'),function(data,indices){
          
      data<-ecotype.fund.df%>%
        group_by(variable)%>%
        drop_na(c(x,y))%>%
        filter(variable=='eMED4')
      indices<-1:nrow(data)
      df=data[indices[sample(indices,length(indices)*0.8,replace=F)],]
        
        abs.vals.2=df%>%summarise(val=range(x[which(y==0)]),.groups='keep')%>%
            summarise(val=val,diff=diff(val),.groups='keep')%>%
            group_modify(~{
              if(.$diff[1]<2){
                .$val[1]<--Inf
              }
              return(.)
            })%>%
            dplyr::select(-diff)
        
        startmu<-mean(df$x[df$y>0],na.rm=T)
        startsigma<-sd(df$x[df$y>0],na.rm=T)
        
        nls.output<-with(df,nls2(y~(a*exp(-1/2*(x-mu)^2/sig^2)),
                                      algorithm = 'port', weights=n,
                                      control=list(maxiter=5000,eval.max=5000),
                                      start=list(a=max(df$y,na.rm=T),sig=startsigma,mu=startmu)))
        nls.output<-tryCatch(with(df,nls2(y~(a*exp(-1/2*(x-mu)^2/sig^2)),
                                 algorithm = 'port', weights=n,
                                 control=list(maxiter=5000,eval.max=5000),
                                 start=list(a=max(df$y,na.rm=T),sig=startsigma,mu=startmu))),error=print('no'))
        if(nls.output=='no'){
          q.min<-NA
          q.mean<-NA
          q.max<-NA
        }else{
        tt=seq(-2,40,by=0.1)
        ft2=(coef(nls.output)['a']*exp(-1/2*(tt-coef(nls.output)['mu'])^2/coef(nls.output)['sig']^2))
        
        df2<-data.frame(tt=tt,ft2=ft2)
        subdat<-subset(df2,tt>=min(abs.vals.2$val) & tt<=max(abs.vals.2$val))
        
        if(is.infinite(abs.vals.2$val[2])){
          abs.vals.2$val[2]<-Inf
        }
        
        if (min(abs.vals.2$val)==-Inf){
          min.val<-subdat%>%
            arrange(tt)%>%
            summarise(x=tt,y=cumsum(ft2)/sum(ft2))%>%
            summarise(min.1=x[which.min(abs(y-0.01))])%>%pull(min.1)
        }else{
          min.val<-min(subdat$tt)
        }
        if (max(abs.vals.2$val)==Inf){
          max.val<-subdat%>%
            arrange(tt)%>%
            summarise(x=tt,y=cumsum(ft2)/sum(ft2))%>%
            summarise(max=x[which.min(abs(y-0.99))])%>%
            pull(max)
        }else{
          max.val<-max(subdat$tt)
        }
         mean.val<- as.vector(quantile(df$x,c(0.5)))
        }
        full.subset<-list(q.mean=mean.val,q.max=max.val,q.min=min.val)
        return(full.subset)
          },100)
    
        return(boot)
      },.groups='keep')%>%
      setNames(group_keys(ecotype.fund.df%>%
                            drop_na(c(x,y))%>%
                            group_by(variable))%>%pull(variable))


abs.vals<-ecotype.fund.df%>%
  group_by(variable)%>%
  summarise(val=range(x[which(y==0)]),.groups='keep')%>%
  summarise(val=val,diff=diff(val))%>%
  group_modify(~{
    if(.$diff[1]<2){
      .$val[1]<--Inf
    }
    return(.)
  })%>%
  dplyr::select(-diff)

for (i in 1:4){
  var.s=c('eMED4','eMIT9312','eNATL2A','eMIT9313')[i]
  subdat=subset(ecotype.fund.fit,variable==var.s)
  subdat<-subset(subdat,tt>=min(abs.vals[abs.vals$variable==var.s,'val']) & tt<=max(abs.vals[abs.vals$variable==var.s,'val']))
  
  max.val=max(subdat$tt)
  if (min(abs.vals[abs.vals$variable==var.s,'val'])==-Inf){
    subdat%>%
      arrange(tt)%>%
      summarise(x=tt,y=cumsum(ft2)/sum(ft2))%>%
      summarise(min.1=x[which.min(abs(y-0.01))])%>%
      pull(min.1) -> min.val
  }else{
    min.val<-min(subdat$tt)
  }
  mean.val<- quantile(na.omit(ecotype.fund.df[ecotype.fund.df$variable==var.s,c('x','y')])$x,c(0.5))
  if (i==1){
    full.subset<-data.frame(variable=var.s,q.mean=mean.val,q.max=max.val,q.min=min.val)
    line.subset<-subdat
  }else{
    full.subset=rbind(full.subset,data.frame(variable=var.s,q.mean=mean.val,q.max=max.val,q.min=min.val))
    line.subset<-rbind(line.subset,subdat)
  }
}
full.quant.vals<-full.subset
#####hiding########
ecotype.fund.df%>%
  drop_na(c(y))%>%
  group_by(variable)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),0.5)
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(ecotype.fund.df%>%
                        drop_na(c(y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.full.fund.means}

do.call('rbind',lapply(boot.full.fund.means,function(v){quantile(v,c(0.05,0.95))}))
do.call('rbind',lapply(boot.full.fund.widths,function(v){quantile(v,c(0.05,0.95))}))

fund.quant.vals$q.max-fund.quant.vals$q.min

real.CB.quant.vals %>%
  group_by(variable)%>%
  summarise(mean=mean(q.mean),sd.m=sd(q.mean),width=mean(q.max-q.min), sd.w=sd(q.max-q.min))

ecotype.fund.df%>%
  drop_na(c(y))%>%
  group_by(variable)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      diff(quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),c(0.01,0.99)))
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(ecotype.fund.df%>%
                        drop_na(c(y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.full.fund.widths}





####Realized
b.s.iter<-1000
raw.data.real%>%
  drop_na(c(x,y))%>%
  group_by(variable)%>%
  group_map(~{group_by(.,CB)%>%
      group_map(~{
        mean.list<-lapply(1:b.s.iter,function(c){
          c<-.x
          quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),0.5)
        })
        return(as.vector(unlist(mean.list)))
      },.groups='keep')%>%
      setNames(group_keys(raw.data.real%>%
                            drop_na(c(x,y))%>%
                            group_by(CB))%>%pull(CB))
  })%>%
  setNames(group_keys(raw.data.real%>%
                        drop_na(c(x,y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.full.real.CB.means}

lapply(boot.full.real.CB.means,function(c){do.call('rbind',lapply(c,function(v){quantile(v,c(0.05,0.95))}))})

raw.data.real%>%
  drop_na(c(x,y))%>%
  group_by(variable)%>%
  group_map(~{group_by(.,CB)%>%
      group_map(~{
        width.list<-lapply(1:b.s.iter,function(c){
          c<-.x
          diff(quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),c(0.01,0.99)))
        })
      return(as.vector(unlist(width.list)))
      },.groups='keep')%>%
      setNames(group_keys(raw.data.real%>%
                            drop_na(c(x,y))%>%
                            group_by(CB))%>%pull(CB))
  })%>%
  setNames(group_keys(raw.data.real%>%
                        drop_na(c(x,y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.full.real.CB.widths}

lapply(boot.full.real.CB.widths,function(c){do.call('rbind',lapply(c,function(v){quantile(v,c(0.05,0.95))}))})




####### Confidence difference between lab and field means -----
for (j in 1:4){
  e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
  raw.ts<-subset(fund.quant.vals,variable==e.df)$q.mean
  for (i in 1:4){
    cb.df<-c('NA','NP','SA','SP')[i]
    raw.x<-subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.mean
    real_data <- boot.full.real.CB.means[[eval(e.df)]][[eval(cb.df)]]
    real_data2<-real_data[order(real_data)]
    real_df_cumsum<-data.frame(x=real_data2,y=cumsum(real_data2)/sum(real_data2))
    conf.val<-real_df_cumsum$y[which.min(abs(real_df_cumsum$x-raw.ts))]
    if (raw.ts>raw.x){
      conf.val<-conf.val
    }else{
      conf.val<-1-conf.val
    }
    t.test.val<-t.test(real_data,mu=raw.ts,exact=T)
    if (i==1){
      ks.df<-data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value)
    }else{
      ks.df<-rbind(ks.df,data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value))
    }
  }
  if(j==1){
    ks.all.cb_lab.means<-ks.df
  }else{
    ks.all.cb_lab.means<-rbind(ks.all.cb_lab.means,ks.df)
  }
}
ks.all.cb_lab.means$sigmeans<-'ns'
ks.all.cb_lab.means$sigmeans[(1-ks.all.cb_lab.means$p_val_c)<0.05]='*'
ks.all.cb_lab.means$sigmeans[(1-ks.all.cb_lab.means$p_val_c)<0.01]='**'


for (i in 1:4){
  for (j in 1:4){
  e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[i]
  cb.df<-c('NA','NP','SA','SP')[j]
  mnu.test<-wilcox.test(boot.full.fund.means[[e.df]], boot.full.real.CB.means[[e.df]][[cb.df]],'two.sided')
  
  if(i==1 & j==1){
  out.df<-data.frame(variable=e.df,
                     CB=cb.df,
                     mwu_p=mnu.test$p.value)
  }else{
  out.df<-rbind(out.df,data.frame(variable=e.df,
                                  CB=cb.df,
                                  mwu_p=mnu.test$p.value))
}
  }
}
mwu_fund_CB<-out.df
mwu_fund_CB$sigmeans<-'ns'
mwu_fund_CB$sigmeans[(mwu_fund_CB$mwu_p)<0.05]='*'
mwu_fund_CB$sigmeans[(mwu_fund_CB$mwu_p)<0.01]='**'



######### Confidence of lab vs ecotype width comparisons-------
for (j in 1:4){
  e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
  raw.ts<-subset(fund.quant.vals,variable==e.df)$q.max-subset(fund.quant.vals,variable==e.df)$q.min
  for (i in 1:4){
    cb.df<-c('NA','NP','SA','SP')[i]
    raw.x<-subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.max - subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.min
    real_data <- boot.full.real.CB.widths[[eval(e.df)]][[eval(cb.df)]]
    real_data2<-real_data[order(real_data)]
    real_df_cumsum<-data.frame(x=real_data2,y=cumsum(real_data2)/sum(real_data2))
    conf.val<-real_df_cumsum$y[which.min(abs(real_df_cumsum$x-raw.ts))]
    if (raw.ts>raw.x){
      conf.val<-conf.val
    }else{
      conf.val<-1-conf.val
    }
    t.test.val<-t.test(real_data,mu=raw.ts,exact=T)
    if (i==1){
      ks.df<-data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value)
    }else{
      ks.df<-rbind(ks.df,data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value))
    }
  }
  if(j==1){
    ks.all.cb_lab.widths<-ks.df
  }else{
    ks.all.cb_lab.widths<-rbind(ks.all.cb_lab.widths,ks.df)
  }
}
ks.all.cb_lab.widths$sigwidths<-'ns'
ks.all.cb_lab.widths$sigwidths[(1-ks.all.cb_lab.widths$p_val_c)<0.05]='*'
ks.all.cb_lab.widths$sigwidths[(1-ks.all.cb_lab.widths$p_val_c)<0.01]='**'


for (i in 1:4){
  for (j in 1:4){
    e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[i]
    cb.df<-c('NA','NP','SA','SP')[j]
    mnu.test<-wilcox.test(boot.full.fund.widths[[e.df]], boot.full.real.CB.widths[[e.df]][[cb.df]],'two.sided')
    
    if(i==1 & j==1){
      out.df<-data.frame(variable=e.df,
                         CB=cb.df,
                         mwu_p=mnu.test$p.value)
    }else{
      out.df<-rbind(out.df,data.frame(variable=e.df,
                                      CB=cb.df,
                                      mwu_p=mnu.test$p.value))
    }
  }
}
mwu_trans<-out.df
mwu_trans$sigwidths<-'ns'
mwu_trans$sigwidths[(mwu_trans$mwu_p)<0.05]='*'
mwu_trans$sigwidths[(mwu_trans$mwu_p)<0.01]='**'



##### Combine 
sig.vals.CB.lab<-left_join(ks.all.cb_lab.means,ks.all.cb_lab.widths,c('variable','CB'))%>%
  dplyr::select(!ends_with("y") & !ends_with("x"))

signif_vals_lab.CB<-left_join(mwu_fund_CB,mwu_trans,c('variable','CB'))%>%
  dplyr::select(-c('mwu_p.x','mwu_p.y'))
sig.vals.CB.lab<-signif_vals_lab.CB
###### Plot the results figure ----
all.quant.vals<-rbind(real.CB.quant.vals,do.call('rbind',replicate(4,fund.quant.vals,simplify=F)))
all.quant.vals<-all.quant.vals[order(all.quant.vals$variable),]
all.quant.vals$width<-all.quant.vals$q.max-all.quant.vals$q.min






error.CI.5<-data.frame(do.call('rbind',lapply(boot.full.real.CB.means,function(x){unlist(lapply(x,function(c){quantile(c,c(0.05))}),use.names=T)})))
error.CI.95<-data.frame(do.call('rbind',lapply(boot.full.real.CB.means,function(x){unlist(lapply(x,function(c){quantile(c,c(0.95))}),use.names=T)})))
colnames(error.CI.5)<-c('NA','NP','SA','SP')
colnames(error.CI.95)<-c('NA','NP','SA','SP')
CI.means.5<-error.CI.5%>%
  mutate(variable=rownames(.))%>%
  gather(.,CB,val,-variable)%>%
  mutate(key='CI.5',se=val)%>%
  dplyr::select(.,-val)
CI.means.95<-error.CI.95%>%
  mutate(variable=rownames(.))%>%
  gather(.,CB,val,-variable)%>%
  mutate(key='CI.95',se=val)%>%
  dplyr::select(.,-val)

CI.all.mean<-rbind(CI.means.5,CI.means.95)%>%
  group_by(variable)%>%
  arrange(key,.by.group=T)%>%
  pivot_wider(.,names_from='key',values_from='se')


error.CI.fund<-data.frame(do.call('rbind',lapply(boot.full.fund.means,function(x){quantile(x,c(0.05,0.95))})))
colnames(error.CI.fund)<-c('CI.5','CI.95')
error.CI.fund.means<-error.CI.fund%>%
  mutate(variable=rownames(.))


error.CI.5.width<-data.frame(do.call('rbind',lapply(boot.full.real.CB.widths,function(x){unlist(lapply(x,function(c){quantile(c,0.05)}),use.names=T)})))
error.CI.95.width<-data.frame(do.call('rbind',lapply(boot.full.real.CB.widths,function(x){unlist(lapply(x,function(c){quantile(c,0.95)}),use.names=T)})))
colnames(error.CI.5.width)<-c('NA','NP','SA','SP')
colnames(error.CI.95.width)<-c('NA','NP','SA','SP')
CI.width.5<-error.CI.5.width%>%
  mutate(variable=rownames(.))%>%
  gather(.,CB,val,-variable)%>%
  mutate(key='CI.5.w',se=val)%>%
  dplyr::select(.,-val)
CI.width.95<-error.CI.95.width%>%
  mutate(variable=rownames(.))%>%
  gather(.,CB,val,-variable)%>%
  mutate(key='CI.95.w',se=val)%>%
  dplyr::select(.,-val)


CI.all.width<-rbind(CI.width.5,CI.width.95)%>%
  group_by(variable)%>%
  arrange(key,.by.group=T)%>%
  pivot_wider(.,names_from='key',values_from='se')

error.CI.fund.width<-data.frame(do.call('rbind',lapply(boot.full.fund.widths,function(x){quantile(x,c(0.05,0.95))})))
colnames(error.CI.fund.width)<-c('CI.5.w','CI.95.w')
error.CI.fund.width<-error.CI.fund.width%>%
  mutate(variable=rownames(.))

fund.combo<-left_join(error.CI.fund.means,error.CI.fund.width)
fund.combo$CB='Lab'
join.1<-rbind(as.data.frame(left_join(CI.all.width,CI.all.mean)),fund.combo)
all.combo.vals<-left_join(all.quant.vals,join.1,by=c('variable','CB'))

all.combo.vals<-left_join(all.combo.vals,sig.vals.CB.lab)

values.signif<-ggplot()+
  geom_segment(data=subset(all.quant.vals,dat=='fund'),
               aes(x=subset(all.quant.vals,dat=='fund')$width,
                   y=subset(all.quant.vals,dat=='fund')$q.mean,
                   xend=subset(all.quant.vals,dat=='real')$width,
                   yend=subset(all.quant.vals,dat=='real')$q.mean),
               arrow = arrow(length = unit(0.1,"cm")))+
  geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,ymin=CI.5,ymax=CI.95))+
  geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,xmin=CI.5.w,xmax=CI.95.w))+
  geom_point(data=all.quant.vals,aes(x=width,y=q.mean,shape=CB,col=CB,fill=CB),size=4)+
  geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NA')),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable!='eMIT9313'),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable=='eMIT9313'),aes(x=width+1.7,y=q.mean+0.8,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NP','SA')),aes(x=width+1.5,y=q.mean+0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
  scale_x_continuous(name=bquote(Niche~width~(degree*C)))+
  scale_y_continuous(name=bquote(Mean~temperature~(degree*C)))+
  scale_shape_manual(name='',
                     values = c('Lab' = 16, 'NA' = 24,
                                'SA'= 25,
                                'NP' = 24,
                                'SP'= 25))+
  scale_color_manual(name='',values=c('Lab'='black',
                                      'NP'='red',
                                      'NA'='blue',
                                      'SP'='red',
                                      'SA'='blue'))+
  scale_fill_manual(name='',values=c('Lab'='black',
                                     'NP'='red',
                                     'NA'='blue',
                                     'SP'='white',
                                     'SA'='white'))+
  theme_bw(16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="bottom",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.5, "lines"), 
        panel.spacing.y=unit(0,"lines"))+
  facet_wrap(.~factor(variable,levels=c('eMED4','eMIT9312','eNATL2A','eMIT9313')))+
  coord_cartesian(xlim=c(7,23),y=c(16,25),clip='off')+
  geom_text(data=letters.vals,aes(x = 7, y = 26, label = labels),fontface='bold')


ggsave('~/Desktop/signif_resultsfig_no-line-withbootstrap95-lab-true.tiff',values.signif,dpi=600,width=7,height=7.1)


####### Confidence of region-region differences -----
### Means
for (j in 1:4){
  e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
  for (i in 1:4){
    full.re<-c('NA','NP','SA','SP')
    cb.df<-full.re[i]
    real_data_comp <- boot.full.real.CB.means[[eval(e.df)]][[eval(cb.df)]]$t
    for (b in 1:3){
      sub.cb<-full.re[!full.re==cb.df][b]
      real_data <-  boot.full.real.CB.means[[eval(e.df)]][[eval(sub.cb)]]$t
      
      id=paste(e.df,paste(cb.df,sub.cb,sep='.'),sep='_')
      sim.test1<-ks.test(real_data_comp, real_data,'two.sided')
      sim.test2<-wilcox.test(real_data_comp, real_data,'two.sided')
      sim.test3<-t.test(real_data_comp, real_data,'two.sided')
      
      if (b==1){
        out.df<-data.frame(id=id,KS_p=sim.test1$p.value,wil_p=sim.test2$p.value,t_p=sim.test3$p.value)
      }else{
        out.df<-rbind(out.df,data.frame(id=id,KS_p=sim.test1$p.value,wil_p=sim.test2$p.value,t_p=sim.test3$p.value))
      }
    }
    if (i==1){
      ks.df<-out.df
    }else{
      ks.df<-rbind(ks.df,out.df)
    }
  }
  if(j==1){
    ks.all.cb_cb.mean<-ks.df
  }else{
    ks.all.cb_cb.mean<-rbind(ks.all.cb_cb.mean,ks.df)
  }
}
ks.all.cb_cb.mean$sig.ks<-'ns'
ks.all.cb_cb.mean$sig.ks[ks.all.cb_cb.mean$KS_p<0.05]='*'
ks.all.cb_cb.mean$sig.ks[ks.all.cb_cb.mean$KS_p<0.01]='**'
ks.all.cb_cb.mean$sig.t<-'ns'
ks.all.cb_cb.mean$sig.t[ks.all.cb_cb.mean$t_p<0.05]='*'
ks.all.cb_cb.mean$sig.t[ks.all.cb_cb.mean$t_p<0.01]='**'
ks.all.cb_cb.mean$sig.wils<-'ns'
ks.all.cb_cb.mean$sig.wils[ks.all.cb_cb.mean$wil_p<0.05]='*'
ks.all.cb_cb.mean$sig.wils[ks.all.cb_cb.mean$wil_p<0.01]='**'

####Widths
for (j in 1:4){
  e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
  for (i in 1:4){
    full.re<-c('NA','NP','SA','SP')
    cb.df<-full.re[i]
    real_data_comp <- boot.vals.real.width.CB[[eval(e.df)]][[eval(cb.df)]]$t[,3]
    for (b in 1:3){
      sub.cb<-full.re[!full.re==cb.df][b]
      real_data <-  boot.vals.real.width.CB[[eval(e.df)]][[eval(sub.cb)]]$t[,3]
      id=paste(e.df,paste(cb.df,sub.cb,sep='.'),sep='_')
      sim.test1<-ks.test(real_data_comp, real_data,'two.sided',exact=T)
      sim.test2<-wilcox.test((real_data_comp), (real_data),'two.sided',exact=T)
      sim.test3<-t.test(unique(real_data_comp), unique(real_data),'two.sided',exact=T)
      if (b==1){
        out.df<-data.frame(id=id,KS_p=sim.test1$p.value,wil_p=sim.test2$p.value,t_p=sim.test3$p.value)
      }else{
        out.df<-rbind(out.df,data.frame(id=id,KS_p=sim.test1$p.value,wil_p=sim.test2$p.value,t_p=sim.test3$p.value))
      }
    }
    if (i==1){
      ks.df<-out.df
    }else{
      ks.df<-rbind(ks.df,out.df)
    }
  }
  if(j==1){
    ks.all.cb_cb.width<-ks.df
  }else{
    ks.all.cb_cb.width<-rbind(ks.all.cb_cb.width,ks.df)
  }
}
ks.all.cb_cb.width$sig.ks<-'ns'
ks.all.cb_cb.width$sig.ks[ks.all.cb_cb.width$KS_p<0.05]='*'
ks.all.cb_cb.width$sig.ks[ks.all.cb_cb.width$KS_p<0.01]='**'
ks.all.cb_cb.width$sig.t<-'ns'
ks.all.cb_cb.width$sig.t[ks.all.cb_cb.width$t_p<0.05]='*'
ks.all.cb_cb.width$sig.t[ks.all.cb_cb.width$t_p<0.01]='**'
ks.all.cb_cb.width$sig.wils<-'ns'
ks.all.cb_cb.width$sig.wils[ks.all.cb_cb.width$wil_p<0.05]='*'
ks.all.cb_cb.width$sig.wils[ks.all.cb_cb.width$wil_p<0.01]='**'



####### Summary values ------
### Fundamental temperature niche values -----
fund.niche.values<-full.quant.vals[,1:2]
fund.niche.values$width<-full.quant.vals$q.max-full.quant.vals$q.min
rownames(fund.niche.values)<-NULL
colnames(fund.niche.values)[2:3]<-c('f.mean','f.width')
real.niche.values<-real.CB.quant.vals[,c('variable','CB','q.mean')]
real.niche.values$q.width<-real.CB.quant.vals$q.max-real.CB.quant.vals$q.min

full.niche.values<-left_join(real.niche.values,fund.niche.values)
full.niche.values$fund.diff.mean<-full.niche.values$q.mean-full.niche.values$f.mean
full.niche.values$fund.diff.width<-full.niche.values$width-full.niche.values$f.width

#### Average difference ----
mean(full.niche.values$fund.diff.width)
sd(full.niche.values$fund.diff.width)

mean(full.niche.values$fund.diff.mean)
sd(full.niche.values$fund.diff.mean)




by.variable<-full.niche.values%>%
  group_by(variable)%>%
  summarise(mean.w=mean(fund.diff.width),
            sd.w=sd(fund.diff.width),
            mean.m=mean(fund.diff.mean),
            sd.m=sd(fund.diff.mean),
            max.m=max(fund.diff.mean),
            max.w=max(fund.diff.width),
            min.m=min(fund.diff.mean),
            min.w=min(fund.diff.width),
            .groups='keep')

diff.var<-full.niche.values%>%
  group_by(variable)%>%
  summarise(mean.w=max(diff(width)),
            sd.w=sd(diff(width)),
            mean.m=max(diff(q.mean)),
            sd.m=sd(diff(q.mean)),
            .groups='keep')

by.CB<-full.niche.values%>%
  group_by(CB)%>%
  summarise(mean.w=mean(fund.diff.width),
            sd.w=sd(fund.diff.width),
            mean.m=mean(fund.diff.mean),
            sd.m=sd(fund.diff.mean),
            max.m=max(fund.diff.mean),
            max.w=max(fund.diff.width),
            min.m=min(fund.diff.mean),
            min.w=min(fund.diff.width),
            .groups='keep')


###### the strains -------
strain.names<-unique(picosubset$`Our Naming`)
for (i in 1:length(strain.names)){

use=strain.names[i]
y=picosubset[which(picosubset$`Our Naming`==use),grep('T',colnames(picosubset))]
x=as.numeric(gsub('T_','',colnames(picosubset)[grep('T',colnames(picosubset))]))
y2=matrix(as.numeric(as.matrix(y)),nrow=nrow(y),ncol=ncol(y))
x2=matrix(rep(x),nrow=nrow(y2),ncol=ncol(y2),byrow=T)
n2=matrix(as.numeric(as.matrix(n)),nrow=nrow(y),ncol=ncol(y))
y3=as.vector(y2)
x3=as.vector(x2)
eco.use<-picosubset[which(picosubset$`Our Naming`==use),'ecotype']

if (i==1){
  strain.out<-data.frame(strain=use,ecotype=eco.use,x=x3,y=y3)
}else{
  strain.out<-rbind(strain.out,data.frame(strain=use,ecotype=eco.use,x=x3,y=y3))
}
}

do.call('rbind',strain.out%>%
  drop_na(y)%>%
  group_by(strain)%>%
  group_map(~{quantile(.x$x,c(0.01,0.5,0.99))},.groups='keep')%>%
  setNames(group_keys(strain.out%>%
                        drop_na(y)%>%
                        group_by(strain))%>%pull(strain)))%>%
  as.data.frame(.)%>%
  mutate(strain=rownames(.))%>% {. ->> strains.fund}
colnames(strains.fund)[1:3]<-c('q.min','q.mean','q.max')
  

strains.fund[strains.fund$strain%in%c('MIT9321','UH18301','VOL29'),1:3]<-NA

strain.out%>%
  drop_na(c(y))%>%
  group_by(strain)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),0.5)
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(strain.out%>%
                        drop_na(c(y))%>%
                        group_by(strain))%>%pull(strain))%>%
                        {. ->> boot.strains}

strain.out%>%
  drop_na(c(y))%>%
  group_by(strain)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      diff(quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),c(0.01,0.99)))
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(strain.out%>%
                        drop_na(c(y))%>%
                        group_by(strain))%>%pull(strain))%>%
                        {. ->> boot.strains.width}

do.call('rbind',lapply(boot.strains,function(v){quantile(v,c(0.05,0.95))}))
do.call('rbind',lapply(boot.strains.width,function(v){quantile(v,c(0.05,0.95))}))
strains.fund$q.max-strains.fund$q.min

do.call('rbind',raw.data.real%>%
          drop_na(c(x,y))%>%
          filter(y>0)%>%
          group_by(variable)%>%
          group_map(~{
            quantile(.$x,c(0.01,0.5,0.99))
          },.groups='keep')%>%
          setNames(group_keys(raw.data.real%>%
                                drop_na(c(x,y))%>%
                                group_by(variable))%>%pull(variable)))%>%
  as.data.frame(.)%>%
  mutate(variable=rownames(.))%>% {. ->> real.ecotype.quant.vals}
colnames(real.ecotype.quant.vals)[1:3]<-c('q.min','q.mean','q.max')


raw.data.real%>%
  drop_na(c(x,y))%>%
  filter(y>0)%>%
  group_by(variable)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),0.5)
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(raw.data.real%>%
                        drop_na(c(x,y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.ecotype}


do.call('rbind',lapply(boot.ecotype,function(v){quantile(v,c(0.05,0.95))}))

raw.data.real%>%
  drop_na(c(x,y))%>%
  filter(y>0)%>%
  group_by(variable)%>%
  group_map(~{
    mean.list<-lapply(1:b.s.iter,function(c){
      c<-.x
      diff(quantile(sample(c$x,size=floor(length(c$x)*0.8),replace=F),c(0.01,0.99)))
    })
    return(as.vector(unlist(mean.list)))
  },.groups='keep')%>%
  setNames(group_keys(raw.data.real%>%
                        drop_na(c(x,y))%>%
                        group_by(variable))%>%pull(variable))%>%
                        {. ->> boot.ecotype.width}


do.call('rbind',lapply(boot.ecotype.width,function(v){quantile(v,c(0.05,0.95))}))
real.ecotype.quant.vals$q.max-real.ecotype.quant.vals$q.min

strain.plot<-ggplot()+
    geom_point(data=strain.out,aes(x=x,y=y))+
    geom_vline(data=strains.fund,aes(xintercept=q.mean),col='red',linetype='dashed')+
    geom_vline(data=strains.fund,aes(xintercept=q.min),col='blue',linetype='dashed')+
    geom_vline(data=strains.fund,aes(xintercept=q.max),col='blue',linetype='dashed')+
    scale_x_continuous(name=bquote(Temperature~(degree*C)))+
    scale_y_continuous(name=bquote(Log[10]~Abundance~(cell*mL^-1)))+
    theme_bw(16)+
    theme(panel.grid=element_blank(),
          axis.title = element_text(size=12),
          axis.text = element_text(size=12,color='black'),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust=0.5, face='bold'),
          aspect.ratio=1,
          legend.position="none",legend.box = "horiztonal",
          strip.background = element_blank(),
          strip.text = element_text(size=12,face='bold',vjust=-1),
          legend.text = element_text(size=12),
          panel.spacing.x=unit(0.9, "lines"), 
          panel.spacing.y=unit(0.9,"lines"))+
    facet_wrap(~factor(strain,levels = strain.names))+
    coord_cartesian(ylim = c(0,0.65),clip = 'off')+
    geom_text(data=data.frame(strain=strain.names,labels=paste(letters[1:11],'.',sep='')),aes(x = -0.1, y = 0.72, label = labels),fontface='bold',size=4.5)
  
ggsave('~/Desktop/strain_fund.tiff',dpi=600,strain.plot,width=7,height=7)
 
###### removing strains from eMIT9312 ------
testing.diff<-strain.out%>%
  filter(ecotype=='eMIT9312')

###Sequential
for (i in 1:length(unique(testing.diff$strain))){
  use.df<-testing.diff%>%
    filter(strain!=unique(testing.diff$strain)[1:i])
  if (i==1){
    df.out<-as.data.frame(quantile(use.df%>%drop_na(y)%>%pull(x),c(0.01,0.05,0.99)))
  }else{
    df.out<-rbind(df.out,data.frame(quantile(use.df%>%drop_na(y)%>%pull(x),c(0.01,0.05,0.99))))
  }
  
}

###Random
num.strains<-data.frame(iter=1:50)
num.strains<-num.strains%>%
  mutate(st.num = case_when(
    iter<=10 ~ 1,
    iter>10 &iter<=20 ~ 2,
    iter>20 & iter<=30 ~ 3,
    iter>30 & iter<=40 ~ 4,
    iter>40 & iter<=50 ~5
  ))
for (j in 1:100){
for (i in 1:5){
  use.df<-testing.diff%>%
    filter(!strain %in% sample(unique(testing.diff$strain),i,replace=F))
  
  q.out<-quantile(use.df%>%drop_na(y)%>%pull(x),c(0.01,0.5,0.99))
  if (i==1){
  out.df<-data.frame(num.strains=i,q.1=q.out[1],q.50=q.out[2],q.99=q.out[3])
  }else{
    out.df<-rbind(out.df,data.frame(num.strains=i,q.1=q.out[1],q.50=q.out[2],q.99=q.out[3]))
  }
}
  if (j==1){
    out.df.rep<-out.df
    }else{
      out.df.rep<-rbind(out.df.rep,out.df)
    }
  }

random.takeout<-do.call('rbind',out.df.rep%>%
  group_by(num.strains)%>%
  mutate(width=q.99-q.1)%>%
  group_map(~{apply(.x,2,mean)},.groups='keep'))%>%
  as.data.frame()%>%
  mutate(strain.nums=group_keys(out.df.rep%>%
                        group_by(num.strains))%>%pull(num.strains))

random.takeout.summary<-do.call('rbind',out.df.rep%>%
                          group_by(num.strains)%>%
                          mutate(width=q.99-q.1)%>%
                          group_map(~{apply(.x,2,mean)},.groups='keep'))%>%
  as.data.frame()%>%
  mutate(strain.nums=group_keys(out.df.rep%>%
                                  group_by(num.strains))%>%pull(num.strains))%>%
  group_map(~{cbind(apply(.x,2,mean),apply(.x,2,sd))})%>%
  as.data.frame()%>%
  mutate(variable=rownames(.))
colnames(random.takeout.summary)[1:2]<-c('mean','sd')


random.sample<-ggplot()+
  geom_point(data=testing.diff,aes(x=x,y=y))+
  geom_rect(data=random.takeout.summary[2,],aes(xmin=mean-sd,xmax=mean+sd,ymin=-Inf,ymax=Inf),alpha=0.1,fill='red')+
  geom_rect(data=random.takeout.summary[c(1,3),],aes(xmin=mean-sd,xmax=mean+sd,ymin=-Inf,ymax=Inf),alpha=0.1,fill='blue')+
  geom_vline(data=random.takeout.summary[1:3,],aes(xintercept=mean),col=c('blue','red','blue'),linetype='dashed')+
  geom_vline(data=fund.9312,aes(xintercept=value),col=c('blue','red','blue'))+
  scale_x_continuous(name=bquote(Temperature~(degree*C)))+
  scale_y_continuous(name=bquote(Growth~rate~(days^-1)),expand=c(0.01,0))+
  coord_cartesian(ylim=c(0,0.65))+
  theme_bw(16)+
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12,color='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(hjust=0.5, face='bold'),
        aspect.ratio=1,
        legend.position="none",legend.box = "horiztonal",
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold',vjust=-1),
        legend.text = element_text(size=12),
        panel.spacing.x=unit(0.9, "lines"), 
        panel.spacing.y=unit(0.9,"lines"))
  
ggsave('~/Desktop/eMIT9312_sampling.tiff',dpi=600,random.sample,width=4,height=4)

fund.9312<-fund.quant.vals%>%
  filter(variable=='eMIT9312')%>%
  melt()
colnames(fund.9312)[4]<-'quant'

random.takeout.summary[1:3,1]-fund.9312$value
diff(random.takeout.summary[c(1,3),1])-diff(fund.9312$value[c(1,3)])


######## Model just for the paper --------
icloud<-c('~/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/PhD_Work/Research')
open<-nc_open(paste(icloud,'/Niche_Model/Data_Sets/sst.day.mean.nc',sep=''))
# summary.ncdf(open)
tlon<-ncvar_get(open,'lon')
tlat<-ncvar_get(open,'lat')
ttime<-ncvar_get(open,'time')
tsst<-ncvar_get(open,'sst')

###### Model Set-up #######

#Initial parameters
n=10 #species
bx=20 #boxes; Keep even
t.step=3/24 #day
k=0.5*(1/1E-3)*(1/1E3) #mmolPm^-3 half-saturation constant
d=(1E-5)*60*60*24 #50*(1/365)*(1/1E2) #(1E-5)*60*60*24 #50*(1/365)*(1/1E2) #(1E-5)*60*60*24 #vertical flux of nutrients from the deep; day^-1
R.0=(0.8)*(1/1E3)*(1/1E-3) #Concentration of nutrients in the deep being advected up; chemostatic mmolP m^-3
years=50 #How many years the model runs for; should up to 100 on final runs
n.day=365*years #Number of days for model run purposes
iter=n.day/t.step #Model iterations
P.alpha=1 #Value by which to raise P to in the equation
m=0.4

###### Setting up latitudes ######

fulllatitude=seq(-80,90,length.out = (bx+1)) #Split up -80 to 90 by the number of boxes +1
lwoz<-length(fulllatitude[!fulllatitude==0]) #New number of boxes
latitude<-fulllatitude[-length(fulllatitude)] + diff(fulllatitude)/2 #Make latitude values the mean


###### Create the time and temp curves ----
TIME=seq(1,n.day,len=iter) #Time vector

####### Take the average of the climatological sst for each latband 
lat_t_mean<-matrix(nrow=length(ttime),ncol=bx) #For each box set up an empty vector of length time
#From the data file, take the mean temperature between each latitude band
for (i in 2:length(fulllatitude)){
  latband<-which(tlat>=fulllatitude[i-1] & tlat<=fulllatitude[i])
  lat_t_mean[,i-1]<-apply(tsst[,latband,],3,function(x){mean(x,na.rm=T)})
}
#Repeat the yearly temp cycle for the number of years set
trep<-do.call("rbind", rep(list(lat_t_mean), years))
tnum<-cbind(c(1:(365*years)),trep)

#Discretize the temperature values to the TIME vector
mlist<-apply(tnum,2,function(x){approx(x,xout=TIME)})
ttot<-cbind(mlist[[2]]$x,mlist[[2]]$y)
for (c in 3:(bx+1)){
  ttot<-cbind(ttot,mlist[[c]]$y)
}

TEMP<-ttot[,2:ncol(ttot)]

###### Run Model-----
    #Model Conditions
    
variable=1
interaction=1
    
  if (variable==0){
    st.v<-apply(TEMP,2,function(x){mean(x,na.rm=T)})
    TEMP<-matrix(rep(st.v,nrow(TEMP)),ncol=bx,byrow=T)
  }else{
    TEMP<-ttot[,2:ncol(ttot)]
  }
    
    
    ###### Growth Rate Curves -----
    ###### Setting up the growth curves ----
    mu.t=array(data=NaN,dim=c(length(TIME),n,bx)) 
    mu.t.2=array(data=NaN,dim=c(length(TIME),n,bx)) 
    
    z.vals<-seq(-4,40,length.out=n)
    z=matrix(rep(z.vals,bx),bx,n,byrow=TRUE)
    w=matrix(10,bx,n,byrow=TRUE) #c(11,10) 
    
    for (j in 1:n){
      mu.t[,j,]=0.81*exp(0.0631*TEMP)*(1-((TEMP-z[,j])/(w[,j]/2))^2)
    }
    mu.t[mu.t<=0]=0
    
    
    
    #### Setting up dispersal -----
    ######Create an empty matrix that is bx by bx dimensions
    taumatrix0=matrix(0,nrow=bx,ncol=bx)
    ######Name the rows and columns amu.ter the box numbers
    colnames(taumatrix0)=latitude
    rownames(taumatrix0)=latitude
    ######Take the difference between the rows and columns for the difference factor
    diffmatrix=abs(outer(as.numeric(colnames(taumatrix0)),as.numeric(rownames(taumatrix0)),'-'))
    colnames(diffmatrix)=latitude
    rownames(diffmatrix)=latitude
    ####Give realistic differences (1 degree latitude= 111km convert to m)
    diffmatrix<-diffmatrix*(111*1000)
    ########Create the tau matrix 
    #velocity<-0.1*(60*60*24) #0.1(m/s) speed #mixing vs. advection
    K.h<-(c(1E0,1E1,1E2,1E3,1E4))*(60*60*24) #m2/sec ###1E-4
    
    k.val<-4
    
      taumatrix2=K.h[kval]/(diffmatrix)^2 ## SQUARED ## units= day^-1
      taumatrix2[which(!is.finite(taumatrix2))]=0
      
      
      ##If there is no interaction then the taumatrix is just zeros
      if (interaction==1){
        taumatrix2=taumatrix2
      }else {
        taumatrix2=taumatrix0
        kval=0
      }
      
      ##Create the filename within this loop
      output.file.name<-paste('T',variable,'_D',interaction,'_E',kval,'siple_manuscript1',sep='')
      
      # #Caluclate R* and P* of these parameters
      # mu.timeavg<-array(apply(mu.t,3,function(x){apply(x,2,function(y){as.vector(by(y, ceiling(1:iter / oneyr_iter), function(z){mean(z,na.rm=T)}))})}),dim=c(years,n,bx))
      # R.star<-(m*k)/(mu.timeavg-m)
      # #R.star.box<-
      # P.star=(d*(R.0-R.star)*(R.star+k))/(mu.timeavg*R.star)
      # ####Just need to make sure the initial values for P and R are greater than P and R star???
      
      ####### Initial Values ----
      P=array(data=NaN,dim=c(iter,n,bx)) #row,column,box
      P_init=0.5 #1E-2 #0.5  #10^-2 #cells/mL
      P[1,,]=P_init
      R=array(data=NaN,dim=c(iter,1,bx))
      R_init=0.5 #2 #umolP/L
      R[1,,]=R_init
      
      ####Create arrays for that are time vs number of species and then repeated for each box ----
      growth=array(data=NaN,dim=c(iter,n,bx))
      decay=array(data=NaN,dim=c(iter,n,bx))
      tausum=array(data=NaN,dim=c(iter,n,bx))
      tausub=array(data=NaN,dim=c(iter,n,bx))
      
      ###### Run the model ----
      for (i in 2:iter){
        
        P2=array(apply(P[i-1,,],1,function(x){rep(x,each=bx)}),dim=c(bx,bx,n)) 
        
        tausum[i-1,,]<-t(apply(P2,3,function(x){apply(x*taumatrix2,1,function(y){sum(y,na.rm=T)})}))
        tausub[i-1,,]<-t(apply(P2,3,function(x){apply(x*taumatrix2,2,function(y){sum(y,na.rm=T)})}))
        
        # #P[i,,]=P[i-1,,]+t.step*((t(t(mu.t[i-1,,]*P[i-1,,])*(R[i-1,,]/(R[i-1,,]+k))))-(m*(P[i-1,,]^P.alpha)))
        # #R[i,,]=R[i-1,,]+t.step*(d*(R.0-R[i-1,,])-apply(((t(t(mu.t[i-1,,]*P[i-1,,])*(R[i-1,,]/(R[i-1,,]+k))))),2,function(x){sum(x,na.rm=T)}))
        # P[i,,]=P[i-1,,]+t.step*((t(apply((mu.t[i-1,,]*P[i-1,,]),1,function(x){x*(R[i-1,,]/(R[i-1,,]+k))})))-(m*(P[i-1,,]^P.alpha))+tausum[i-1,,]-tausub[i-1,,])
        # R[i,,]=R[i-1,,]+t.step*(d*(R.0-R[i-1,,])-apply((t(apply((mu.t[i-1,,]*P[i-1,,]),1,function(x){x*(R[i-1,,]/(R[i-1,,]+k))}))),2,function(x){sum(x,na.rm=T)}))
        # # R[i,,]=R[i-1,,]+t.step*(d*(R.0-R[i-1,,])-apply(t((mu.t[i-1,,]*P[i-1,,])*(R[i-1,,]/(R[i-1,,]+k))),2,function(x){sum(x,na.rm=T)}))
        
        ###LOL you dumb dumb OKAY correct formula:
        P[i,,]=P[i-1,,]+t.step*(((mu.t[i-1,,]*P[i-1,,]) * matrix(rep(R[i-1,,]/(R[i-1,,]+k),each=n),nrow=n) ) - (m*(P[i-1,,]^P.alpha)) + tausum[i-1,,]-tausub[i-1,,])
        R[i,,]=R[i-1,,]+t.step*((d*(R.0-R[i-1,,]))-apply(((mu.t[i-1,,]*P[i-1,,]) * matrix(rep(R[i-1,,]/(R[i-1,,]+k),each=n),nrow=n) ),2,function(x){sum(x,na.rm=T)}))
        

        
      }
      
      save(latitude,mu.t,TIME,TEMP,years,n.day,iter,z,w,n,bx,P,R,growth,decay,tausum,tausub,
           file=paste(icloud,output.file.name,'.Rdata',sep=''))
    
####### end model ------
      
######### Use of the Thomas curve -------
ecotype.fund.df%>%
        drop_na(y)%>%
        group_by(variable)%>%
        group_map(~{
          nls.pass2<-nls2(formula= y~(a*exp(x*0.0631)*(1-((x-z)/(w/2))^2)),
                          data=.x,
                          start=list(z=quantile(.x$x,0.5),
                                     w=diff(quantile(.x$x,c(0.05,0.95))),
                                     a=max(.x$y,na.rm=T)),
                          algorithm = 'port',
                          control=list(eval.max=5000, iter.max=1000,warnOnly=T),
                          lower=list(z=-2,w=2,a=0.1),upper=list(z=40,w=35,a=1))
          
          
          z.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['z','Estimate'],summary(nls.pass2)$parameters['z','Std. Error']*sqrt(nrow(.x)))
          w.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['w','Estimate'],summary(nls.pass2)$parameters['w','Std. Error']*sqrt(nrow(.x)))
          a.bs<-rnorm(1000,mean=summary(nls.pass2)$parameters['a','Estimate'],summary(nls.pass2)$parameters['a','Std. Error']*sqrt(nrow(.x)))
          
          x.out<-seq(-2,40,by=0.01)
          mu.y<-mean(a.bs)*exp(x.out*0.0631)*(1-((x.out-mean(z.bs))/(mean(w.bs)/2))^2)
          mu.y[mu.y<0]=NA
          output<-data.frame(x=x.out,y=mu.y)

          return(data.frame(x=x.out,y=mu.y))
          
        },.groups='keep')%>%
        setNames(group_keys(ecotype.fund.df%>%
                              drop_na(y)%>%
                              group_by(variable))%>%pull(variable))%>%{.->>thomas.fund}
      
      lapply(1:4,function(c){
        out.df<-thomas.fund[[c]]
        out.df$y[out.df$x<abs.vals$val[abs.vals$variable==names(thomas.fund)[c]][1] | out.df$x>abs.vals$val[abs.vals$variable==names(thomas.fund)[c]][2]]=NA
        return(out.df)
      })%>%{.->>thomas.df}
      
      
      names(thomas.df)<-names(thomas.fund)
      
      
      do.call('rbind',lapply(thomas.df,function(c){
        c%>%
          drop_na(y)%>%
          summarise(x=.$x,y=cumsum(.$x)/sum(.$x))%>%
          summarise(mean=.$x[which.min(abs(.$y-0.5))],
                    min=.$x[which.min(abs(.$y-0.01))],
                    max=.$x[which.min(abs(.$y-0.99))])
      }))%>%
        mutate(variable=rownames(.))%>%{.->>thomas.vals}
      
      do.call('rbind',thomas.df)%>%
        mutate(variable=rownames(.))%>%
        separate(variable,into=c('variable'))%>%{.->>thomas.line}

      fund.quant.vals<-thomas.vals
      
      colnames(fund.quant.vals)[1:3]<-c('q.mean','q.min','q.max')
      fund.quant.vals$dat<-'fund'
      fund.quant.vals$CB<-'Lab'
      
      
      
      thomas.flip<-pivot_longer(thomas.vals,cols=c('mean','min','max'))
      fund.thomas.plot<-ggplot()+
        geom_point(data=ecotype.fund.df,aes(x=x,y=y))+
        geom_line(data=thomas.line,aes(x=x,y=y))+
        geom_vline(data=thomas.flip,aes(xintercept=value,col=name),linetype='dashed')+
        scale_color_manual(name='',values=c(max='blue',min='blue',mean='red'))+
        scale_x_continuous(name=bquote(Temperature~(degree*C)))+
        scale_y_continuous(name=bquote(Growth~rate~(day^-1)))+
        theme_bw(16)+
        theme(panel.grid=element_blank(),
              axis.title = element_text(size=12),
              axis.text = element_text(size=12,color='black'),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              plot.title = element_text(hjust=0.5, face='bold'),
              aspect.ratio=1,
              legend.position="none",legend.box = "horiztonal",
              strip.background = element_blank(),
              strip.text = element_text(size=12,face='bold',vjust=-1),
              legend.text = element_text(size=12),
              panel.spacing.x=unit(0.9, "lines"), 
              panel.spacing.y=unit(0.9,"lines"))+
        coord_cartesian(ylim = c(0,0.75),clip = 'off')+
        facet_wrap(~variable)+
        geom_text(data=letters.vals,aes(x = -2, y = 0.815, label = labels),fontface='bold')
      
      ggsave('~/Desktop/thomas-fundamental_new.tiff',fund.thomas.plot,dpi=600,width=6,height=6.1)

    
            
######### Comparison Thomas to realized niche -----
      do.call('rbind',raw.data.real%>%
                drop_na(c(x,y))%>%
                group_by(variable)%>%
                group_map(~{group_by(.,CB)%>%
                    group_map(~{
                      quantile(.$x,c(0.01,0.5,0.99))
                    },.groups='keep')%>%
                    setNames(group_keys(raw.data.real%>%
                                          drop_na(c(x,y))%>%
                                          group_by(CB))%>%pull(CB))%>%
                    do.call('rbind',.)%>%
                    as.data.frame(.)%>%
                    mutate(CB=rownames(.))
                })%>%
                setNames(group_keys(raw.data.real%>%
                                      drop_na(c(x,y))%>%
                                      group_by(variable))%>%pull(variable)))%>%
        as.data.frame(.)%>%
        mutate(variable=rownames(.))%>%
        separate(variable,c('variable','ex'))%>%
        dplyr::select(-ex)%>% {. ->> real.CB.quant.vals}
      colnames(real.CB.quant.vals)[1:3]<-c('q.min','q.mean','q.max')
      real.CB.quant.vals$dat='real'
      
###### Compare fundamental and realized niches ------
      for (j in 1:4){
        e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
        raw.ts<-subset(fund.quant.vals,variable==e.df)$q.mean
        for (i in 1:4){
          cb.df<-c('NA','NP','SA','SP')[i]
          raw.x<-subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.mean
          real_data <- boot.full.real.CB.means[[eval(e.df)]][[eval(cb.df)]]
          real_data2<-real_data[order(real_data)]
          real_df_cumsum<-data.frame(x=real_data2,y=cumsum(real_data2)/sum(real_data2))
          conf.val<-real_df_cumsum$y[which.min(abs(real_df_cumsum$x-raw.ts))]
          if (raw.ts>raw.x){
            conf.val<-conf.val
          }else{
            conf.val<-1-conf.val
          }
          t.test.val<-t.test(real_data,mu=raw.ts,exact=T)
          if (i==1){
            ks.df<-data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value)
          }else{
            ks.df<-rbind(ks.df,data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value))
          }
        }
        if(j==1){
          ks.all.cb_lab.means<-ks.df
        }else{
          ks.all.cb_lab.means<-rbind(ks.all.cb_lab.means,ks.df)
        }
      }
      ks.all.cb_lab.means$sigmeans<-'ns'
      ks.all.cb_lab.means$sigmeans[(1-ks.all.cb_lab.means$p_val_c)<0.05]='*'
      ks.all.cb_lab.means$sigmeans[(1-ks.all.cb_lab.means$p_val_c)<0.01]='**'
  
      for (j in 1:4){
        e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
        raw.ts<-subset(fund.quant.vals,variable==e.df)$q.max-subset(fund.quant.vals,variable==e.df)$q.min
        for (i in 1:4){
          cb.df<-c('NA','NP','SA','SP')[i]
          raw.x<-subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.max - subset(real.CB.quant.vals,variable==e.df & CB==cb.df)$q.min
          real_data <- boot.full.real.CB.widths[[eval(e.df)]][[eval(cb.df)]]
          real_data2<-real_data[order(real_data)]
          real_df_cumsum<-data.frame(x=real_data2,y=cumsum(real_data2)/sum(real_data2))
          conf.val<-real_df_cumsum$y[which.min(abs(real_df_cumsum$x-raw.ts))]
          if (raw.ts>raw.x){
            conf.val<-conf.val
          }else{
            conf.val<-1-conf.val
          }
          t.test.val<-t.test(real_data,mu=raw.ts,exact=T)
          if (i==1){
            ks.df<-data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value)
          }else{
            ks.df<-rbind(ks.df,data.frame(variable=e.df,CB=cb.df,p_val_c=conf.val,t.test=t.test.val$p.value))
          }
        }
        if(j==1){
          ks.all.cb_lab.widths<-ks.df
        }else{
          ks.all.cb_lab.widths<-rbind(ks.all.cb_lab.widths,ks.df)
        }
      }
      ks.all.cb_lab.widths$sigwidths<-'ns'
      ks.all.cb_lab.widths$sigwidths[(1-ks.all.cb_lab.widths$p_val_c)<0.05]='*'
      ks.all.cb_lab.widths$sigwidths[(1-ks.all.cb_lab.widths$p_val_c)<0.01]='**'
      
      
      error.CI.5<-data.frame(do.call('rbind',lapply(boot.full.real.CB.means,function(x){unlist(lapply(x,function(c){quantile(c,c(0.05))}),use.names=T)})))
      error.CI.95<-data.frame(do.call('rbind',lapply(boot.full.real.CB.means,function(x){unlist(lapply(x,function(c){quantile(c,c(0.95))}),use.names=T)})))
      colnames(error.CI.5)<-c('NA','NP','SA','SP')
      colnames(error.CI.95)<-c('NA','NP','SA','SP')
      CI.means.5<-error.CI.5%>%
        mutate(variable=rownames(.))%>%
        gather(.,CB,val,-variable)%>%
        mutate(key='CI.5',se=val)%>%
        dplyr::select(.,-val)
      CI.means.95<-error.CI.95%>%
        mutate(variable=rownames(.))%>%
        gather(.,CB,val,-variable)%>%
        mutate(key='CI.95',se=val)%>%
        dplyr::select(.,-val)
      
      CI.all.mean<-rbind(CI.means.5,CI.means.95)%>%
        group_by(variable)%>%
        arrange(key,.by.group=T)%>%
        pivot_wider(.,names_from='key',values_from='se')
      
      
      error.CI.fund<-data.frame(do.call('rbind',lapply(boot.full.fund.means,function(x){quantile(x,c(0.05,0.95))})))
      colnames(error.CI.fund)<-c('CI.5','CI.95')
      error.CI.fund.means<-error.CI.fund%>%
        mutate(variable=rownames(.))
      
      
      error.CI.5.width<-data.frame(do.call('rbind',lapply(boot.full.real.CB.widths,function(x){unlist(lapply(x,function(c){quantile(c,0.05)}),use.names=T)})))
      error.CI.95.width<-data.frame(do.call('rbind',lapply(boot.full.real.CB.widths,function(x){unlist(lapply(x,function(c){quantile(c,0.95)}),use.names=T)})))
      colnames(error.CI.5.width)<-c('NA','NP','SA','SP')
      colnames(error.CI.95.width)<-c('NA','NP','SA','SP')
      CI.width.5<-error.CI.5.width%>%
        mutate(variable=rownames(.))%>%
        gather(.,CB,val,-variable)%>%
        mutate(key='CI.5.w',se=val)%>%
        dplyr::select(.,-val)
      CI.width.95<-error.CI.95.width%>%
        mutate(variable=rownames(.))%>%
        gather(.,CB,val,-variable)%>%
        mutate(key='CI.95.w',se=val)%>%
        dplyr::select(.,-val)
      
      
      CI.all.width<-rbind(CI.width.5,CI.width.95)%>%
        group_by(variable)%>%
        arrange(key,.by.group=T)%>%
        pivot_wider(.,names_from='key',values_from='se')
      
      error.CI.fund.width<-data.frame(do.call('rbind',lapply(boot.full.fund.widths,function(x){quantile(x,c(0.05,0.95))})))
      colnames(error.CI.fund.width)<-c('CI.5.w','CI.95.w')
      error.CI.fund.width<-error.CI.fund.width%>%
        mutate(variable=rownames(.))
      
      fund.combo<-left_join(error.CI.fund.means,error.CI.fund.width)
      fund.combo$CB='Lab'
      join.1<-rbind(as.data.frame(left_join(CI.all.width,CI.all.mean)),fund.combo)
      all.combo.vals<-left_join(all.quant.vals,join.1,by=c('variable','CB'))
      
      all.combo.vals<-left_join(all.combo.vals,sig.vals.CB.lab)
      
      
      all.quant.vals<-rbind(real.CB.quant.vals,do.call('rbind',replicate(4,fund.quant.vals,simplify=F)))
      all.quant.vals<-all.quant.vals[order(all.quant.vals$variable),]
      all.quant.vals$width<-all.quant.vals$q.max-all.quant.vals$q.min
      
      
      values.signif<-ggplot()+
        geom_segment(data=subset(all.quant.vals,dat=='fund'),
                     aes(x=subset(all.quant.vals,dat=='fund')$width,
                         y=subset(all.quant.vals,dat=='fund')$q.mean,
                         xend=subset(all.quant.vals,dat=='real')$width,
                         yend=subset(all.quant.vals,dat=='real')$q.mean),
                     arrow = arrow(length = unit(0.1,"cm")))+
        # geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,ymin=CI.5,ymax=CI.95))+
        # geom_errorbar(data=all.combo.vals,aes(x=width,y=q.mean,xmin=CI.5.w,xmax=CI.95.w))+
        geom_point(data=all.quant.vals,aes(x=width,y=q.mean,shape=CB,col=CB,fill=CB),size=4)+
        # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NA')),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
        # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable!='eMIT9313'),aes(x=width-1.5,y=q.mean-0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
        # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('SP') & variable=='eMIT9313'),aes(x=width+1.7,y=q.mean+0.8,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
        # geom_text(data=subset(all.combo.vals,dat=='real' & CB %in% c('NP','SA')),aes(x=width+1.5,y=q.mean+0.5,label=paste(sigmeans,sigwidths,sep="/")),size=5)+
        scale_x_continuous(name=bquote(Niche~width~(degree*C)))+
        scale_y_continuous(name=bquote(Mean~temperature~(degree*C)))+
        scale_shape_manual(name='',
                           values = c('Lab' = 16, 'NA' = 24,
                                      'SA'= 25,
                                      'NP' = 24,
                                      'SP'= 25))+
        scale_color_manual(name='',values=c('Lab'='black',
                                            'NP'='red',
                                            'NA'='blue',
                                            'SP'='red',
                                            'SA'='blue'))+
        scale_fill_manual(name='',values=c('Lab'='black',
                                           'NP'='red',
                                           'NA'='blue',
                                           'SP'='white',
                                           'SA'='white'))+
        theme_bw(16)+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title = element_text(size=12),
              axis.text = element_text(size=12,color='black'),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.title = element_text(hjust=0.5, face='bold'),
              aspect.ratio=1,
              legend.position="bottom",legend.box = "horiztonal",
              strip.background = element_blank(),
              strip.text = element_text(size=12,face='bold',vjust=-1),
              legend.text = element_text(size=12),
              panel.spacing.x=unit(0.5, "lines"), 
              panel.spacing.y=unit(0,"lines"))+
        facet_wrap(.~factor(variable,levels=c('eMED4','eMIT9312','eMIT9313','eNATL2A')))+
        coord_cartesian(xlim=c(10,21),y=c(16,26),clip='off')+
        geom_text(data=letters.vals,aes(x = 10, y = 27, label = labels),fontface='bold')
      
      ggsave('~/Desktop/thomas-rawdata_summary.tiff',values.signif,dpi=600,width=7,height=7.1)
      
           
###### Compare realized to realized -------
    for (j in 1:4){
        e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
        for (i in 1:4){
          full.re<-c('NA','NP','SA','SP')
          cb.df<-full.re[i]
          real_data_comp <- boot.full.real.CB.means[[eval(e.df)]][[eval(cb.df)]]
          for (b in 1:3){
            sub.cb<-full.re[!full.re==cb.df][b]
            real_data <-  boot.full.real.CB.means[[eval(e.df)]][[eval(sub.cb)]]
            
            id=paste(e.df,paste(cb.df,sub.cb,sep='.'),sep='_')
            sim.test<-wilcox.test(real_data_comp, real_data,'two.sided')

            if (b==1){
              out.df<-data.frame(id=id,wil_p=sim.test$p.value)
            }else{
              out.df<-rbind(out.df,data.frame(id=id,wil_p=sim.test$p.value))
            }
          }
          if (i==1){
            ks.df<-out.df
          }else{
            ks.df<-rbind(ks.df,out.df)
          }
        }
        if(j==1){
          ks.all.cb_cb.mean<-ks.df
        }else{
          ks.all.cb_cb.mean<-rbind(ks.all.cb_cb.mean,ks.df)
        }
      }
      ks.all.cb_cb.mean$sig.wils<-'ns'
      ks.all.cb_cb.mean$sig.wils[ks.all.cb_cb.mean$wil_p<0.05]='*'
      ks.all.cb_cb.mean$sig.wils[ks.all.cb_cb.mean$wil_p<0.01]='**'
      
      ####Widths
      for (j in 1:4){
        e.df<-c('eMED4','eMIT9312','eMIT9313','eNATL2A')[j]
        for (i in 1:4){
          full.re<-c('NA','NP','SA','SP')
          cb.df<-full.re[i]
          real_data_comp <- boot.vals.real.width.CB[[eval(e.df)]][[eval(cb.df)]]$t[,3]
          for (b in 1:3){
            sub.cb<-full.re[!full.re==cb.df][b]
            real_data <-  boot.vals.real.width.CB[[eval(e.df)]][[eval(sub.cb)]]$t[,3]
            id=paste(e.df,paste(cb.df,sub.cb,sep='.'),sep='_')
            sim.test<-wilcox.test((real_data_comp), (real_data),'two.sided',exact=T)
            if (b==1){
              out.df<-data.frame(id=id,wil_p=sim.test$p.value)
            }else{
              out.df<-rbind(out.df,data.frame(id=id,wil_p=sim.test$p.value))
            }
          }
          if (i==1){
            ks.df<-out.df
          }else{
            ks.df<-rbind(ks.df,out.df)
          }
        }
        if(j==1){
          ks.all.cb_cb.width<-ks.df
        }else{
          ks.all.cb_cb.width<-rbind(ks.all.cb_cb.width,ks.df)
        }
      }
      ks.all.cb_cb.width$sig.wils<-'ns'
      ks.all.cb_cb.width$sig.wils[ks.all.cb_cb.width$wil_p<0.05]='*'
      ks.all.cb_cb.width$sig.wils[ks.all.cb_cb.width$wil_p<0.01]='**'
      

      
         
      
      all.quant.2<-all.quant.vals[all.quant.vals$dat=='real',]
      all.quant.2$fund.mean<-all.quant.vals[all.quant.vals$dat=='fund','q.mean']
      all.quant.2$fund.width<-all.quant.vals[all.quant.vals$dat=='fund','width']
      all.quant.2$diff.mean<-all.quant.2$fund.mean-all.quant.2$q.mean
      all.quant.2$diff.width<-all.quant.2$fund.width-all.quant.2$width
      
      apply(all.quant.2[,c('diff.mean','diff.width')],2,mean)
      



########## -----



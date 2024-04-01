test7 <- function() {
  ## Make the figure 1 in the introduction section

  par(mgp=c(2,0.5,0.5))
  plot.new()
  plot.window(xlim = c(0,10),ylim=c(0,0.5))
  axis(1,at=c(0,2,8.1,10),labels = c("t-l","t","T-l+1","T"),padj=0,tcl=0.5,lwd = 2,cex.axis=1.5)
  abline(v=8.1,lty=2,lwd=2)
  arrows(10,0.1,0,0.1,length = 0.2,angle = 20,lwd = 2)
  text(4,0.12,"1-Back-calculation",cex = 1.4)
  arrows(8.1,0.15,10,0.15,length = 0.2,angle = 20,lwd = 2)
  text(9,0.17,"2-Nowcasting",cex=1.4)

  ## Save tiff file
  tiff("fig1.tiff", units="in", width=7.5, height=4, res=300)
  par(mgp=c(2,0.5,0.5))
  plot.new()
  plot.window(xlim = c(0,10),ylim=c(0,0.5))
  axis(1,at=c(0,2,8.1,10),labels = c("t-l","t","T-l+1","T"),padj=0,tcl=0.5,lwd = 1,cex.axis=1)
  abline(v=8.1,lty=2,lwd=1)
  arrows(10,0.1,0,0.1,length = 0.1,angle = 20,lwd = 1)
  text(4,0.12,"1-Back-calculation",cex = 0.85)
  arrows(8.1,0.15,10,0.15,length = 0.1,angle = 20,lwd = 1)
  text(9,0.17,"2-Nowcasting",cex=0.85)
  dev.off()

  ## Extract the geweke's diagnostics
  g <- readRDS('geweke.rds')
  g <- rbind(g$g1,g$g2,g$g3,g$g4,g$g5,g$g6)
  apply(g,2,mean) ## On average, 92% of the daily count and reproduction number estimates passed the geweke's test for convergence


  ##############################################
  ## Generate supplementary figure 1, 2 and 3 ##
  ##############################################

  ## Read the processed output
  outc <- readRDS('count.rds')
  outr <- readRDS('rest.rds')
  library(tidyverse)
  library(ggpubr)

  outc <- outc%>%filter(date>20)%>%mutate(date=date-20)
  ## Assuming the first day is 2020-02-01 and the last day is 2020-03-31
  outc <- outc%>%mutate(date=as.Date('2020-01-31')+date)
  outr <- outr%>%filter(date>20)%>%mutate(date=date-20)
  outr <- outr%>%mutate(date=as.Date('2020-01-31')+date)

  outc[outc$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"

  outr[outr$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"

  out1 <- outc%>%filter(sn==1|sn==2)

  out1%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
    geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") + geom_line(aes(x=date,y=epic),size=1.2)+
    theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                     legend.text = element_text(size=11)) +labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, ncol = 2)

  out1 <- outr%>%filter(sn==1|sn==2)

  out1%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
    annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
    geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") + geom_line(aes(x=date,y=epir),size=1.2)+
    theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                     legend.text = element_text(size=11)) + labs(x = "Date", y = "Reproduction Number")+facet_wrap(~scenario, ncol = 2)


}

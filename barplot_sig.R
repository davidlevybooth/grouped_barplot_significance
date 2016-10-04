soil_barplot <- function(formula, data, label) {
   #for Tukey letters
  
  require(multcompView)
  require(ggplot2)
  

  #Standard Error function for data aggregation
  st.err <- function(x, na.rm=FALSE) {
    if(na.rm==TRUE) x <- na.omit(x)
    sd(x)/sqrt(length(x))
  }
  
  #Significance value -> symbol function
  p.sym <- function(Pr){ 
    if (Pr <= 0.001) { x<-"***" } 
    else if (Pr <= 0.01) {x<-"**" } 
    else if (Pr <=0.05) { x<-"*" } 
    else {x <-"NS" }
    return(as.character(x))}
  
  #Summarize data - mean and SE
  SUM <- aggregate(formula, data, mean); y_i <- length(SUM)  
  SUM$SE <- aggregate(formula, data, st.err)[,y_i]; se_i <- length(SUM)
  
  #positions for plot annotation
  SUM$pos <- ((SUM[,y_i] + SUM[,se_i])*1.05)
  y_max <- max(SUM$pos)*1.25
  y_line <- max(SUM$pos)*1.05
  y_top <- signif(y_max, 1)
  
  #Housekeeping to ensure a "y ~ A + B + C" formula used for now
  if(y_i == 4) {
    y_term = names(SUM)[y_i]
    x1_term = names(SUM)[1]
    x2_term = names(SUM)[2]
    x3_term = names(SUM)[3]
    form1 = as.formula(paste(y_term,"~",x1_term))
    form2 = as.formula(paste(y_term,"~",x2_term))
    
  }
  else {stop("Only works for 3 term Formula, Sorry")}
  
  #1-way ANOVA and Tukey on A and B terms
  AV1 <- aov(form1, data=data)
  AV2 <- aov(form2, data=data)
  tHSD1 <- TukeyHSD(AV1, ordered = FALSE, conf.level = 0.95)
  tHSD2 <- TukeyHSD(AV2, ordered = FALSE, conf.level = 0.95)
  Pval <- tHSD2[[x2_term]][,4]
  Psym <- p.sym(Pval) 

  
  #Convert Tukey significance to letters and match to data summary table
  Tukey.levels <- tHSD1[[x1_term]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  SUM <- SUM[match(names(Tukey.labels[['Letters']]), SUM[,1]),]
  SUM$Letters <- Tukey.labels[['Letters']]
  
  #Plot that shit
  SUM_plot <-
    ggplot(SUM, aes(x=SUM[,2], y=SUM[4], fill=SUM[,3])) +
    geom_bar(position=position_dodge(), stat="identity", colour='black') +
    scale_y_continuous(expand = c(0,0))+
    geom_errorbar(aes(ymin=SUM[4]-SE, ymax=SUM[4]+SE), width=0,position=position_dodge(.9)) +
    scale_fill_manual(values = c("grey80", "grey20")) +
    theme_bw() + theme(panel.grid = element_blank(), legend.title=element_blank()) +
    coord_cartesian(ylim = c(0, y_top)) + 
    ylab(label) +
    xlab(paste(x2_term))
  
  
  SUM_plot <- SUM_plot + 
    geom_text(data = SUM, aes(y = pos, label = Letters), position=position_dodge(.9)) +
    geom_line(aes(y = y_line, group = 1)) + 
    annotate("text", x = 1.5, y = y_line*1.02, label = Psym)
  
  return(SUM_plot)
}
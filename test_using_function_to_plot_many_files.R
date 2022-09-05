library(raster)
library(tidyverse)
library(viridis)
library(ggthemes)
library(colorspace)
library(ggplot2)

filelist <- c("data/EEA_10km_o2s_CMEMS_001_029_o2s.csv",
              "data/EEA_10km_o2t_CMEMS_001_029_o2t.csv",
              "data/EEA_10km_phs_CMEMS_001_029_phs.csv",
              "data/EEA_10km_sbs_CMEMS_001_031_sbs.csv",
              "data/EEA_10km_sss_CMEMS_001_031_sss.csv")

my_plot <- function(file){
  df<- read.table(file,header=T,sep=",")
  
  var_name <- substr(file,15,17)
  
  dfl <- pivot_longer(df,
                      cols = starts_with("X2"),
                      names_to = c("Year"),
                      values_to = "value")
  dfl <- dfl %>%
    mutate(Year=as.numeric(substr(Year,2,5)))
  
  dfmean <- dfl %>% 
    group_by(E,N) %>%
    summarise(mean=mean(value,na.rm=T)) %>%
    ungroup()
  
  dfl <- dfl %>%
    left_join(dfmean,by=c("E","N")) 
  
  dfl <- dfl %>%
    mutate(anomaly=value-mean)
  
  dfl <- dfl %>%
    mutate(anomaly_trunc=ifelse(anomaly>2,2,ifelse(anomaly< -2,-2,anomaly)))
  
  p <- ggplot() +  
    labs(subtitle=paste0("Mean ",var_name)) +
    geom_tile(data=dfl, aes(x=E, y=N, fill=value), alpha=0.8)  +
    scale_fill_viridis() +
    coord_equal() +
    theme_map() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm"))
    facet_wrap(.~Year,ncol=4)
  
  plot_file_name <- paste0("plot_mean_",var_name,".png")
  ggsave(p,file=plot_file_name,dpi=300,units="cm",width=20,height=16)
  
  return(p)
}

for(file in filelist){
  print(paste0("Now plotting ",file))
  my_plot(file)
}

my_histplot <- function(file){
  
  
  
  
  
  
  
  
}






## The Impact of Motorway Network Development on Urban Population Growth in the Netherlands
## Author: Or Levkovich
## Date :         24.06.2016
## Latest update: 30.07.2018
## ------------------------------ #

# Libraries ---------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(data.table); library(lattice); library(msm); 
library(ggplot2)   # famous r plugin for graphing. Also extensively described in 'r graphics cookbook'.
library(car) # Necessary for DeltaMethod functions
library(stargazer) # Library for exporting tables
library(bbmle) # Maximum-likelihood estimation
library(ggmap); library(rgdal);  library(broom); library(rgeos); # for mapping
library(gridExtra); library(gtable) # For combine graphs
library(readstata13) # read stata files (.dta)
library(AER);library(Hmisc);library(ivpack);library(gtools);
library(sf)#;library(raster);


# Expansion of the motorway network ---------------------------------------

  snelwegen <-read.delim("H:/Desktop/0 - ERC/Data/R/tables/snelwegen.csv", sep=";")  ### Source : Ligtermoet, D. M. (1990). Beleid en planning in de wegenbouw , Statistics Netherlands (2016)
  personautos<- read.delim("H:/Desktop/0 - ERC/Data/R/tables/personautos.csv", sep=";") ### Source : CBS: 'Historie verkeer en vervoer vanaf 1899'--> 'Wegvoertuigen stand op 1 januari'
  personautos$pop <- personautos$pop*1000000
  personautos$autos <- personautos$autos*1000
  personautos$ratio = 1000*personautos$autos/personautos$pop
  
  snelwegen$lengte_focus <- NA
  snelwegen$lengte_focus[which(snelwegen$Year %in% seq(1960,1972,1))] <- snelwegen$lengte[which(snelwegen$Year %in% seq(1960,1972,1))]
  snelwegen<- merge(snelwegen,personautos,by.x="Year",by.y="year")
  snelwegen <- subset(snelwegen,snelwegen$Year<=1990)
  
  snelwegen.gph <-
    ggplot(snelwegen) + aes(x=Year) +
    geom_bar(aes(y=lengte, fill="Length of highways"), stat="identity", alpha=0.5) +
    geom_bar(aes(y=lengte_focus, fill="Expansion period"), stat="identity",  alpha=1) +
    scale_fill_manual(name="",values=c("Length of highways"="grey50","Expansion period"="grey30")) + # darkolivegreen4
    geom_line(aes(y=10*ratio, color="Number of vehicles for 10,000 people"), size = 1) +
    scale_color_manual(name="",values=c("Number of vehicles for 10,000 people"="black")) +
    # geom_line(aes(y=autos, color="Private vehicles (thousands)"), size = 1) +
    # scale_color_manual(name="",values=c("Private vehicles (thousands)"="red")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
    coord_cartesian(xlim=c(1945,1990)) + xlab("Year") + ylab("km") # +
    # labs(title = "Highway network length and car ownership in the Netherlands",
    #     subtitle = "(1945-1990)")
  ggsave(file = "H:/Desktop/0 - ERC/Data/R/tables/snelwegenLengte.tiff", snelwegen.gph);
  ggsave(file = "H:/Desktop/0 - ERC/Data/R/tables/snelwegenLengte.png", snelwegen.gph);
  

  
  
# Read data and prepare for R analysis  -------------------------------------------------------------
  
  for (range in 50000){  #for (range in seq(30000,70000,10000)){ # for (range in 50000){ 
  
  data <- read.dta13(paste0("H:/Desktop/0 - ERC/Data/stata/stata_files/hug_1980wf_",range,".dta")) # hug_1980wf_30000,hug_1980wf_40000,hug_1980wf_50000,hug_1980wf_60000,hug_1980wf_70000
  
    ### New variables (July 2017)
  
      # Share of reclaimed land
        reclaimed <- read.delim("H:/Desktop/0 - ERC/Data/GIS/Municipal borders_jasper/Hist_Pop in 1980 gem_borders/reclaimed_since1930.txt", sep = ",")      
        reclaimed <- reclaimed %>% select(code,Shape_Area) %>% rename(reclaimed_area = Shape_Area)
        data <- data %>% left_join(reclaimed, by='code') %>% mutate(reclaimed_area = ifelse(is.na(reclaimed_area),0,reclaimed_area),
                                                                    reclaimed = reclaimed_area/exp(shape_area),
                                                                    reclaimed = ifelse(reclaimed<0.1,0,reclaimed))
        write.table(data[c("code","reclaimed")], file = "H:/Desktop/0 - ERC/Data/stata/stata_files/reclaimed.csv", sep=",", row.names = FALSE)
        rm(reclaimed)
        
        gebieden.1830 <-read.delim("H:/Desktop/0 - ERC/Data/stata/stata_files/Gebieden_overzicht_1830_v2.csv", sep=";")
        gebieden.1830 <- gebieden.1830 %>% select(code, Provincie) %>%
          mutate(code = as.numeric(gsub("[A-z]","",gebieden.1830$code)))
        data <- data %>% left_join(gebieden.1830, by='code')
        rm(gebieden.1830)
        
      # Pop_t1980_1960 
        data$pop_t1980_1960 <- data$pop_1980-data$pop_1960

      # Generate new buffer instrument (August 2017)
          # library(sf) # https://edzer.github.io/UseR2017/
          nl.80 = read_sf("H:/Desktop/0 - ERC/Data/GIS/Municipal borders_jasper/municipalities/nl_1980.shp")
          nl.80 = nl.80 %>% mutate(code= as.numeric(GM_1980)) 
              # nl.80 %>% left_join(data,by="code") %>% filter(nota2_buff>0) %>% select(nota2_buff) %>% plot()
          cc = nl.80 %>% left_join(data,by="code") %>% filter(pop_1930>log(50000)) %>% select(pop_1930)
          restricted.80 <- nl.80 %>% left_join(data,by="code") %>% mutate(restricted = (nota2_buff>0 & !is.na(nota2_buff)) ) %>% filter(restricted==1) %>% select(code, restricted) 
          
          buffer = nl.80 %>% left_join(data,by="code") %>% filter((pop_1930>log(150000) | code==935)) %>% select(pop_1930) %>% 
            st_union() %>% st_buffer(15000) %>%  st_difference(st_union(cc))
          buffer.intersect = nl.80 %>% select(code, Shape_Area) %>% st_intersection(buffer) %>%
            mutate(buffer = as.numeric(st_area(.)), buffer_instrument = buffer/Shape_Area, buffer_instrument = ifelse(buffer_instrument>0.9,1,0))  %>% select(code, buffer_instrument)
              # buffer.intersect %>% select(buffer_instrument) %>% filter(buffer_instrument>0) %>% plot()
          
      # Belts
          beltsize = 10000
          beltsize.min = 0 # 5000
          # GIS Buffer around restricted zones   (June 2018)
              buff.belt <- nl.80 %>% left_join(data,by="code") %>% filter( (nota2_buff>0 & !is.na(nota2_buff))  ) %>% 
                st_union() %>% st_buffer(beltsize) %>%  st_difference(st_union(cc))  %>% st_difference(st_union(restricted.80)) 
              belt.buff.int <- nl.80 %>% select(code, Shape_Area) %>% st_intersection(buff.belt) %>%
                mutate(buffer = as.numeric(st_area(.)), buffer_belt = buffer/Shape_Area, buffer_belt = ifelse(buffer_belt>0.9,1,0)) %>% select(code, buffer_belt)
              
          # GIS buffer around cc's  (June 2018)
              buff.belt.min <- nl.80 %>% left_join(data,by="code") %>% filter(cc_bs==1) %>% select(pop_1930) %>% 
                st_union() %>% st_buffer(beltsize.min) 
              buffer.wide = nl.80 %>% left_join(data,by="code") %>% filter(cc_bs==1) %>% select(pop_1930) %>% 
                st_union() %>% st_buffer(beltsize) %>%  st_difference(st_union(cc))  %>% st_difference(st_union(restricted.80)) %>% st_difference(st_union(buff.belt.min))
              buff.int.wide = nl.80 %>% select(code, Shape_Area) %>% st_intersection(buffer.wide) %>%
                mutate(buffer = as.numeric(st_area(.)), buffer_cc = buffer/Shape_Area, buffer_cc = ifelse(buffer_cc>0.9,1,0))  %>% select(code, buffer_cc) 
              
      # Merge with data
          data <- data %>% left_join(buffer.intersect, by='code') %>%
            left_join(belt.buff.int, by='code') %>%
            left_join(buff.int.wide, by='code') %>%
            left_join(restricted.80, by='code')
          data = data[-c(grep("geometry",names(data)))]
          
          data <- 
          data %>% mutate(buffer_instrument = ifelse(is.na(buffer_instrument), 0, 1),
                          buffer_belt = ifelse(is.na(buffer_belt), 0, 1),
                          buffer_cc = ifelse(is.na(buffer_cc), 0, 1),
                          restricted = ifelse(is.na(restricted), FALSE, TRUE),
                          belt = ifelse(buffer_cc==1 & buffer_belt !=1 & cc_bs==0 & nota2_buff==0, "_around_CC", 
                                        ifelse(buffer_cc==1 & buffer_belt ==1 & cc_bs==0 & nota2_buff==0, "_around_Nota2Buffer", "_Remote Periphery")))
          data <- data %>% mutate(belt = ifelse((code==93 | code==60 | code==88 | code==96 | code==448 | is.na(belt)), "_Remote Periphery", belt)) ## WADDEN ISLANDS, | code==46 : Rottumerplaat 
          data <- data %>% mutate(belt = ifelse(belt == "_Remote Periphery" & cc_bs!= 0, "Urban agglomeration", belt)) 
          data <- data %>% mutate(belt = ifelse(belt == "_Remote Periphery" & restricted == TRUE, "Restricted area", belt)) 
          
          table(data$belt)
          write.table(data[c("code","buffer_instrument","belt")], file = "H:/Desktop/0 - ERC/Data/stata/stata_files/buffer_instrument.csv", sep=",", row.names = FALSE)
          
          data.nl.80 <- nl.80 %>% mutate(code= as.numeric(GM_1980)) %>% select(code, geometry)%>% left_join(data,by="code") 
          if (range==50000){
                gph <- ggplot(data.nl.80) + 
                  geom_sf(data = data.nl.80, aes(fill = as.factor(cc_bs)), colour = "grey50", size = 0.1)  +
                  scale_fill_manual(name="", 
                                    values=c("#ffeda0", "#feb24c", "#ec7014"),
                                    labels = c("Periphery", "Suburbs", "Central city")) +
                  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), panel.grid = element_line(colour="white"), panel.background=element_rect(fill="white",colour="white"), legend.position = "right") 
                ggsave(gph, file = paste0("H:/Desktop/0 - ERC/Data/R/model_results/maps/Map_ccbs",range,".eps"), height = 200, width = 200, units = c("mm"))
            
                gph <- ggplot(data.nl.80) + 
                  geom_sf(data = data.nl.80, aes(fill = belt), colour = NA)  +
                  scale_fill_manual(name="", 
                                    values=c("#feb24c", "#ec7014", "#ffeda0",  "#238b45", "#d9d9d9"),
                                    labels = c("Near periphery", "Near periphery: Belt around buffer zones", "Remote periphery", "Restricted areas", "Urban agglomerations")) + #  ("Belt around central cities", "Belt around buffer zones", "Remote periphery", "Restricted areas", "Urban agglomerations")
                  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), panel.grid = element_line(colour="white"), panel.background=element_rect(fill="white",colour="white"), legend.position = "right") #+
                  # labs(caption = paste0("Belt width: ", beltsize/1000," kilometer around central cities."))
                ggsave(gph, file = paste0("H:/Desktop/0 - ERC/Data/R/model_results/maps/MapBelts_range",range,"_Belt",beltsize,".eps"), height = 200, width = 200, units = c("mm"))
          }
          rm(cc, buffer, buffer.intersect, buff.belt, belt.buff.int, buffer.wide, buff.int.wide, gph)

        # Weights
          data$weights <- as.integer(exp(data$pop_1930))
          W.mat <- diag(data$weights)

       ### Import spatial data
          cc.threshold <- 50000
          
          ## Import spatial data
          shp.mun.1980 <-  readOGR("H:/Desktop/0 - ERC/Data/GIS/Municipal borders_jasper/municipalities/nl_1980.shp")
          proj4string(shp.mun.1980)  ## verify map projection
          proj4string(shp.mun.1980)<-CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_defs")
          
          shp.mun.1980@data$id <- rownames(shp.mun.1980@data); shp.mun.1980.points <- tidy(shp.mun.1980); ## create region id's
          row.names(shp.mun.1980) <- as.character(shp.mun.1980@data$GM_1980)
          spmat <- gTouches(shp.mun.1980, byid=TRUE);      # Contiguity matrix
          
          # mapdf <- merge(shp.mun.1980.points,shp.mun.1980@data, by='id'); rm(shp.mun.1980.points, shp.mun.1980); ## merge id's and spatial dataframe into a new dataframe
          # mapdf$code <- as.numeric(as.character(mapdf$GM_1980))
          
          
          ### Join databases
          data.small <- data[,c("code","GM_NAAM","pop_1930","pop_dens_1930","hgn_nat1960","hgn_nat1960_mean")]
          # apdf$row.id<-as.numeric(rownames(mapdf)) ## create row id to prevent polygon mis-shaping
          # mapdf <- merge(mapdf,data.small,by='code',all.x=TRUE)
          # sum(is.na(mapdf$pop_1930))
          
          spmat <- cbind(as.numeric(row.names(spmat)),spmat)
          spmat <- merge(data.small,spmat, by.x='code', by.y='V1') 
          spmat <- subset(spmat, exp(spmat$pop_1930)>=cc.threshold & spmat$pop_dens_1930>500 )
          spmat <- spmat[,!(names(spmat) %in% c("code","GM_NAAM","pop_1930","pop_dens_1930","hgn_nat1960","hgn_nat1960_mean"))]
          spmat <- data.frame(code=as.numeric(names(spmat)), neighbors = colSums(spmat))
          table(spmat$neighbors)
          
          # mapdf<- merge(mapdf,spmat, by='code')
          # mapdf$bs <- "Periphery"
          # mapdf$bs[mapdf$neighbors>0] <- "Suburb" 
          # mapdf$bs[exp(mapdf$pop_1930)>=cc.threshold & mapdf$pop_dens_1930>500] <- "Central city"
          # mapdf$bs[mapdf$hgn_nat1960>10000 & mapdf$hgn_nat1960_mean>0.5]  <- "Periphery"
          # mapdf <- mapdf %>% arrange(row.id)
          # 
          # ### Plot maps        
          # ggplot() +  geom_polygon(data=mapdf, aes(x=long, y=lat, group=group, fill=bs), color = "black", size = 0.25) + 
          #   scale_fill_manual(name="", values=c("#f03b20","#ffeda0","#feb24c")) +
          #   coord_equal(ratio=1) + xlab("") + ylab("") +
          #   theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background=element_rect(fill="white",colour="black")) 
          
          # rm(data.small,mapdf)
          
          data<- merge(data,spmat,by='code',all.x=TRUE)
          
          ### Number of treatments (= for suburbs which are adjacent to multiple cc)
          data$ntr <- 1
          data$ntr[data$cc_bs==1] <- data$neighbors[data$cc_bs==1] 
          
      data$cc_dt <- as.factor(data$cc_dt)
      data$cc_bs <- as.factor(data$cc_bs)
      
      ## cc_bs scenario #2 (cc, suburbs, periphery, sub.res, per.res) (2018)
      data <- 
        data %>% mutate(cc_bs_2 = ifelse(cc_bs==1 & (gh_1958>0.5 | nota2_buff>0), "sub.res",
                                         ifelse(cc_bs==0 & (gh_1958>0.5 | nota2_buff>0), "per.res", 
                                                ifelse(cc_bs == 0 , "Periphery",
                                                       ifelse(cc_bs==1, "Suburb",
                                                              ifelse(cc_bs == 2, "CC",NA))))))
      
      # nl.80 %>% left_join(data,by="code") %>% select(cc_bs_2) %>% plot()
      
      # Distance to cc's and cc's themselves   
          nl.80.c <- st_centroid(nl.80)
          nl.80.c <- nl.80.c %>% left_join(data.small, by = "code") 
          nl.80.cc <- nl.80.c %>% filter(exp(pop_1930)>=cc.threshold & pop_dens_1930>500)
          mat <- st_distance(nl.80.c, nl.80.cc)
          mat.dist <- apply(mat, 1, min )
          mat.ind <- apply(mat, 1, function(x) which(x == min(x)))
          nl.80.c <- nl.80.c %>% 
            mutate(dist_cc_n = ifelse(mat.dist!=0,log(mat.dist),0),
                   nearest_cc_ind = nl.80.cc$code[mat.ind]) %>%
            select(code, dist_cc_n, nearest_cc_ind) %>% 
            left_join(data.small, by = c("nearest_cc_ind"="code")) %>% 
            select(code, dist_cc_n, nearest_cc_ind, pop_1930) %>%
            rename(ln_pop_cc = pop_1930)
          data <- data %>% left_join(nl.80.c, by='code') 
          data = data[-c(grep("geometry",names(data)))]
          
      rm(nl.80, data.nl.80, mat, mat.dist, mat.ind, nl.80.c, nl.80.cc)
    ### To estimate Kleibergen-Paap (2006) F-statistics run stata do file : 
    ### H:/Desktop/0 - ERC/Data/stata/hwy_1980_analysis/BS_DT_analysis_hwys_1980_ivreg.do
      
      saveRDS(data, file=paste0("H:/Desktop/0 - ERC/Data/data_1980_",range,".rds"))      
  } 
  # data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds") 
  # save.dta13(data, "H:/Desktop/0 - ERC/Data/stata/stata_files/data_1980_50000.dta")

# Read data ---------------------------------------------------------------

  data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds") 
  
  pop_t <- "pop_t1980"
  instruments <- c("Rays_1821_5k","length_hwys_1821") # c("Rays_1821_5k") # c("Rays_1821_5k","length_hwys_1821")
  inst.lm <- paste(instruments,collapse = " + ")
  soilvar = paste0(names(data)[grep("soiltype",names(data))],collapse="+")
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed + ")
  control.vars <- paste(control.vars," + ",soilvar)
  hwys = c("Rays_1970_5k","length_hwys_1970")
 
  
# Data descriptives -------------------------------------------------------
    
    data = readRDS(paste0("H:/Desktop/0 - ERC/Data/data_1980_50000.rds"))
    
  # Summary table
    summary.table <- data[,c("pop_t1980","Rays_1970_5k","length_hwys_1970","rail_stations_1930","pop_1930","pop_1960","nota2_buff","hgn_nat1960_mean","gh_1958","gr_area1966","dist_cc","reclaimed","Rays_1821_5k","length_hwys_1821","buffer_instrument","cc_dt","cc_bs")] ## "AP_1970","APd_1970"
    row.names(summary.table) <- data$code
    
    summary.table$pop_t1980 <- exp(summary.table$pop_t1980)-1
    summary.table$pop_t1980[summary.table$pop_t1980>9] <- NA
    # summary.table$pop_t1980[summary.table$pop_t1980==1] <- 0
    summary.table$pop_1930 <- exp(summary.table$pop_1930)
    summary.table$pop_1930[summary.table$pop_1930==1] <- 0
    summary.table$pop_1960 <- exp(summary.table$pop_1960)
    summary.table$pop_1960[summary.table$pop_1960==1] <- 0
    summary.table$length_hwys_1970 <- exp(summary.table$length_hwys_1970)
    summary.table$length_hwys_1970[summary.table$length_hwys_1970<=1] <- 0
    summary.table$length_hwys_1821 <- exp(summary.table$length_hwys_1821)
    summary.table$length_hwys_1821[summary.table$length_hwys_1821<=1] <- 0
    summary.table$dist_cc <- exp(summary.table$dist_cc)
    summary.table$dist_cc[summary.table$dist_cc<=1] <- 0
    
    
    stargazer(summary.table,subset(summary.table, cc_bs==2),subset(summary.table, cc_bs==1),subset(summary.table, cc_bs==0),
              type = "latex", title="Descriptive statistics", digits=3, nobs = FALSE,
              out="H:/Desktop/0 - ERC/Data/R/tables/descriptives_regions.tex"
              )
  
  # Summary table : buffer areas and green heart  
    summary.table2 <-data %>% group_by(cc_bs) %>% summarise(mean_Buffer=mean(nota2_buff), sd_Buffer=sd(nota2_buff), 
                                           mean_gh=mean(gh_1958), sd_gh=sd(gh_1958), 
                                           mean_gr=mean(gr_area1966), sd_gr=sd(gr_area1966)) 
    stargazer(format(summary.table2,digits=2), summary = FALSE, flip = TRUE, digits=4,
              type = "html", title="Spatial planning variables and urban level of municipalities",
              out="H:/Desktop/0 - ERC/Data/R/tables/descriptives_buffer_gh_gr.html",
              covariate.labels=c("","Periphery","Suburbs","Central city")
              )
    rm(summary.table,summary.table2); 
    
    cor(data$nota2_buff,(data$cc_bs==1))
    data %>% group_by(cc_bs) %>% summarise(tot_area = sum(shape_area), tot_share_buff = sum(nota2_buff*shape_area), share_buff = tot_share_buff/tot_area)
    cor(data$gh_1958,(data$cc_bs==1))
    data %>% group_by(cc_bs) %>% summarise(tot_area = sum(shape_area), tot_share_gh = sum(gh_1958*shape_area), share_gh = tot_share_gh/tot_area)
    
    dplyr::summarize(group_by(data,cc_bs), cor(length_hwys_1970, length_t1980))
    dplyr::summarize(group_by(data,cc_bs), cor(Rays_1970_5k, Rays_t1980))

    cor(data[c("Rays_1821_5k","length_hwys_1821","Rays_t1980","length_t1980")])
    
    
    
  ### * For growth areas analysis run file hwy_tables_gr_areas.R *  

# Highways and urban agglomeration (Duranton Turner (2012)) ----------------------------------------
      ## Highways and urban agglomeration 
      hwys = c("Rays_1970_5k","length_hwys_1970")
      for (hwy in hwys){
          ols_inst <- lm(paste0(hwy, " ~ cc_dt +",inst.lm,"+",control.vars), data=data)
          summary(ols_inst) 
          adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
          Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
          data$instr <- predict.lm(ols_inst)
          ols <- lm(paste0(pop_t, " ~ cc_dt +  cc_dt:instr +",control.vars), data=data)
          summary(ols)
          
            nn<- length(ols$residuals); nk <- ols$df.residual 
            y <- data$pop_t1980
            biv <- (ols$coefficients)
            x <- model.matrix(eval(parse(text=paste0(" ~ cc_dt + cc_dt:",hwy,"+",control.vars))), data=data)
            xh <- model.matrix(ols$model, data=data)
            rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
            adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
          assign(paste0("ols.dt.1st.",substr(hwy,1,1)),adj.se.ols_inst)
          assign(paste0("ols.dt.1st.R2.",substr(hwy,1,1)),summary(ols_inst)$adj.r.squared)
          assign(paste0("main.ols.dt",substr(hwy,1,1)),ols)
          assign(paste0("main.adj.se.dt",substr(hwy,1,1)),adj.se)
          assign(paste0("Ftest.dt",substr(hwy,1,1)), Ftest)
      }   

# Highways and suburbanization (Baum-Snow (2007)) ------------------------------------
      ## Highways and suburbanization        
    
      # data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds") 
    
      ols <- lm(paste0(pop_t, " ~ cc_bs + cc_bs:",hwy," +",control.vars), data=data)
      summary(ols)  
      
    
      for (hwy in hwys){
          ols_inst <- lm(paste0(hwy, "~ cc_bs +",inst.lm,"+",control.vars), data=data)
          summary(ols_inst) 
          adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
          Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
          data$instr <- predict.lm(ols_inst)
          ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data)
          summary(ols)
          linearHypothesis(ols, "cc_bs1:instr=cc_bs2:instr", white.adjust = "hc1")
          
              nn<- length(ols$residuals); nk <- ols$df.residual 
              y <- data[,pop_t]
              biv <- (ols$coefficients)
              x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+",control.vars))), data=data)
              xh <- model.matrix(ols$model, data=data)
              rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
              adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
          assign(paste0("ols.bs.1st.",substr(hwy,1,1)),adj.se.ols_inst)
          assign(paste0("ols.bs.1st.R2.",substr(hwy,1,1)),summary(ols_inst)$adj.r.squared)
          assign(paste0("main.ols.bs",substr(hwy,1,1)),ols)
          assign(paste0("main.adj.se.bs",substr(hwy,1,1)),adj.se)
          assign(paste0("Ftest.bs",substr(hwy,1,1)), Ftest)
      }   

  
  ### First stage regression results    
      stargazer(ols.bs.1st.R, ols.bs.1st.l, ols.dt.1st.R, ols.dt.1st.l, 
        type="latex", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
        out="H:/Desktop/0 - ERC/Data/R/model_results/hwys1980_1ststage.tex", 
        dep.var.labels   = c("Main model","Main model","Restricted specification (CC=suburbs)","Restricted specification (CC=suburbs)"), dep.var.caption = "", 
        keep.stat = c("n"), no.space = TRUE,
        add.lines = list(c("R-squared",
                           format(ols.bs.1st.R2.R,digits=4),format(ols.bs.1st.R2.l,digits=4),
                           format(ols.dt.1st.R2.R,digits=4),format(ols.dt.1st.R2.l,digits=4)))
      )
      

# Central city pop size sensitivity analysis (30k-70k)  -------------------

      control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
      control.vars <- paste(control.vars," + ",soilvar)
      for (range in seq(30000,70000,10000)){
        data = readRDS(file=paste0("H:/Desktop/0 - ERC/Data/data_1980_",range,".rds"))
        
        for (hwy in hwys){
          hwylab = substr(hwy,1,1)
          if (substr(hwy,1,1) == "R"){
            hwylab = "hR"
          }
          ols_inst <- lm(paste0(hwy, "~ cc_bs +",inst.lm,"+",control.vars), data=data)
          data$instr <- predict.lm(ols_inst)
          ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data)
          
                nn<- length(ols$residuals); nk <- ols$df.residual 
                y <- data[,pop_t]
                biv <- (ols$coefficients)
                x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+",control.vars))), data=data)
                xh <- model.matrix(ols$model, data=data)
                rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
                adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
                
          assign(paste0("range.ols.bs",range/1000,substr(hwylab,1,1)),ols)
          assign(paste0("range.adj.se.bs",range/1000,substr(hwylab,1,1)),adj.se)
        }
      }
      listols <-mixedsort(apropos("range.ols."))
      listse <-mixedsort(apropos("range.adj.se."))
      stargazer(mget(listols),
                se = mget(listse),
                    type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
                    out="H:/Desktop/0 - ERC/Data/R/model_results/hwys1980_thresholds.html", 
                    order = c("instr$", "cc_bs[0-9]$"),
                    keep.stat = c("n"), no.space = TRUE          )
      rm(list = c(listols,listse))
      
      data = readRDS(file="H:/Desktop/0 - ERC/Data/data_1980_50000.rds")
      
# Highways in the periphery Chandra Thompson (2000) -----------------------------------------------
      ## Highways in the periphery 
      
      control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + nota2_buff + gh_1958 + dist_cc  +  reclaimed")
      control.vars <- paste(control.vars," + ",soilvar)
      data.ct=subset(data,cc_bs=="0") # & gr_area1966==0)
      
      for (hwy in hwys){
          ols.ninst <- lm(paste0(pop_t, " ~", hwy, "  +",control.vars), data=data.ct)
          summary(ols.ninst)
          adj.se.ninst <- coeftest(ols.ninst, vcov = vcovHC(ols.ninst, "HC1"))
          ols_inst <- lm(paste0(hwy, " ~", inst.lm,"+",control.vars), data=data.ct)
          summary(ols_inst) 
          #Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
          data.ct$instr <- predict.lm(ols_inst)
          ols <- lm(paste0(pop_t, " ~ instr  +",control.vars), data=data.ct)
          summary(ols)
          
              nn<- length(ols$residuals); nk <- ols$df.residual 
              y <- data.ct[,pop_t]
              biv <- (ols$coefficients)
              x <- model.matrix(eval(parse(text=paste0(" ~  ",hwy,"+",control.vars))), data=data.ct)
              xh <- model.matrix(ols$model, data=data.ct)
              rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
              adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
          assign(paste0("ols.ct.ninst.",substr(hwy,1,1)),ols.ninst)
          assign(paste0("ols.ct",substr(hwy,1,1)),ols)
          assign(paste0("adj.se.ct.ninst.",substr(hwy,1,1)),adj.se.ninst)
          assign(paste0("adj.se.ct",substr(hwy,1,1)),adj.se)
          assign(paste0("Ftest.ct",substr(hwy,1,1)), Ftest)
      }
      
      stargazer(ols.ctR,ols.ctl,ols.ct.ninst.R,ols.ct.ninst.l, 
                se = list(adj.se.ctR, adj.se.ctl, adj.se.ct.ninst.R[,2], adj.se.ct.ninst.l[,2]),
                type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
                out="H:/Desktop/0 - ERC/Data/R/model_results/ct.html", 
                dep.var.labels   = c("IV","IV","OLS","OLS"), dep.var.caption = "", 
                keep.stat = c("n","rsq"), no.space = TRUE,
                add.lines = list(c("Instrumented", "Yes", "Yes", "No", "No"),
                                 c("R2","","",format(summary(ols.ct.ninst.R)$adj.r.squared, digits=4), format(summary(ols.ct.ninst.l)$adj.r.squared, digits=4)))
      )


# Excluding growth cities -------------------------------------------------

      data.ngr <- subset(data, gr_area1966==FALSE)
      data.ngr.ct <- subset(data.ngr, cc_bs=="0")
      
      ols_inst <- lm(paste0("Rays_1970_5k ~ cc_bs +",inst.lm,"+",control.vars), data=data.ngr)
      summary(ols_inst) 
      Ftest.ngr.bsr <- linearHypothesis(ols_inst, instruments)$F[2]
      data.ngr$instr <- predict.lm(ols_inst)
      ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data.ngr)
      summary(ols)
      
          nn<- length(ols$residuals); nk <- ols$df.residual 
          y <- data.ngr[,pop_t]
          biv <- (ols$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~   cc_bs + cc_bs:",hwy,"+",control.vars))), data=data.ngr)
          xh <- model.matrix(ols$model, data=data.ngr)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
      
      ols.bsR.ngr<-ols; adj.se.bsR.ngr<-adj.se;
      
      ols_inst <- lm(paste0("length_hwys_1970 ~ cc_bs+",inst.lm,"+",control.vars), data=data.ngr)
      summary(ols_inst) 
      Ftest.ngr.bsl <- linearHypothesis(ols_inst, instruments)$F[2]
      data.ngr$instr.l <- predict.lm(ols_inst)
      ols <- lm(paste0(pop_t, " ~  cc_bs +cc_bs:instr.l +",control.vars), data=data.ngr)
      summary(ols)
      
          nn<- length(ols$residuals); nk <- ols$df.residual 
          y <- data.ngr[,pop_t]
          biv <- (ols$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~   cc_bs + cc_bs:",hwy,"+",control.vars))), data=data.ngr)
          xh <- model.matrix(ols$model, data=data.ngr)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
      ols.bsl.ngr<-ols; adj.se.bsl.ngr<-adj.se;
      
      ols_inst <- lm(paste0("Rays_1970_5k ~ ",inst.lm,"+",control.vars), data=data.ngr.ct)
      summary(ols_inst) 
      Ftest.ngr.ctr <- linearHypothesis(ols_inst, instruments)$F[2]
      data.ngr.ct$instr <- predict.lm(ols_inst)
      ols <- lm(paste0(pop_t, " ~ instr  +",control.vars), data=data.ngr.ct)
      summary(ols)
          
          nn<- length(ols$residuals); nk <- ols$df.residual 
          y <- data.ngr.ct[,pop_t]
          biv <- (ols$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~   ",hwy,"+",control.vars))), data=data.ngr.ct)
          xh <- model.matrix(ols$model, data=data.ngr.ct)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
      ols.ctR.ngr<-ols; adj.se.ctR.ngr<-adj.se
      
      ols_inst <- lm(paste0("length_hwys_1970 ~ ",inst.lm,"+",control.vars), data=data.ngr.ct)
      summary(ols_inst) 
      Ftest.ngr.ctl <- linearHypothesis(ols_inst, instruments)$F[2]
      data.ngr.ct$instr.l <- predict.lm(ols_inst)
      ols <- lm(paste0(pop_t, " ~ instr.l  + ",control.vars), data=data.ngr.ct)
      summary(ols)
      
          nn<- length(ols$residuals); nk <- ols$df.residual 
          y <- data.ngr.ct[,pop_t]
          biv <- (ols$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~   ",hwy,"+",control.vars))), data=data.ngr.ct)
          xh <- model.matrix(ols$model, data=data.ngr.ct)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
      ols.ctl.ngr<-ols; adj.se.ctl.ngr<-adj.se
      
      stargazer(ols.bsR.ngr,ols.bsl.ngr,ols.ctR.ngr,ols.ctl.ngr,
        se = list(adj.se.bsR.ngr, adj.se.bsl.ngr, adj.se.ctR.ngr, adj.se.ctl.ngr),
        type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
        out="H:/Desktop/0 - ERC/Data/R/model_results/gr_areas_excl.html", 
        # title="(Dependent variable: Pop. growth (1970-1980, ln))",
        order = c("cc_bs[0-9]:instr","^instr", "cc_bs[0-9]$"),
        #         covariate.labels = c("Highway rays (1970)","Highway density (1970, ln)","Highway rays (1970)","Highway density (1970, ln)","Rail stations (1930)","Pop. (ln, 1930)", "Pop. (ln, 1960)", "Buffer zone share","Nature coverage share","Green heart share","Distance from central city","Constant"),
        dep.var.labels   = c("IV","IV","OLS","OLS"), dep.var.caption = "", 
        keep.stat = c("n","rsq"), no.space = TRUE,
        add.lines = list(c("Instrumented", "Yes", "Yes", "Yes", "Yes"),
                         c("First stage F-test",format(Ftest.ngr.bsr,digits=4),format(Ftest.ngr.bsl,digits=4),format(Ftest.ngr.ctr,digits=4),format(Ftest.ngr.ctl,digits=4)))
      )
      
      
      rm(data.ngr, data.ngr.ct, 
         ols.bsl.ngr,ols.bsR.ngr, ols.ctl.ngr, ols.ctR.ngr, 
         adj.se.bsl.ngr, adj.se.bsR.ngr, adj.se.ctl.ngr, adj.se.ctR.ngr)


# Effect on 1960-1990 growth ----------------------------------------------

      ### Run hwy_tables_app1970.R
      ### Run hwy_tables_app1990.R
      ### Run hwy_tables_app1960_1980.R
      stargazer(ols.bsR.70, ols.bsl.70, ols.bsR.90, ols.bsl.90, ols.bsR.8060, ols.bsl.8060,
                se=list(adj.se.bsR.70, adj.se.bsl.70, adj.se.bsR.90, adj.se.bsl.90, adj.se.bsR.8060, adj.se.bsl.8060), 
                type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1980 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
                out="H:/Desktop/0 - ERC/Data/R/model_results/hwys_6090.html", dep.var.caption = "", 
                order = c("instr$", "cc_bs[0-9]$"),
                keep.stat = c("n","rsq"), no.space = TRUE,
                add.lines = list(c("Instrumented", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                 c("First stage F-test","","","","","",""))
      ) 
      
# Endogeneity in the interaction terms ------------------------------------------------------

control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
control.vars <- paste(control.vars," + ",soilvar)  

## Bun & Harrison (2014)
      
      # Testing exogeneity : Wald test : 2.43-2.48 
        # H0: OLS estimator is consistent
      hwys = c("Rays_1970_5k","length_hwys_1970")
      for (hwy in hwys){
      
          nn= 811;
          x= as.vector(data[names(data) %in% hwy]) 
          w= as.vector(as.numeric(data$cc_dt)-1)
          
          tt = data.frame(t1= colMeans(x*w), 
                          t2= colMeans(x*(w*w)),
                          t3= mean(w*w),
                          t4= colMeans((x*x)*w))
          mm = data.frame(m1 = x*w - as.numeric(tt[1]),
                          m2 = x*(w*w)- as.numeric(tt[2]),
                          m3 = w*w - as.numeric(tt[3]), 
                          m4 = (x*x)*w- as.numeric(tt[4]));
          tt = as.matrix(tt); mm = as.matrix(mm);
          ht = (tt[1]*tt[2] - tt[3]*tt[4])
          cc= t(mm)%*%mm/nn # cc = colMeans(mm*mm) # cc = mean(diag(mm%*%t(mm)))
          rr = c(tt[2],tt[1],-tt[4],-tt[3])
          
          wald = ht/sqrt(rr%*%cc%*%rr/nn); # ht/sqrt(rr*central city*rr/nn)
          
          assign(paste0("wald.",hwy),2*(1-pnorm(abs(wald))))
          
          rm(tt,mm,ht,cc,rr,wald);
      }
      paste0("P-value of the Wald test is ",wald.Rays_1970_5k)
      paste0("P-value of the Wald test is ",wald.length_hwys_1970)
      
      
      for (hwy in hwys){
        # Hausman test (dt) (2.50,2.51,2.52)
            # Comparison of IV2 and IV3. 
            # Rejecting the null hypothesis means that assumption 2 is invalid 
            # == IV3 is inconsistent (the interaction term is not exogenous) and IV2 is preferred.
          
            y1 = data[names(data) %in% hwy] #  data$Rays_1970_5k # data$length_hwys_1970
            yy = data[,pop_t]
            zz = data[instruments]
            
            x = model.matrix(as.formula(paste0(" ~   cc_dt + cc_dt:", hwy," + ", control.vars)), data=data) 
            z2 = model.matrix(as.formula(paste0(" ~  cc_dt +",paste0("cc_dt:",instruments, collapse="+"),"+", control.vars)), data=data)
            z3 = model.matrix(as.formula(paste0(" ~  cc_dt +",paste0("cc_dt:",instruments, collapse="+"),"+ cc_dt:", hwy," + ", control.vars)), data=data); 
            z3 <- z3[ , !(colnames(z3) %in% paste0("cc_bs0:",hwy))] # exclude the base interaction from the instrument matrix, which due to specification renders length_hwys_1970 as exogenous as well (compared with ivreg in stata)
            
            pz2 = z2%*%solve(t(z2)%*%z2)%*%t(z2)
            pz3 = z3%*%solve(t(z3)%*%z3)%*%t(z3)
            
            b2= solve(t(x)%*%pz2%*%x) %*% t(x)%*%pz2%*%yy
            b3= solve(t(x)%*%pz3%*%x) %*% t(x)%*%pz3%*%yy
  
            su2 = as.numeric((1/nn)%*%t(yy-x%*%b2)%*%(yy-x%*%b2))
            su3 = as.numeric((1/nn)%*%t(yy-x%*%b3)%*%(yy-x%*%b3))
            
            vh =    su2 * solve(t(x)%*%pz2%*%x) - su3 * solve(t(x)%*%pz3%*%x)
            
            t(b2-b3)%*%solve(vh)%*%(b2-b3); # full Hausman test, distributed chi-2 (df=dim(H)=9)
            
            HM = t(b2-b3)%*%solve(vh)%*%(b2-b3); # full Hausman test, distributed chi-2 (df=dim(H)-1=8)
            HM; 1-pchisq(HM,8)
            
            H = (b2-b3)*(b2-b3)/diag(vh) # Last coefficient (interaction term), distributed chi-2 (df=1)
            # paste0("P-value of the Hausman test statistic is ..... ",1-pchisq(H,1)[(length(pchisq(H,1))-1):length(pchisq(H,1))])
            paste0("Standard Hausman test statistic is .................. ",H[grep("cc_dt[0-9]:",rownames(H))])
            paste0("P-value of the Hausman test statistic is ............ ",1-pchisq(H,1)[grep("cc_dt[0-9]:",rownames(pchisq(H,1)))])
            
            
            rm(y1,yy,x,z2,z3,b2,b3,su2,su3,vh,H);
            
            
        # Hausman test (bs) (2.50,2.51,2.52)
            # Comparison of IV2 and IV3. 
            # Rejecting the null hypothesis means that assumption 2 is invalid 
            # == IV3 is inconsistent (the interaction term is not exogenous)and IV2 is preferred.
            
            y1 = data[names(data) %in% hwy] # data$Rays_1970_5k # data$length_hwys_1970
            yy = data[,pop_t]
            zz = data$length_hwys_1821
  
            x = model.matrix(as.formula(paste0(" ~   cc_bs + cc_bs:", hwy," + ", control.vars)), data=data); 
            z2 = model.matrix(as.formula(paste0(" ~  cc_bs +", paste0("cc_bs:",instruments, collapse="+"),"+", control.vars)), data=data); 
            z3 = model.matrix(as.formula(paste0(" ~  cc_bs +", paste0("cc_bs:",instruments, collapse="+"),"+ cc_bs:", hwy," + ", control.vars)), data=data);  
            z3 <- z3[ , !(colnames(z3) %in% paste0("cc_bs0:",hwy))]    # exclude the base interaction from the instrument matrix, which due to specification renders length_hwys_1970 as exogenous as well (compared with ivreg in stata)
            
            pz2 = z2%*%solve(t(z2)%*%z2)%*%t(z2)
            pz3 = z3%*%solve(t(z3)%*%z3)%*%t(z3)
            
            b2= solve(t(x)%*%pz2%*%x) %*% t(x)%*%pz2%*%yy
            b3= solve(t(x)%*%pz3%*%x) %*% t(x)%*%pz3%*%yy
            
            su2 = as.numeric((1/nn)%*%t(yy-x%*%b2)%*%(yy-x%*%b2))
            su3 = as.numeric((1/nn)%*%t(yy-x%*%b3)%*%(yy-x%*%b3))
            
            vh =    su2 * solve(t(x)%*%pz2%*%x) - su3 * solve(t(x)%*%pz3%*%x)
         
            HM = t(b2-b3)%*%solve(vh)%*%(b2-b3); # full Hausman test, distributed chi-2 (df=dim(H)=9)
            HM; 1-pchisq(HM,10)
            
            H = (b2-b3)*(b2-b3)/diag(vh) # Last coefficient (interaction term), distributed chi-2 (df=1)
            # paste0("P-value of the Hausman test statistic is ..... ",1-pchisq(H,1)[(length(pchisq(H,1))-1):length(pchisq(H,1))])
            paste0("Standard Hausman test statistic is .................. ",H[grep("cc_bs[0-9]:",rownames(H))])
            paste0("P-value of the Hausman test statistic is ............ ",1-pchisq(H,1)[grep("cc_bs[0-9]:",rownames(pchisq(H,1)))])
            
            Hausman.test <- data.frame(H.stat = H[grep("cc_bs[0-9]:",rownames(H))],
                                       P.value = 1-pchisq(H,1)[grep("cc_bs[0-9]:",rownames(pchisq(H,1)))])
            
            assign(paste0("Hausman.test.",hwy),Hausman.test)
            rm(y1,yy,x,z2,z3,b2,b3,su2,su3,vh,H);
      }
      Hausman.test.Rays_1970_5k
      Hausman.test.length_hwys_1970
          
               
  ## export: OLS and IV2/IV3 (IV1/IV2 in the paper) 
      for (hwy in hwys){ 
          ### OLS
          ols <- lm(paste0(pop_t, " ~ cc_bs +  cc_bs:",hwy," + ",control.vars), data=data)
          summary(ols)
          adj.se.ols <- coeftest(ols, vcov = vcovHC(ols, "HC1"))
          
          ### IV1 (Multiple 1st stage : Wooldridge)
          iv1 <- ivreg(paste0(pop_t, " ~ cc_bs +  cc_bs:",hwy," + ",control.vars," | 
                           cc_bs +  ", paste0("cc_bs:",instruments, collapse="+")," +", control.vars), data=data)
          summary(iv1) 
          adj.se.iv1 <- robust.se(iv1)
          
          ### IV2 (Single 1st stage : Bun & Harrison (2014))
          # data$interact <- (as.numeric(data$cc_bs)-1)*data$Rays_1970_5k
          data$interact.1 <- as.numeric(data$cc_bs==1)*eval(parse(text = paste0("data$",hwy)))
          data$interact.2 <- as.numeric(data$cc_bs==2)*eval(parse(text = paste0("data$",hwy)))
          iv2 <- ivreg(paste0(pop_t, " ~  cc_bs +  cc_bs:",hwy," + ",control.vars," | 
                           cc_bs +  ",paste0("cc_bs:",instruments, collapse="+")," +  interact.1 + interact.2 +", control.vars), data=data)
          summary(iv2)
          adj.se.iv2 <- robust.se(iv2)
          
          assign(paste0("endogtest.ols.",hwy),adj.se.ols)
          assign(paste0("endogtest.iv1.",hwy),adj.se.iv1)
          assign(paste0("endogtest.iv2.",hwy),adj.se.iv2)
          assign(paste0("R2.ols.",hwy),summary(ols)$adj.r.squared)
          assign(paste0("Ftest.end.iv1.",hwy),summary(iv1)$waldtest[1])
          assign(paste0("Ftest.end.iv2.",hwy),summary(iv2)$waldtest[1])
      }
          
          # Check and compare in stata :
          #     xi: ivreg2 $Y i.cc_dt $vars  (i.cc_dt*$length = i.cc_dt*$inst), robust (IV1)
          #     xi: ivreg2 $Y i.cc_dt $vars  (i.cc_dt*$length = i.cc_dt*$inst i.cc_dt*$length), robust (IV2)
          #     xi: ivreg2 $Y i.cc_dt $vars   i.cc_dt*$length ($length = i.cc_dt*$inst), robust (IV2 misestimated as OLS, as in here)
          
          #     replace $length = 0 if cc_dt==1
          #     ivreg2$Y cc_dt $vars (length_hwys_1970 _Icc_Xlengt_1 = length_hwys_1821 _Icc_Xlengta1), robust (IV1)
          #     ivreg2 $Y cc_dt $vars _Icc_Xlengt_1 (length_hwys_1970 = length_hwys_1821 _Icc_Xlengta1), robust (IV2)
          #     hausman b2 b3

          
          stargazer(endogtest.ols.Rays_1970_5k, endogtest.iv2.Rays_1970_5k, endogtest.iv1.Rays_1970_5k,
                    endogtest.ols.length_hwys_1970, endogtest.iv2.length_hwys_1970, endogtest.iv1.length_hwys_1970, 
                    type="latex", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
                    out="H:/Desktop/0 - ERC/Data/R/model_results/olsiv1iv2.tex", 
                    title="(Dependent variable: Pop. growth (1970-1980, ln))",
                    column.labels   = c("OLS", "BH","W","OLS","BH","W"), model.names = FALSE,
                    order = c("instr$", "cc_bs[0-9]$"),
                    # covariate.labels = c("Highway rays*periphery ","Highway rays*suburbs","Highway rays*central city","Highway rays*periphery ","Highway rays*suburbs","Highway rays*central city",
                    #                     "Highway density*periphery","Highway density*suburbs","Highway density*central city","Highway density*periphery","Highway density*suburbs","Highway density*central city",
                    #                     "Suburbs","Central city","Rail stations (1930)","Pop. (ln, 1930)","Buffer zone share","Nature coverage share","Green heart (1958)","Constant"),
                    dep.var.labels.include = FALSE, dep.var.caption = "", 
                    keep.stat = c("n","rsq"), no.space = TRUE,
                    add.lines = list(c("Instrumented","No", "Yes","Yes","No", "Yes", "Yes"),
                                     c("First stage F-test","","", "",
                                       "",format(Ftest.end.iv2.length_hwys_1970,digits=4),format(Ftest.end.iv1.length_hwys_1970,digits=4)),
                                     c("R-squared",R2.ols.Rays_1970_5k,"","",
                                       R2.ols.length_hwys_1970,"",""))
          )
          
          
# Figures and maps --------------------------------------------------------

    ## Import spatial data
    mun1980<- readOGR("H:/Desktop/0 - ERC/Data/GIS/Municipal borders_jasper/municipalities/nl_1980.shp") ## read shape file
      proj4string(mun1980)  ## verify map projection
      proj4string(mun1980) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_defs") ## source: http://www.spatialreference.org/ref/sr-org/6781/proj4/ 
      mun1980.fort <- tidy(mun1980, region = "GM_1980")
      mun1980.fort$code <- as.numeric(mun1980.fort$id)
          
      mun1980.fort <- merge(mun1980.fort,subset(data, select=c(code,cc_bs,pop_1930,length_hwys_1970,pop_t1980,belt)),by="code"); rm(mun1980);
      mun1980.fort$cc_bs<- revalue(as.factor(mun1980.fort$cc_bs), c("0"="Periphery", "1"="Suburbs", "2"="Central City"))
      
    AppD_50k<- 
      ggplot() +  geom_polygon(data=mun1980.fort, aes(x = long, y = lat, fill = cc_bs, group = group), color = "black", size = 0.25) +
      coord_equal(ratio=1) + xlab("") + ylab("") +
      scale_fill_manual(name="", values=c("#ffeda0", "#feb24c", "#ec7014"),
                        labels = c("Periphery", "Suburbs", "Central city")) + # c("#ffeda0","#feb24c", "#f03b20") # grey50 steelblue3 lightsteelblue3 gold2 firebrick tomato2 skyblue1 # source: http://sape.inf.usi.ch/quick-reference/ggplot2/colour 
      theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), panel.grid = element_line(colour="white"), panel.background=element_rect(fill="white",colour="white"), legend.position = "right")
      # labs(title = "Population threshold for central city: 50000");
      ggsave(AppD_50k, file="H:/Desktop/0 - ERC/Data/R/AppD_50k.eps", scale = 1)
    rm(AppD_50k)
   
    ## Distribution plots
    ggplot(data) + aes(fill = cc_bs) +
        # geom_density(aes(x=gh_1958), position = "identity", bw=0.05, alpha=0.5) +
        # geom_density(data=subset(data,nota2_buff>0), aes(x=nota2_buff), position = "identity", bw=0.05, alpha=0.5) +
        # geom_histogram(data=subset(data,nota2_buff>0), aes(x=nota2_buff), binwidth = 0.1) +
        geom_histogram(aes(x=nota2_buff), binwidth = 0.1) +
        facet_grid(~cc_bs)

# Model illustration ------------------------------------------------------

xbuff.1 <- 20
xbuff.2 <- 40
y.rural <- 12.5
y1.arrows <- 7.5; y2.arrows <- -5
alpha_steps <- seq(from = 0.5, to = 0, length.out = 150)
    
plot <- data.frame(distance = seq(0,90,1))
  plot$t0 <- (12.5- 1.15*plot$distance^0.55)^2;
  plot$t1 <- (9.5 - 0.6*plot$distance^0.55)^2;  plot$t1[plot$distance>100]<- NA;
  plot$t2 <- (10.5  - 0.6*plot$distance^0.55)^2; plot$t2[plot$distance>xbuff.1 & plot$distance<xbuff.2]<- NA;
  # plot$t1 <- 100 - 1*plot$distance; 
  # plot$t2 <- 115 - 1*plot$distance;  
  plot$t21 <- 25;  plot$t21[plot$distance<xbuff.1 | plot$distance>xbuff.2]<- NA;
  
  plot$t0[plot$t0<y.rural] <- y.rural;
  plot$t1[plot$t1<y.rural] <- y.rural;
  plot$t2[plot$t2<y.rural] <- y.rural;
  
  
gr.hwys <-  
ggplot(plot) + aes(x=distance) +
  geom_line(aes(y=t0), linetype=1, size=1) +
  geom_line(aes(y=t1), linetype=2, size=1) +
  geom_line(aes(y=t2), linetype=1, size=1) +
  geom_line(aes(y=t21), linetype=1, size=1) +
  geom_area(aes(y=t2), position=position_dodge(width = 0), alpha=0.5) + 
  geom_area(aes(y=t21), position=position_dodge(width = 0), alpha=0.5) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 160), linetype = 1) +
    geom_segment(aes(x = xbuff.1, y = 0, xend = xbuff.1, yend = 160), linetype = 3, size=1) +
    geom_segment(aes(x = xbuff.2, y = 0, xend = xbuff.2, yend = 160), linetype = 3, size=1) +
    geom_segment(aes(x = 0, y = y.rural, xend = max(plot$distance), yend = y.rural), linetype = 3, size=0.5) +
    geom_hline(aes(yintercept = 0))  +
    # annotate("text", x = 108, y = y.rural-2.5, label = "Rural density") +
    annotate("text", x = 3, y = 130, label = "t0", size=4) +
    annotate("text", x = 3, y = 81, label = "t1", size=4) +
    annotate("text", x = 3, y = 100, label = "t2", size=4) +
  
    # annotate("rect",xmin=1,xmax=xbuff.1-1,ymin=y1.arrows-2.5,ymax=y1.arrows+2.5,fill="grey75", alpha=.25) +
    annotate("rect",xmin=xbuff.1+1,xmax=xbuff.2-1,ymin=y1.arrows-4,ymax=y1.arrows+3,fill="grey95", alpha=.25) +
    # annotate("rect",xmin=xbuff.2+1,xmax=100,ymin=y1.arrows-2.5,ymax=y1.arrows+2.5,fill="grey75", alpha=.25) +
    annotate("text", x = (xbuff.1+xbuff.2)/2, y = y1.arrows, label = "Restricted area", size=3.5) +
    # annotate("text", x = (xbuff.1)/2, y = y1.arrows, label = "Urban area", size=3.5) +
    # annotate("text", x = (xbuff.2+100)/2, y = y1.arrows, label = "Rural", size=3.5) +
    
    geom_rect(aes(xmin=distance,xmax=distance+1,ymin=y2.arrows-3,ymax=y2.arrows+2,fill=distance)) +
    scale_fill_gradient2(low="grey50",mid="white",high="grey50", midpoint=(xbuff.1+xbuff.2)/2) +
    annotate("text", x = (xbuff.1-5)/2, y = y2.arrows, label = "Central city", size=3.5) +
    annotate("text", x = (xbuff.1+xbuff.2)/2, y = y2.arrows, label = "suburb", size=3.5) +
    annotate("text", x = (xbuff.1+10+118)/2, y = y2.arrows, label = "Periphery", size=3.5) +
    theme_classic() + theme(legend.position="none", axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),  axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + xlab("Distance") + ylab("Population density")
    
    
  gr.hwys
  ggsave(file="H:/Desktop/0 - ERC/Data/R/Fig1.png", gr.hwys)
  
# Instruments check - Rail stations ---------------------------------------

  control.vars <- c("pop_1930 + nota2_buff + hgn_nat1960_mean + gh_1958")
  control.vars <- paste(control.vars," + ",soilvar)  
  for (hwy in hwys){ 
    ols_inst <- lm(paste0(hwy, "~ cc_bs + rail_stations_1930 +",control.vars), data=data)
    summary(ols_inst) 
    adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
    Ftest <- linearHypothesis(ols_inst, "rail_stations_1930")$F[2]
    data$instr <- predict.lm(ols_inst)
    ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data)
    summary(ols)
    linearHypothesis(ols, "cc_bs1:instr=cc_bs2:instr", white.adjust = "hc1")
    
        nn<- length(ols$residuals); nk <- ols$df.residual 
        y <- data[,pop_t]
        biv <- (ols$coefficients)
        x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+",control.vars))), data=data)
        xh <- model.matrix(ols$model, data=data)
        rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
        adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.bs.1st.",substr(hwy,1,1)),adj.se.ols_inst)
    assign(paste0("ols.bs.1st.R2.",substr(hwy,1,1)),summary(ols_inst)$adj.r.squared)
    assign(paste0("ols.bs",substr(hwy,1,1)),ols)
    assign(paste0("adj.se.bs",substr(hwy,1,1)),adj.se)
    assign(paste0("Ftest.bs",substr(hwy,1,1)), Ftest)
  }
    
  
  stargazer(ols.bs.1st.R, ols.bs.1st.l,
            type="html", digits = 5, notes = "Robust standard errors in parentheses", single.row = FALSE, 
            out="H:/Desktop/0 - ERC/Data/R/model_results/bs_inst_railstations_1st.html", 
            dep.var.labels   = c("Highway rays","Highway density"), dep.var.caption = "", 
            no.space = TRUE
  )
  
  stargazer(ols.bsR,ols.bsl,
            se=list(adj.se.bsR,adj.se.bsl), 
            type="html", digits = 5, notes = "Robust standard errors in parentheses", single.row = FALSE, 
            out="H:/Desktop/0 - ERC/Data/R/model_results/bs_inst_railstations.html", 
            title="Highways and suburbanization",
            order = c("instr$", "cc_bs[0-9]$"),
            keep.stat = c("n"), no.space = TRUE,
            add.lines = list(c("Instrumented", "Yes", "Yes"),
                             c("First stage F-test",format(Ftest.bsR,digits=4),format(Ftest.bsl,digits=4)))
  )
  
  
# Highways and suburbanization - Multiple treatments for suburbs (Baum-Snow (2007)) ------------------------------------
  ## Highways and suburbanization        
  
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean +  gh_1958 + dist_cc  +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar)  
  
  for (hwy in hwys){
    ols_inst <- lm(paste0(hwy, "~ cc_bs + ntr +",inst.lm,"+",control.vars), data=data)
    summary(ols_inst) 
    adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
    Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
    data$instr <- predict.lm(ols_inst)
    ols <- lm(paste0(pop_t, " ~  cc_bs + ntr + cc_bs:instr:ntr +",control.vars), data=data)
    summary(ols)
    linearHypothesis(ols, "cc_bs1:ntr:instr=cc_bs2:ntr:instr", white.adjust = "hc1")
        
        nn<- length(ols$residuals); nk <- ols$df.residual 
        y <- data[,pop_t]
        biv <- (ols$coefficients)
        x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs +ntr+ cc_bs:",hwy,"+",control.vars))), data=data)
        xh <- model.matrix(ols$model, data=data)
        rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
        adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.bs.1st.",substr(hwy,1,1)),adj.se.ols_inst)
    assign(paste0("ols.bs.1st.R2.",substr(hwy,1,1)),summary(ols_inst)$adj.r.squared)
    assign(paste0("ols.bs",substr(hwy,1,1)),ols)
    assign(paste0("adj.se.bs",substr(hwy,1,1)),adj.se)
    assign(paste0("Ftest.bs",substr(hwy,1,1)), Ftest)
  }   
  
  stargazer(ols.bsR,ols.bsl,
            se=list(adj.se.bsR,adj.se.bsl), 
            type="html", digits = 5, notes = "Robust standard errors in parentheses", single.row = FALSE, 
            out="H:/Desktop/0 - ERC/Data/R/model_results/bs_ntr.html", 
            order = c("instr$", "cc_bs[0-9]$", "ntr"),
            dep.var.labels   = c("Pop. growth (1970-1980, ln)","Pop. growth (1970-1980, ln)"), dep.var.caption = "", 
            keep.stat = c("n"), no.space = TRUE,
            add.lines = list(c("Instrumented", "Yes", "Yes"),
                             c("First stage F-test",format(Ftest.bsR,digits=4),format(Ftest.bsl,digits=4)))
  )
  

# Testing the single first-stage approach (July 2017) ---------------------

  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + reclaimed")
 # control.vars <- paste(control.vars," + ",soilvar)  
  control.vars.int <- paste0("cc_bs:",unlist(strsplit(control.vars,"\\+")), collapse = "+")
  data$cc_bs0.distcc <- as.numeric(data$cc_bs==0) * data$dist_cc
  data$cc_bs1.distcc <- as.numeric(data$cc_bs==1) * data$dist_cc
  
  # Define the hypothesis matrix 
  # Rejecting the null hypothesis implies that the restrictions do not hold and the standard multiple first-stage approach is preferable
  
  hypmat = c("cc_bs0:length_hwys_1821 = cc_bs1:length_hwys_1821",
             "cc_bs0:length_hwys_1821 = cc_bs2:length_hwys_1821",
             "cc_bs0:Rays_1821_5k = cc_bs1:Rays_1821_5k",
             "cc_bs0:Rays_1821_5k = cc_bs2:Rays_1821_5k",
             "cc_bs0:rail_stations_1930 = cc_bs1:rail_stations_1930",
             "cc_bs0:rail_stations_1930 = cc_bs2:rail_stations_1930",
             "cc_bs0:pop_1930 = cc_bs1:pop_1930",
             "cc_bs0:pop_1930 = cc_bs2:pop_1930",
             "cc_bs0:pop_1960 = cc_bs1:pop_1960",
             "cc_bs0:pop_1960 = cc_bs2:pop_1960",
             "cc_bs0:nota2_buff = cc_bs1:nota2_buff",
             "cc_bs0:nota2_buff = cc_bs2:nota2_buff",
             "cc_bs0:hgn_nat1960_mean = cc_bs1:hgn_nat1960_mean",
             "cc_bs0:hgn_nat1960_mean = cc_bs2:hgn_nat1960_mean",
             "cc_bs0:gh_1958 = cc_bs1:gh_1958",
             "cc_bs0:gh_1958 = cc_bs2:gh_1958",
             "cc_bs0:reclaimed=cc_bs1:reclaimed",
             "cc_bs0:reclaimed=cc_bs2:reclaimed",
             "cc_bs0.distcc = cc_bs1.distcc")
  
  data %>% group_by(cc_bs) %>% summarise (n = n()) %>%    mutate(freq = n / sum(n))
  
  for (hwy in hwys){
    ols_inst <- lm(paste0(hwy, "~ cc_bs +",inst.lm,"+ dist_cc  + ",control.vars), data=data)
    summary(ols_inst)
    stage1_b = summary(ols_inst)$coefficients
    
    ols_inst_full <- lm(paste0(hwy, "~ cc_bs +",paste0("cc_bs:",instruments, collapse=" + "),"+  cc_bs + cc_bs0.distcc + cc_bs1.distcc  + ", control.vars.int), data=data)
    summary(ols_inst_full) 
    
    hypmat = c(paste0("0.8113*cc_bs0:length_hwys_1821 + 0.1639*cc_bs1:length_hwys_1821 + 0.0246*cc_bs2:length_hwys_1821 = ",stage1_b['length_hwys_1821',1]),
               paste0("0.8113*cc_bs0:Rays_1821_5k + 0.1639*cc_bs1:Rays_1821_5k +  0.0246*cc_bs2:Rays_1821_5k = ",stage1_b['Rays_1821_5k',1]),
               paste0("0.8113*cc_bs0:rail_stations_1930 + 0.1639*cc_bs1:rail_stations_1930 + 0.0246*cc_bs2:rail_stations_1930 = ",stage1_b['rail_stations_1930',1]),
               paste0("0.8113*cc_bs0:pop_1930 + 0.1639*cc_bs1:pop_1930 + 0.0246*cc_bs2:pop_1930 = ",stage1_b['pop_1930',1]),
               paste0("0.8113*cc_bs0:pop_1960 + 0.1639*cc_bs1:pop_1960 + 0.0246*cc_bs2:pop_1960 = ",stage1_b['pop_1960',1]),
               paste0("0.8113*cc_bs0:nota2_buff + 0.1639*cc_bs1:nota2_buff + 0.0246*cc_bs2:nota2_buff = ",stage1_b['nota2_buff',1]),
               paste0("0.8113*cc_bs0:hgn_nat1960_mean + 0.1639*cc_bs1:hgn_nat1960_mean + 0.0246*cc_bs2:hgn_nat1960_mean =",stage1_b['hgn_nat1960_mean',1]),
               paste0("0.8113*cc_bs0:gh_1958 + 0.1639*cc_bs1:gh_1958 + 0.0246*cc_bs2:gh_1958 = ",stage1_b['gh_1958',1]),
               paste0("0.8113*cc_bs0:reclaimed + 0.1639*cc_bs1:reclaimed + 0.0246*cc_bs2:reclaimed = ",stage1_b['reclaimed',1]),
               paste0("0.8113*cc_bs0.distcc + 0.1639*cc_bs1.distcc = ",stage1_b['dist_cc',1]))
    
    stargazer(linearHypothesis(ols_inst_full, hypmat, vcov = vcovHC(ols_inst_full, "HC1")), summary = FALSE,
              out = paste0("H:/Desktop/0 - ERC/Data/R/model_results/iv_hyptest_",hwy,".tex"))
    print(list(hwy, linearHypothesis(ols_inst_full, hypmat, vcov = vcovHC(ols_inst_full, "HC1"))))
  }

  rm(stage1_b)  

# Highways and suburbanization - New interaction variables (2017) ---------
  
  gc()
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960  + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar) 
  
  hwys = c("Rays_1970_5k","length_hwys_1970")
  for (hwy in hwys){
    ## Full
          ols_inst <- lm(paste0(hwy, "~ cc_bs + cc_bs:nota2_buff +",inst.lm," + ",control.vars), data=data)
          Ftest1 <- linearHypothesis(ols_inst,instruments)$F[2]
          data$instr <- predict.lm(ols_inst)
          ols.full <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr + cc_bs:nota2_buff + cc_bs:nota2_buff:instr + ",control.vars), data=data)
          summary(ols.full)
                
                nn<- length(ols.full$residuals); nk <- ols.full$df.residual 
                y <- data[,pop_t]
                biv <- (ols.full$coefficients)
                x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+ cc_bs:nota2_buff + cc_bs:nota2_buff:",hwy,"+",control.vars))), data=data)
                xh <- model.matrix(ols.full$model, data=data)
                rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
                adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
          assign(paste0("ols.full.",hwy),ols.full)
          assign(paste0("adj.se.full.",hwy),adj.se)
          assign(paste0("Ftest1.full.",hwy), Ftest1)
              ### Conclusion : We cannot reject the null hyopthesis according which
              ### The effect of highways within bufferzones is statistically identical for suburbs and periphery.
              ### The effect can be restricted.
              linearHypothesis(ols.full,"cc_bs0:instr:nota2_buff = cc_bs0:instr", white.adjust="hc1")
              linearHypothesis(ols.full,"cc_bs1:instr:nota2_buff = cc_bs1:instr", white.adjust="hc1")
              # linearHypothesis(ols.full,"cc_bs2:instr:nota2_buff = cc_bs2:instr", white.adjust="hc1") # ignore due to low number of observations
          
    ## Restricted model
          ols_inst <- lm(paste0(hwy, " ~ cc_bs + nota2_buff + ",inst.lm," + ",control.vars), data=data) #, weights = weights)
          Ftest1 <- linearHypothesis(ols_inst,instruments)$F[2]
          data$instr <- predict.lm(ols_inst)
          ols.restr <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr + nota2_buff + ",control.vars), data=data) #, weights = weights)
          assign(paste0("ols.restr.",hwy), ols.restr)
            ### Restricted results are equal to the original general model! 
          
                nn<- length(ols.restr$residuals); nk <- ols.restr$df.residual 
                y <- data[,pop_t]
                biv <- (ols.restr$coefficients)
                x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+ nota2_buff +",control.vars))), data=data)
                xh <- model.matrix(ols.restr$model, data=data)
                rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
                adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
          
          assign(paste0("adj.se.restr.",hwy),adj.se)
          assign(paste0("Ftest1.restr.",hwy), Ftest1)
          
          rm(ols_inst, ols.full,ols.restr,Ftest1)
  }   
  
  stargazer(ols.full.Rays_1970_5k,ols.full.length_hwys_1970, ols.restr.Rays_1970_5k, ols.restr.length_hwys_1970,
    se = list(adj.se.full.Rays_1970_5k, adj.se.full.length_hwys_1970, adj.se.restr.Rays_1970_5k, adj.se.restr.length_hwys_1970), 
              type="html", digits = 5, single.row = FALSE, 
              out="H:/Desktop/0 - ERC/Data/R/model_results/bs2017_buffint.html", 
              title="(Dependent variable: Log. pop. growth (1970--1980))",
              order = c("cc_bs[0-9]:instr", "cc_bs[0-9]$", "cc_bs[0-9]:nota2_buff$", "nota2_buff"),
              keep.stat = c("n"), no.space = TRUE,
              add.lines = list(c("Instrumented", "Yes", "Yes", "Yes", "Yes"),
                               c("First stage F-test",
                                 format(Ftest1.full.Rays_1970_5k,digits=4),
                                 format(Ftest1.full.length_hwys_1970,digits=4),
                                 format(Ftest1.restr.Rays_1970_5k,digits=4),
                                 format(Ftest1.restr.length_hwys_1970,digits=4)))
    )
      
# Highways and suburbanization - New interaction variables + Instrumenting buffer zones (2017) ---------
  
  gc()
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960  + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar) 
  
  hwys = c("Rays_1970_5k","length_hwys_1970")
  for (hwy in hwys){
    
    ## Full
    # 1st stage #1
      ols_inst <- lm(paste0(hwy, "~ cc_bs  +  ",inst.lm," +  buffer_instrument +",paste0(instruments,":buffer_instrument", collapse=" + ")," + ",control.vars), data=data)
      Ftest1 <- linearHypothesis(ols_inst, instruments)$F[2]
      Ftest1.all <- linearHypothesis(ols_inst, c(instruments,"buffer_instrument",paste0(instruments,":buffer_instrument")))$F[2]
      data$instr <- predict.lm(ols_inst)
    # 1st stage #2
      ols_inst.buff <- lm(paste0("nota2_buff ~ cc_bs  +  ",inst.lm," +  buffer_instrument +",paste0(instruments,":buffer_instrument", collapse=" + ")," + ",control.vars), data = data)
      Ftest1.buff <- linearHypothesis(ols_inst.buff, "buffer_instrument")$F[2]
      Ftest1.buff.all <- linearHypothesis(ols_inst.buff, c(instruments,"buffer_instrument",paste0(instruments,":buffer_instrument")))$F[2]
      data$inst.buff <- predict(ols_inst.buff) # fitted.values(ols_inst.buff)
    # 1st stage #3
      hwy.var <- eval(parse(text = (paste0("data$",hwy))))
      data$hwybuff = data$nota2_buff * hwy.var
      ols_inst.hwybuff <- lm(paste0("hwybuff ~ cc_bs  +  ",inst.lm," +  buffer_instrument  +",paste0(instruments,":buffer_instrument", collapse=" + ")," + ",control.vars), data = data)
      Ftest1.hwybuff <- linearHypothesis(ols_inst.hwybuff, paste0(instruments,":buffer_instrument"))$F[2]
      Ftest1.hwybuff.all <- linearHypothesis(ols_inst.hwybuff, c(instruments,"buffer_instrument",paste0(instruments,":buffer_instrument")))$F[2]
      data$instr.hwybuff <- predict(ols_inst.hwybuff) 
      
    ols.full <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr + cc_bs:inst.buff + cc_bs:instr.hwybuff + ",control.vars), data=data)
    summary(ols.full)
    
          nn<- length(ols.full$residuals); nk <- ols.full$df.residual 
          y <- data[,pop_t]
          biv <- (ols.full$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+ cc_bs:nota2_buff + cc_bs:nota2_buff:",hwy,"+",control.vars))), data=data)
          xh <- model.matrix(ols.full$model, data=data)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.full.inst.",hwy),ols.full)
    assign(paste0("adj.se.full.inst.",hwy),adj.se)
    assign(paste0("Ftest1.full.inst.",hwy), Ftest1)
    assign(paste0("Ftest1.full.instbuff.",hwy), Ftest1.buff)
    assign(paste0("Ftest1.full.insthwybuff.",hwy), Ftest1.hwybuff)
    assign(paste0("Ftest1.full.inst.all.",hwy), Ftest1.all)
    assign(paste0("Ftest1.full.instbuff.all.",hwy), Ftest1.buff.all)
    assign(paste0("Ftest1.full.insthwybuff.all.",hwy), Ftest1.hwybuff.all)
        ### Conclusion : We cannot reject the null hyopthesis according which
        ### The effect of highways within bufferzones is statistically identical for suburbs and periphery.
        ### The effect can be restricted.
        linearHypothesis(ols.full,"cc_bs0:instr.hwybuff = cc_bs0:instr", white.adjust="hc1")
        linearHypothesis(ols.full,"cc_bs1:instr.hwybuff = cc_bs1:instr", white.adjust="hc1")
        linearHypothesis(ols.full,"cc_bs2:instr.hwybuff = cc_bs2:instr", white.adjust="hc1")
  
        ## Restricted model
      # Since both highways and buffer zones are treated as endogenous, interaction terms are not included in the first stage regressions
      # and therefore they are identical to the first stage regressions of the full model.
      # 1st stage #1
        ols_inst <- lm(paste0(hwy, "~ cc_bs  +  ",inst.lm," +  buffer_instrument +",control.vars), data=data)
        Ftest1 <- linearHypothesis(ols_inst, instruments)$F[2]
        Ftest1.all <- linearHypothesis(ols_inst, c(instruments,"buffer_instrument"))$F[2]
        data$instr <- predict.lm(ols_inst)
      # 1st stage #2
        ols_inst.buff <- lm(paste0("nota2_buff ~ cc_bs  +  ",inst.lm," +  buffer_instrument +",control.vars), data=data)
        Ftest1.buff <- linearHypothesis(ols_inst.buff, "buffer_instrument")$F[2]
        Ftest1.buff.all <- linearHypothesis(ols_inst.buff, c(instruments,"buffer_instrument"))$F[2]
        data$inst.buff <- predict(ols_inst.buff) 
        
    ols.restr <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr + inst.buff + ",control.vars), data=data)
    assign(paste0("ols.restr.inst.",hwy), ols.restr)
    ### Restricted results collapse into the original general model! 
          
          nn<- length(ols.restr$residuals); nk <- ols.restr$df.residual 
          y <- data[,pop_t]
          biv <- (ols.restr$coefficients)
          x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+ nota2_buff +",control.vars))), data=data)
          xh <- model.matrix(ols.restr$model, data=data)
          rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
          adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("adj.se.restr.inst.",hwy),adj.se)
    assign(paste0("Ftest1.restr.inst.",hwy), Ftest1)
    assign(paste0("Ftest1.restr.inst.all.",hwy), Ftest1.all)
    assign(paste0("Ftest1.restr.instbuff.",hwy), Ftest1.buff)
    assign(paste0("Ftest1.restr.instbuff.all.",hwy), Ftest1.buff.all)
    
    rm(ols_inst.buff, ols_inst, ols.full,ols.restr,Ftest1,Ftest1.all,Ftest1.buff,Ftest1.buff.all,Ftest1.hwybuff,Ftest1.hwybuff.all,adj.se)
  }   
  
stargazer(ols.full.Rays_1970_5k, ols.full.length_hwys_1970, ols.full.inst.Rays_1970_5k, ols.full.inst.length_hwys_1970,
  se = list(adj.se.full.Rays_1970_5k, adj.se.full.length_hwys_1970, adj.se.full.inst.Rays_1970_5k, adj.se.full.inst.length_hwys_1970),  
            type="html", digits = 5, single.row = FALSE, 
            notes = c("Robust standard errors in parentheses"), 
            out="H:/Desktop/0 - ERC/Data/R/model_results/bs2017_buffint_inst_limited.html", 
            title="(Dependent variable: Log. pop. growth (1970--1980))",
            order = c("cc_bs[0-9]:instr", "cc_bs[0-9]$", "cc_bs[0-9]:nota2_buff$", "cc_bs[0-9]:inst.buff", "nota2_buff"),
            keep.stat = c("n"), no.space = TRUE,
            add.lines = list(c("Highways instrumented", 
                               "Yes", "Yes","Yes","Yes"),
                             c("Buffer zone instrumented", 
                               "No", "No","Yes","Yes"),
                             c("Highways * Buffer zone instrumented", 
                               "No", "No","Yes","Yes"),
                             c("First stage F-test (Highways)",
                               format(Ftest1.full.Rays_1970_5k,digits=4),
                               format(Ftest1.full.length_hwys_1970,digits=4),
                               format(Ftest1.full.inst.all.Rays_1970_5k,digits=4),
                               format(Ftest1.full.inst.all.length_hwys_1970,digits=4)),
                             c("First stage F-test (Buffer zones)",
                               "","",
                               format(Ftest1.full.instbuff.all.Rays_1970_5k,digits=4),
                               format(Ftest1.full.instbuff.all.length_hwys_1970,digits=4)),
                             c("First stage F-test (Highways*Buffer zones)",
                               "","",
                               format(Ftest1.full.insthwybuff.all.Rays_1970_5k,digits=4),
                               format(Ftest1.full.insthwybuff.all.length_hwys_1970,digits=4)))
  )
  
  
### Export endogenous bufferzones in main results
        
  stargazer(main.ols.bsR, main.ols.bsl, main.ols.dtR, main.ols.dtl, ols.restr.inst.Rays_1970_5k, ols.restr.inst.length_hwys_1970,
            se = list(main.adj.se.bsR, main.adj.se.bsl, main.adj.se.dtR, main.adj.se.dtl, adj.se.restr.inst.Rays_1970_5k, adj.se.restr.inst.length_hwys_1970), 
            type="html", digits = 5, single.row = FALSE, 
            notes = c("Robust standard errors in parentheses"), 
            out="H:/Desktop/0 - ERC/Data/R/model_results/hwys1980_wbuff.html", 
            title="(Dependent variable: Log. pop. growth (1970--1980))",
            # order = c(27,28,29,30,31,1,2,3,5,6,7,4,8,9,10,11,12),
            order = c("instr$", "instr.", "cc_bs[0-9]$"),
            keep.stat = c("n"), no.space = TRUE,
            add.lines = list(c("Highways instrumented", 
                               "Yes", "Yes","Yes","Yes","Yes","Yes"),
                             c("Buffer zone instrumented", 
                               "No", "No","No","No","Yes","Yes"),
                             c("First stage F-test (Highways)",
                               format(Ftest.bsR,digits=4),
                               format(Ftest.bsl,digits=4),
                               format(Ftest.dtR,digits=4),
                               format(Ftest.dtl,digits=4),
                               format(Ftest1.restr.inst.all.Rays_1970_5k,digits=4),
                               format(Ftest1.restr.inst.all.length_hwys_1970,digits=4)),
                             c("First stage F-test (Buffer zones)",
                               "","","","",
                               format(Ftest1.restr.instbuff.all.Rays_1970_5k,digits=4),
                               format(Ftest1.restr.instbuff.all.length_hwys_1970,digits=4)))
  )
  
  # Hausman test for the endogeneity of the effect of buffers
      b1 = coefficients(main.ols.bsR)["nota2_buff"]
      b2 = coefficients(ols.restr.inst.Rays_1970_5k)["inst.buff"]
      v1 = diag(vcov(main.ols.bsR))["nota2_buff"]
      v2 = diag(vcov(ols.restr.inst.Rays_1970_5k))["inst.buff"]
      H.stat = t(b2-b1)%*%solve(v2-v1)%*%(b2-b1)
      p.value = 1-pchisq(H.stat,1) # Distributed chi-2 (df=1)
          
      paste0(hwy,"  Hausman test statistic : ",H.stat)
          paste0("P-value = ",p.value)
    
      b1 = coefficients(main.ols.bsl)["nota2_buff"]
      b2 = coefficients(ols.restr.inst.length_hwys_1970)["inst.buff"]
      v1 = diag(vcov(main.ols.bsl))["nota2_buff"]
      v2 = diag(vcov(ols.restr.inst.length_hwys_1970))["inst.buff"]
      H.stat = t(b2-b1)%*%solve(v2-v1)%*%(b2-b1)
      p.value = 1-pchisq(H.stat,1) # Distributed chi-2 (df=1)
      
          paste0(hwy,"  Hausman test statistic : ",H.stat)
          paste0("P-value = ",p.value)
        
      rm(b1,b2,v1,v2,H.stat,p.value)
    
        
  # F-test for the joined effect of highways in suburban buffers
  coefficients(ols.full.inst.length_hwys_1970)["cc_bs0:instr.hwybuff"] + coefficients(ols.full.inst.length_hwys_1970)["cc_bs0:instr"]
  linearHypothesis(ols.full.inst.length_hwys_1970,"cc_bs0:instr.hwybuff + cc_bs0:instr = 0", white.adjust="hc1")

    
  
# Highways and suburbanization - RAYS AND DENSITY TOGETHER (Baum-Snow (2007)) ------------------------------------
  
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar) 
  
  ols_inst_r <- lm(paste0("Rays_1970_5k ~ Rays_1821_5k + length_hwys_1821 +  cc_bs +",control.vars), data=data)
  summary(ols_inst_r)
  adj.se.ols_inst_r <- coeftest(ols_inst_r, vcov = vcovHC(ols_inst_r, "HC1")) 
  Ftest_r <- linearHypothesis(ols_inst_r, names(ols_inst_r$coefficients)[grep("1821",names(ols_inst_r$coefficients))])$F[2]
  data$instr_r <- predict.lm(ols_inst_r)
  
  ols_inst_l <- lm(paste0("length_hwys_1970 ~ Rays_1821_5k + length_hwys_1821 +  cc_bs +",control.vars), data=data)
  summary(ols_inst_l)
  adj.se.ols_inst_l <- coeftest(ols_inst_l, vcov = vcovHC(ols_inst_l, "HC1")) 
  Ftest_l <- linearHypothesis(ols_inst_l, names(ols_inst_l$coefficients)[grep("1821",names(ols_inst_l$coefficients))])$F[2]
  data$instr_l <- predict.lm(ols_inst_l)
  
  ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr_r  + cc_bs:instr_l +",control.vars), data=data)
  summary(ols)
  linearHypothesis(ols, "cc_bs1:instr_r=cc_bs2:instr_r")
  linearHypothesis(ols, "cc_bs1:instr_l=cc_bs2:instr_l")
  linearHypothesis(ols, "cc_bs0:instr_r=cc_bs0:instr_l")
  
      nn<- length(ols$residuals); nk <- ols$df.residual 
      y <- data[,pop_t]
      biv <- (ols$coefficients)
      x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + ",paste0("cc_bs:",hwys, collapse=" + ")," + ",control.vars))), data=data)
      xh <- model.matrix(ols$model, data=data)
      rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
      adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
  
  
  stargazer(ols, se=list(adj.se),
    type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
    out="H:/Desktop/0 - ERC/Data/R/model_results/hwys1980_together.html", 
    order = c("cc_bs[0-9]:instr_r", "cc_bs[0-9]:instr_l", "cc_bs[0-9]$"),
    # covariate.labels = c("Highway rays*periphery ","Highway rays*suburbs ","Highway rays*central city","Highway density*periphery ","Highway density*suburb","Highway density*central city","Suburbs","Central city","Rail stations (1930)","Pop. (ln, 1930)","Pop. (ln, 1960)","Buffer zone share","Nature coverage share","Green heart share","Distance to central city","Reclaimed area","Constant"),
    keep.stat = c("n"), no.space = TRUE,
    add.lines = list(c("Instrumented", "Yes"),
                     c("Kleibergen-Paap F-statistic (Rays)",format(Ftest_r,digits=4)),
                     c("Kleibergen-Paap F-statistic (Density)",format(Ftest_l,digits=4)))
  )
  
  rm(ols,adj.se)

  

# Smoothing ---------------------------------------------------------------
  
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960 + nota2_buff + hgn_nat1960_mean + gh_1958 + dist_cc  +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar) 
  
  smooth <- snelwegen %>% filter(Year>=1960 & Year<=1990) %>% select(Year, lengte) %>%
    mutate(smooth.1970 = (lengte - lengte[Year==1960])/(lengte[Year==1970] - lengte[Year==1960]),
           smooth.1980 = (lengte - lengte[Year==1970])/(lengte[Year==1980] - lengte[Year==1970]),
           smooth.1990 = (lengte - lengte[Year==1980])/(lengte[Year==1990] - lengte[Year==1980]))
  smooth.1970 <- 1- mean(smooth$smooth.1970[smooth$Year<=1970])
  smooth.1980 <- 1- mean(smooth$smooth.1980[smooth$Year<=1980 & smooth$Year>=1970])
  smooth.1990 <- 1- mean(smooth$smooth.1990[smooth$Year>=1980])
  print(smooth.1980)
  rm(smooth)
  
  data %>% group_by(cc_bs) %>% summarise(mean.r = mean(Rays_t1980), mean.l = mean(length_t1980))
  
  hwys = c("Rays_1970_5k","length_hwys_1970")
  for (sm in c(0.25,smooth.1980,0.5,0.75)){
    # Smooth
    data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds")
    data$Rays_1970_5k <- data$Rays_1970_5k + (data$Rays_1980_5k - data$Rays_1970_5k)*sm
    data$length_hwys_1970 <- data$length_hwys_1970 + (data$length_hwys_1980 - data$length_hwys_1970)*sm
    
  for (hwy in hwys){
    ols.sm <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:",hwy,"+",control.vars), data=data)
    summary(ols.sm)
    
    ols_inst <- lm(paste0(hwy, "~ cc_bs +",inst.lm," + ",control.vars), data=data)
    summary(ols_inst) 
    adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
    Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
    data$instr <- predict.lm(ols_inst)
    ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data)
    summary(ols)
    linearHypothesis(ols, "cc_bs1:instr=cc_bs2:instr")
    
        nn<- length(ols$residuals); nk <- ols$df.residual 
        y <- data[,pop_t]
        biv <- (ols$coefficients)
        x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy," + ",control.vars))), data=data)
        xh <- model.matrix(ols$model, data=data)
        rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
        adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.bs.1st.",substr(hwy,1,1),".",sm),adj.se.ols_inst)
    assign(paste0("ols.bs.1st.R2.",substr(hwy,1,1),".",sm),summary(ols_inst)$adj.r.squared)
    assign(paste0("ols.bs",substr(hwy,1,1),".",sm),ols)
    assign(paste0("adj.se.bs.",substr(hwy,1,1),".",sm),adj.se)
    assign(paste0("Ftest.bs",substr(hwy,1,1),".",sm), Ftest)
    assign(paste0("ols.sm.",substr(hwy,1,1),".",sm), ols.sm)
  }
  }
  listolsR <-mixedsort(apropos("ols.bsR."))
  listseR <-mixedsort(apropos("adj.se.bs.R"))
  listolsl <-mixedsort(apropos("ols.bsl."))
  listsel <-mixedsort(apropos("adj.se.bs.l"))
  stargazer(mget(listolsR),mget(listolsl), 
            se = mget(c(listseR,listsel)), digits = 5,
            out = "H:/Desktop/0 - ERC/Data/R/model_results/smooth_1980.html", type = "html", 
            order = c("cc_bs[0-9]:instr", "cc_bs[0-9]$"), no.space = TRUE,
            add.lines = list(c("model", listolsR, listsel)))
  rm(list = c(listolsR,listseR,listolsl,listsel))
  
  listednamesR <-mixedsort(apropos("ols.bs.1st.R.0"))
  listednamesl <-mixedsort(apropos("ols.bs.1st.l"))
  stargazer(mget(listednamesR),mget(listednamesl), out = "H:/Desktop/0 - ERC/Data/R/model_results/smooth_1980_1st.html", type = "html", 
            order = c("cc_bs[0-9]:instr", "cc_bs[0-9]$"), no.space = TRUE,
            add.lines = list(c("model", listednamesR, listednamesl)))
  rm(list = c(listednamesR,listednamesl))
  
  
# Highways and suburbanization (Main results under COROP- clustered errors) ------------------------------------
  
  # COROP regions
  library(raster)
  
    nl.80 = read_sf("H:/Desktop/0 - ERC/Data/GIS/Municipal borders_jasper/municipalities/nl_1980.shp")
    nl.corop = read_sf("c:/Data/WBK_CBS/NL.shp")
    nl.corop <- nl.corop %>% st_transform(st_crs(nl.80))
    r <- raster(as(nl.corop, "Spatial"), res=250)
    rr <- rasterize(as(nl.corop, "Spatial"), r, "COROPNUMME", progress = "text")
    rr[is.na(rr)] <- 0
    plot(rr); plot(nl.80, add=TRUE)
    
    nl.80 <- as(nl.80, "Spatial")
    corop.extract <- extract(rr, nl.80, fun = median)
    corop.table <- data.frame(code= nl.80$GM_1980,corop = corop.extract)
    corop.table$code <- as.numeric(as.character(corop.table$code))
    
    data<- data %>% left_join(corop.table, by="code") 
    
    rm(nl.80, nl.corop, r, rr, corop.extract, corop.table)
    detach(raster);library(tidyverse)
  
  ## Highways and suburbanization        
  
  # data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds") 
  
  ols <- lm(paste0(pop_t, " ~ cc_bs + cc_bs:",hwy," +",control.vars), data=data)
  summary(ols)  
  
  
  for (hwy in hwys){
    ols_inst <- lm(paste0(hwy, "~ cc_bs +",inst.lm,"+",control.vars), data=data)
    summary(ols_inst) 
    adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
    Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
    data$instr <- predict.lm(ols_inst)
    ols <- lm(paste0(pop_t, " ~  cc_bs + cc_bs:instr +",control.vars), data=data)
    summary(ols)
    linearHypothesis(ols, "cc_bs1:instr=cc_bs2:instr", white.adjust = "hc1")
    
    nn<- length(ols$residuals); nk <- ols$df.residual 
    y <- data[,pop_t]
    biv <- (ols$coefficients)
    x <- model.matrix(eval(parse(text=paste0(" ~ cc_bs + cc_bs:",hwy,"+",control.vars))), data=data)
    xh <- model.matrix(ols$model, data=data)
    rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
    
    omega=0
    for (g in order(unique(data$corop))){
      xhg = xh[which(data$corop==g),]
      yg = y[which(data$corop==g)]
      eg = yg-xhg%*%biv
      psig = kronecker(eg, t(eg), FUN = "*")
      omega = omega + t(xhg)%*%psig%*%xhg
    }
    rob.vcov2 <- nk*(  solve(t(xh)%*%(xh)) %*%   (omega/nk)   %*% solve(t(xh)%*%(xh)))
    adj.se<- sqrt(diag(rob.vcov2)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.bs.1st.",substr(hwy,1,1)),adj.se.ols_inst)
    assign(paste0("ols.bs.1st.R2.",substr(hwy,1,1)),summary(ols_inst)$adj.r.squared)
    assign(paste0("ols.bs.corop.",substr(hwy,1,1)),ols)
    assign(paste0("adj.se.bs.corop.",substr(hwy,1,1)),adj.se)
    assign(paste0("Ftest.bs",substr(hwy,1,1)), Ftest)
  }   
  
  
  stargazer(ols.bs.corop.R,ols.bs.corop.l, 
            se = list(adj.se.bs.corop.R, adj.se.bs.corop.l),
            type="html", digits = 5, notes =  c("(1) Robust standard errors in parentheses","(2) Highway extent variables are in 1970 levels","(3) Highway density is expressed in logarithm"), single.row = FALSE, 
            out="H:/Desktop/0 - ERC/Data/R/model_results/hwys_1980_corop_cluster.html", 
            dep.var.labels   = c("IV","IV","OLS","OLS"), dep.var.caption = "", 
            keep.stat = c("n","rsq"), no.space = TRUE,
            order = c("cc_bs[0-9]:instr", "cc_bs[0-9]$")
  )
  
  
# Highways and suburbanization - Interactions with belts (Jun 2018) ---------

  gc()
  data <- readRDS("H:/Desktop/0 - ERC/Data/data_1980_50000.rds")
  control.vars <- c("rail_stations_1930 + pop_1930 + pop_1960  + hgn_nat1960_mean + gh_1958 + dist_cc + ln_pop_cc +  reclaimed")
  control.vars <- paste(control.vars," + ",soilvar) 
  
  data.belt = data  # %>% filter(cc_bs==0) 
  
  for (hwy in hwys){
    ols_inst <- lm(paste0(hwy, "~  belt +",inst.lm,"+",control.vars), data=data.belt)
    summary(ols_inst) 
    adj.se.ols_inst <- coeftest(ols_inst, vcov = vcovHC(ols_inst, "HC1")) 
    Ftest <- linearHypothesis(ols_inst, instruments)$F[2]
    data.belt$instr <- predict.lm(ols_inst)
    ols <- lm(paste0(pop_t, " ~   belt + belt:instr +",control.vars), data=data.belt)
    summary(ols)
    ttest = linearHypothesis(ols, "belt_around_CC:instr=belt_around_Nota2Buffer:instr", white.adjust = "hc1")
    
    nn<- length(ols$residuals); nk <- ols$df.residual 
    y <- data.belt[,pop_t]
    biv <- (ols$coefficients)
    x <- model.matrix(eval(parse(text=paste0(" ~  belt + belt:",hwy,"+",control.vars))), data=data.belt)
    xh <- model.matrix(ols$model, data=data.belt)
    rob.vcov1 <- nk*(  solve(t(xh)%*%(xh)) %*%   (t(xh) %*% diag(as.vector((y-x%*%biv)*(y-x%*%biv))) %*% xh/nk)   %*% solve(t(xh)%*%(xh)))
    adj.se<- sqrt(diag(rob.vcov1)) ## Test with robust standard errors from stata
    
    assign(paste0("ols.bs.belt",substr(hwy,1,1)),ols)
    assign(paste0("adj.se.bs.belt",substr(hwy,1,1)),adj.se)
    assign(paste0("Ftest.bs.belt",substr(hwy,1,1)), Ftest)
    assign(paste0("ttest.belt.",substr(hwy,1,1)), ttest)
  }   
  
  stargazer(ols.bs.beltR,ols.bs.beltl,
            se = list(adj.se.bs.beltR, adj.se.bs.beltl), 
            type="html", digits = 5, single.row = FALSE, 
            order = ("instr") ,
            out=paste0("H:/Desktop/0 - ERC/Data/R/model_results/bs_belt_",beltsize,".html"), 
            title="(Dependent variable: Log. pop. growth (1970--1980))",
            keep.stat = c("n"), no.space = TRUE,
            add.lines = list(c("Instrumented", "Yes", "Yes"),
                             c("First stage F-test",
                               format(Ftest.bs.beltR,digits=4),
                               format(Ftest.bs.beltl,digits=4)),
                             c("P-value Hypothesis test", 
                               format(ttest.belt.R[[4]][2],digits=4),
                               format(ttest.belt.l[[4]][2],digits=4)))
  )
# Model illustration (2) ------------------------------------------------------

  alpha = 0.5 ;   beta = 0.5
  income = 5
  ubar = 20
  tcost.h = 5 ; tcost.l = 2.5;
  distance <- seq(0,70,1)
  rbar = -11
  
  ln1l <- function(distance, tcost, ubar){
    ln1l = log((1-beta)^(1/beta) + alpha^(1/beta)) - (1/beta)*log(ubar - income + tcost*distance)
    return(ln1l)
  }
  
  x1.buff = max(distance[ln1l(distance, tcost.h, ubar)>rbar])
  x1buff.ind = which(distance==x1.buff)
  x2.buff = x1.buff + 15
  x2buff.ind = which(distance==x2.buff)
  n.size = x1.buff*-rbar - abs(sum(ln1l(distance, tcost.h, ubar)[1:(x1buff.ind-1)]))
  n.size
  
  fixubar <- function(newtcost){
      ubar.n = ubar
      delta.n = 10000
      while (abs(delta.n)>=0.00001){
        xbar = max(distance[ln1l(distance, newtcost, ubar.n)>rbar])
        xbar.ind = max(which(ln1l(distance, newtcost, ubar.n)>rbar))
        delta.n <- (n.size - 
                      abs(xbar*rbar-
                      sum(ln1l(distance, newtcost, ubar.n)[1:(xbar.ind-1)])
                      )
                      )
        ubar.n <- ubar.n - 0.01*delta.n
        if (ubar.n<(income)){
          print("Function is not defined - change paramters")
          break
        }
      }
      print(paste("delta.n = ",delta.n, "   ubar.n = ", ubar.n, "  x.bar = ", xbar))
      fixubar.res <- data.frame(ubar = ubar.n,
                            xbar = xbar)
      return(fixubar.res)
  }
  
  psi <- (x2.buff - x1.buff)*(-rbar-0.25) - abs(sum(ln1l(distance, tcost.l, ubar)[(x1buff.ind+1):(x2buff.ind-1)]))
        
  ln1l.res <- data.frame(distance, 
                         ttcost.l = ln1l(distance, tcost.l, fixubar(tcost.l)$ubar), 
                         ttcost.l.psi = ln1l(distance, tcost.l, fixubar(tcost.l)$ubar) + 0.6*psi/(fixubar(tcost.l)$xbar),
                         ttcost.l.psi2 = ln1l(distance, tcost.l, fixubar(tcost.l)$ubar) + 1.5*psi/(fixubar(tcost.l)$xbar), 
                         ttcost.h = ln1l(distance, tcost.h, fixubar(tcost.h)$ubar)
  )
  abs(fixubar(tcost.l)$xbar*rbar - sum(ln1l(distance, tcost.l, fixubar(tcost.l)$ubar)[1:(fixubar(tcost.l)$xbar+1)]))
  abs(fixubar(tcost.h)$xbar*rbar - sum(ln1l(distance, tcost.h, fixubar(tcost.h)$ubar)[1:(fixubar(tcost.h)$xbar+1)]) )
  
  ln1l.res$ttcost.l.psi[(x1buff.ind+1):(x2buff.ind-1)] <- NA
  ln1l.res$ttcost.l.psi2[(x1buff.ind+1):(x2buff.ind-1)] <- NA
  
  ln1l.res <- 
    ln1l.res %>% 
    gather(variable, value, -distance) %>% 
    mutate(value = ifelse(value<rbar, rbar, value)) %>%
    spread(variable, value)
  
  gr.hwys <-
  ggplot(ln1l.res) + aes(distance) + 
    geom_ribbon(aes(ymin = rbar, ymax = ttcost.l.psi2), fill = "grey70") +
    geom_rect(aes(xmin=x1.buff, xmax=x2.buff, ymin= rbar ,ymax= rbar+0.25), fill="grey70") +
    geom_rect(aes(xmin=0, xmax=max(distance), ymin= rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE) ,ymax= rbar), fill="grey70") +
    geom_path(aes(y = ttcost.l.psi2), size = 1) +
    geom_path(aes(y = ttcost.l.psi), size = 1) +
    geom_path(aes(y = ttcost.l), size = 1) +
    geom_path(aes(y = ttcost.h), size = 1) +
    
    geom_segment(aes(x = x1.buff, y = rbar, xend = x1.buff, yend = max(ttcost.h, na.rm = TRUE)), linetype = 3, size=1, color = "black") +
    geom_segment(aes(x = x2.buff, y = rbar, xend = x2.buff, yend = max(ttcost.h, na.rm = TRUE)), linetype = 3, size=1, color = "black") +
    
    geom_segment(aes(x = 0, y = rbar, xend = max(distance), yend =rbar), linetype = 2, size=0.5, color = "black") +
    geom_segment(aes(x = 0, y = rbar+0.25, xend =max(distance), yend =rbar+0.25), linetype = 2, size=0.5, color = "black") +
    geom_segment(aes(x = max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l])+0.5, y = rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE), xend =max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l])+0.5, yend =min(ln1l.res$ttcost.h[ln1l.res$ttcost.h > ln1l.res$ttcost.l])-0.05), linetype = 2, size=0.5, color = "black") +
    geom_segment(aes(x = max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l.psi], na.rm = TRUE)+0.5, y = rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE), xend =max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l.psi], na.rm = TRUE)+0.5, yend =min(ln1l.res$ttcost.h[ln1l.res$ttcost.h > ln1l.res$ttcost.l.psi], na.rm = TRUE)-0.05), linetype = 2, size=0.5, color = "black") +
    
    geom_segment(aes(x = x1.buff, y = rbar+0.25, xend =x2.buff, yend =rbar+0.25), linetype = 1, size=1, color = "black") +
    geom_segment(aes(x = 0, y = rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE), xend = max(distance), yend =rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE)), linetype = 1, size=1, color = "black") +
    geom_segment(aes(x = 0, y = rbar-0.25*sd(ln1l.res$ttcost.h, na.rm = TRUE), xend = 0, yend =max(ttcost.h, na.rm = TRUE)), linetype = 1, size=1, color = "black") +
    
    
    annotate("text", x = x1.buff, y = rbar-0.5, parse = TRUE, label =as.character(expression(x[1])), size=4) +
    annotate("text", x = x2.buff, y = rbar-0.5, parse = TRUE, label =as.character(expression(x[2])), size=4) +
    annotate("text", x = max(ln1l.res$distance[ln1l.res$ttcost.l.psi>rbar], na.rm = TRUE), y = rbar-0.5, parse = TRUE, label =as.character(expression(bar(x))), size=4) +
    annotate("text", x = max(ln1l.res$distance[ln1l.res$ttcost.l.psi2>rbar], na.rm = TRUE), y = rbar-0.5, parse = TRUE, label =as.character(expression(bar(x)["e"])), size=4) +
    annotate("text", x = max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l])+0.5, y = rbar-0.5, parse = TRUE, label =as.character(expression(hat(x)["1"])), size=4) +
    annotate("text", x = max(ln1l.res$distance[ln1l.res$ttcost.h > ln1l.res$ttcost.l.psi], na.rm = TRUE)+0.5, y = rbar-0.5, parse = TRUE, label =as.character(expression(hat(x)["2"])), size=4) +
    
    annotate("text", x = -2, y = rbar, parse = TRUE, label =as.character(expression(bar(s))), size=4) +
    annotate("text", x = -2, y = -6.2, parse = TRUE, label =as.character(expression(S^{1})), size=4) +
    annotate("text", x = -2, y = -8.5, parse = TRUE, label =as.character(expression(S[a]^{2})), size=4) +
    annotate("text", x = -2, y = -8.1, parse = TRUE, label =as.character(expression(S[b]^{2})), size=4) +
    annotate("text", x = -2, y = -7.7, parse = TRUE, label =as.character(expression(S[c]^{2})), size=4) +
    annotate("text", x = -2, y = rbar + 0.25, parse = TRUE, label =as.character(expression(s^{"*"})), size=4) +
    annotate("text", x = (8*x1.buff+x2.buff)/9, y = rbar + 0.43, parse = TRUE, label =as.character(expression(psi)), size=4) +
    scale_color_manual(values = c("black","black","black")) +
    theme_classic() + theme(legend.position="none", axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),  axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + xlab("Distance") + ylab("log (Population density)")
  
  
  gr.hwys
  ggsave(file="H:/Desktop/0 - ERC/Data/R/Fig1.png", gr.hwys)
  
  
# Model illustration - manual ---------------------------------------------

xmax = 55
sagr = 3.612075
fig = data.frame(dist = seq(0,xmax,0.01))
fig$s1 = 3.1748/(0.0255+0.1*fig$dist)^1.6667
fig$s2 = 3.1748/(0.2351+0.015*fig$dist)^1.6667
fig$s3 = 3.1748/(0.1695+0.015*fig$dist)^1.6667
fig$s4 = 3.1748/(0.1151+0.015*fig$dist)^1.6667

fig[fig$dist>=10 & fig$dist <=25, c('s3','s4')] <- NA

fig <- fig %>% gather(var, value, -dist)

fig$value[fig$value <=sagr] <- sagr

gr.hwys<-
ggplot(fig) + aes(x = dist, color = var) +
  geom_ribbon(aes(ymin = 0, ymax = value), fill = "grey70", alpha = 0.25) +
  geom_line(aes(y=value), size = 1) + 
  coord_cartesian(xlim = c(0, xmax), ylim = c(0,150)) +
  annotate("text", x = -2, y = 150, parse = TRUE, label =as.character(expression(S^{1})), size=4) +
  annotate("text", x = -2, y = 35, parse = TRUE, label =as.character(expression(S^{2})), size=4) +
  annotate("text", x = -2, y = 65, parse = TRUE, label =as.character(expression(S^{3})), size=4) +
  annotate("text", x = -2, y = 115, parse = TRUE, label =as.character(expression(S^{4})), size=4) +
  annotate("text", x = -2, y = sagr, parse = TRUE, label =as.character(expression(s^{agr})), size=4) +
  annotate("text", x = 10, y = -4, parse = TRUE, label =as.character(expression(x^{l})), size=4) +
  annotate("text", x = 25, y = -4, parse = TRUE, label =as.character(expression(x^{u})), size=4) +
  annotate("text", x = 9.04, y = -4, parse = TRUE, label =as.character(expression(bar(x^{1}))), size=4) +
  annotate("text", x = 46, y = -4, parse = TRUE, label =as.character(expression(bar(x^{2}))), size=4) +
  annotate("text", x = 50, y = -4, parse = TRUE, label =as.character(expression(bar(x^{3}))), size=4) +
  annotate("text", x = 54, y = -4, parse = TRUE, label =as.character(expression(bar(x^{4}))), size=4) +
  annotate("text", x = 2.465882, y = -4, parse = TRUE, label =as.character(expression(hat(x)["1"])), size=4) +
  annotate("text", x = 1.694118, y = -4, parse = TRUE, label =as.character(expression(hat(x)["2"])), size=4) +
  geom_segment(aes(x = 0, y = 0, xend = xmax, yend =0), linetype = 1, size=1, color = "black") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend =150), linetype = 1, size=1, color = "black") +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend =150), linetype = 3, size=1, color = "black") +
  geom_segment(aes(x = 25, y = 0, xend = 25, yend =150), linetype = 3, size=1, color = "black") +
  geom_segment(aes(x = 2.465882, y = 0, xend = 2.465882, yend =fig$value[fig$dist==2.46 & fig$var =='s1']), linetype = 2, size=0.5, color = "black") +
  geom_segment(aes(x = 1.694118, y = 0, xend = 1.694118, yend =fig$value[fig$dist==1.69 & fig$var =='s1']), linetype = 2, size=0.5, color = "black") +
  geom_segment(aes(x = 0, y = sagr, xend = xmax, yend =sagr), linetype = 2, size=0.5, color = "black") +
  scale_color_manual(name="",values=c("s1"="black","s2"="black","s3"="black","s4"="black")) +
  theme_classic() + theme(legend.position="none", axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),  axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + xlab("Distance") + ylab("log (Population density)")

gr.hwys
ggsave(file="C:/Users/Orlevko/Documents/0 - ERC/Data/R/Fig1_fix.png", gr.hwys)
  

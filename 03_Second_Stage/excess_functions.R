# funciones
make.data.excess<-function(data,sex){
  
  if("geometry" %in% colnames(data)){
    #"FEMENINO"  "MASCULINO"
    data2<-data %>% 
           select(-geometry)%>%
           filter(sexo==sex)
    
    return(data2)
    }else{
    data2<-data %>% 
           filter(sexo==sex)
    
    E <- expected(
      population = data2$population,
      cases = data2$n, n.strata = 4)
    
    data3<-data2 %>% 
      select(reg,prov,year,week,sexo,edad_cat_old,n) %>% 
      group_by(reg,prov,year,week,sexo) %>% 
      summarise(n=sum(n))
    
    #data3$E <- E[match(data3$prov, unique(data2$prov))]
    data3$E <-E 
    
    data3<-data3 %>% 
           ungroup() %>% 
           group_by(reg,prov) %>% 
           mutate(index_area=group_indices(),
                  index_year=as.numeric(as.factor(year)))
    
    
    return(data3) 
    return(data2)
    
  }

}


make.data.excess_ver.2<-function(data,sex,edad){
  
  if("geometry" %in% colnames(data)){
    #"FEMENINO"  "MASCULINO"
    data2<-data %>% 
      select(-geometry)%>%
      filter(sexo==sex)
    
    return(data2)
  }else{
    data2<-data %>% 
      filter(sexo==sex) %>% 
      ungroup() %>% 
      group_by(reg,prov) %>% 
      mutate(index_area=group_indices(),
             index_year=as.numeric(as.factor(year)))
    
    
  }
}

inla.batch.predict<-function(formula, dat1 = db.excess.train) {
  result = inla(formula, data=dat1, family="nbinomial", verbose = F, offset=log(E), 
                control.inla=list(strategy="adaptive"),
                control.compute=list(config=T, dic=T, cpo=T, waic=T,
                                     #se requiere config = TRUE 
                                     return.marginals = FALSE),
                control.fixed = list(correlation.matrix=T),
                control.predictor=list(link=1,compute=TRUE)
  )
  return(result)
}


inla.batch.det<-function(formula, dat1 = datos) {
  result = inla(formula, data=dat1, family="nbinomial", verbose = F, offset=log(E), 
                control.inla=list(strategy="adaptive"),
                control.compute=list(config=T, dic=T, cpo=T, waic=T,
                                     #se requiere config = TRUE 
                                     return.marginals = FALSE),
                control.fixed = list(correlation.matrix=T),
                control.predictor=list(link=1,compute=TRUE)
  )
  return(result)
}

inla.batch.predict.bru<-function(formula, dat1 = datos.proc) {
  result = inla(formula, data=dat1, family="nbinomial", verbose = F, offset=log(E), 
                control.inla=list(strategy="adaptive"),
                control.compute=list(config=T, dic=T, cpo=T, waic=T,
                                     #se requiere config = TRUE 
                                     return.marginals = FALSE),
                control.fixed = list(correlation.matrix=T),
                control.predictor=list(link=1,compute=TRUE)
  )
  return(result)
}

inla.batch.predict.safe <- possibly(inla.batch.predict, otherwise = NA_real_)

inla.batch.predict.bru.safe <- possibly(inla.batch.predict.bru, otherwise = NA_real_)

inla.batch.det.safe <- possibly(inla.batch.det, otherwise = NA_real_)


std.var<-function(variable){
  var_std = ((variable - mean(variable,na.rm=T)))/sd(variable,na.rm = T)         
  return(var_std)}

################################################
##### graficos #################################
################################################




##################
# Random effects
##################

plot_random2_ver3 <- function(model, lab1 = NULL, scaled = NULL,
                              scaled_vars = 1,
                              analysis_vars = unique(mydata$var),
                              scaled_means = NULL, 
                              scaled_sds = NULL, 
                              y_lab = "(IRR)", 
                              name="", vars = NULL) {
  
  #  print("Deprecated, update to plot_random3")
  summ <- summary(model)
  a <- scaled_vars
  b <- scaled_means
  c <- scaled_sds
  
  {if(is.null(scaled)) {
    mydata <- model$summary.random %>% bind_rows( .id = "var") %>% filter(var!="index")}
    else{
      mydata <- model$summary.random %>% bind_rows( .id = "var") %>% 
        filter(var %in% scaled_vars) 
      
      names(scaled_sds)<-scaled_vars
      names(scaled_means)<-scaled_vars
      
      mydata$ID<-(mydata$ID * scaled_sds[mydata$var]) + scaled_means[mydata$var]
      
      mydata <- mydata %>%  # no estaba enviando esta operacion a ningun lado 
        bind_rows(model$summary.random %>% bind_rows( .id = "var") %>% 
                    filter(var!="index", !var %in% scaled_vars)) %>% filter(var %in% analysis_vars)
    }
  }
  
  library(ggthemes)
  
  lab2 <- lab1
  
  if(!is.null(vars)) {mydata <- mydata %>% dplyr::filter(var %in% vars)} 
  # !is.null(vars) no se activará por defecto pues vars es NULL
  if(!is.null(lab1)) {names(lab2) <- mydata %>% arrange(var) %>% distinct(var) %>% unlist()}
  
  new_order <- scaled_vars
  mydata2 <- arrange(transform(mydata, var = factor(var, levels = new_order)), var)
  mydata_names <- colnames(mydata)
  colnames(mydata2) <- mydata_names
  levels(mydata2$var) <- lab2
  
  plot<- mydata2 %>% ggplot(aes(x = ID, y = exp(`0.5quant`))) +
    geom_ribbon(aes(ymin = exp(`0.025quant`), ymax = exp(`0.975quant`)), fill="grey90", size=.3, alpha = .6) +
    geom_line(color = "orange") +
    scale_x_continuous(expand=c(0,0)) +
    geom_hline(aes(yintercept = 1), col="black", linetype="dashed", size=.4) +
    labs(title=name, x="", y=y_lab) +
    theme_base() +
    theme(plot.background = element_blank(),
          strip.placement = "outside") +  
    {if(!is.null(lab1)) {facet_wrap(.~var, scales = "free",  strip.position = "bottom")}
      else{facet_wrap(.~var, scales = "free",  strip.position = "bottom")}
    }
  
  return(plot)
}






####################################################################################
#Graficos sin exponenciar ##########################################################



plot.log.fixed<-function(model, label = id, col1="black", dire=-1, breaks, lab_mod, lim, title, legend=NULL, filter, ylab = "Relative risk", 
                         ylim = 10, angle=45, hjust = 1, xlabs=lim, b.size=20, flip=F) {
  library(ggsci)
  #Acepta objetos tipo lista
  label <- enquo(label)
  
  extract_dat <- function(model) {
    mydata <- model$summary.fixed %>%
      rownames_to_column(var = "var")
    return(mydata)
  }
  
  dat_na <- data.frame(var=NA)
  
  extract_dat.safe <- possibly(extract_dat, otherwise = dat_na)
  
  fixed_log_plot <- model %>% purrr::map(~extract_dat.safe(model=.)) %>%
    bind_rows(.id = "formula") %>%
    mutate(id = match(formula, unique(formula))) %>%
    #mutate(id = group_indices(., factor(formula, levels = unique(formula)))) %>%
    filter(!is.na(`0.025quant`), `0.5quant`<filter) %>%
    ggplot(aes(x = var, y = `0.5quant`, col=factor(id))) +
    geom_point(position = position_dodge(0.3),col="black") +
    geom_errorbar(aes(ymin = `0.025quant`, ymax = `0.975quant`), width=.1, position = position_dodge(0.3),col="darkblue") +
    geom_hline(aes(yintercept = 0), col=col1, linetype="dashed", size=.4) +
    #scale_color_discrete(name="Models", breaks=breaks, labels=lab_mod) +
    ggsci::scale_color_npg(name="Models", breaks=breaks, labels=lab_mod) +
    scale_x_discrete(limits=lim, labels = xlabs) +
    ylim(-1, ylim) +
    {if(!is.null(legend)) {guides(color = legend)}} +
    labs(title=title, x = "variables") +
    theme_bw(base_size = b.size) +
    labs(y=ylab) +
    {if(isTRUE(flip)) {coord_flip()}} +
    theme(axis.text.x = element_text(angle = angle, hjust = hjust))
  
  return(fixed_log_plot)
}

plot.log.fixed.safe <- possibly(plot.log.fixed, otherwise = NA_real_)





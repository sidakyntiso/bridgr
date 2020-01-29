bam <- function(df){
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  #identify the number of graders and assign corelta based this
  df$ta <- as.character(unlist(df$ta))
  
  #remove irrelevant columns
  ta <- unique(df$ta)
  df <- df[,c("ta",ta)]
  
  #identify the number of bridging observations
  g =  nrow(df[!apply(df == ""|is.na(df), 1, all),])
  
  len <- length(unique(df$ta))
  
  core1ta<-ifelse(df$ta==unique(df$ta)[1], df[,unique(df$ta)[1]], NA)
  for (i in c(2:len)){
    core1ta<-ifelse(df$ta==unique(df$ta)[i], df[,unique(df$ta)[i]], core1ta)
  }

  avg <- rowMeans(df[,c(2:(len+1))],na.rm =T) 
  
  #Get proper rank
  corpos<-rank(avg, na.last="keep", ties.method= "average")
  
  df$corelta <- core1ta
  df$avg <- avg
  df$corpos <- corpos
  
  #Remove all rows with NA for a  score
  df1<-df[!is.na(df$avg),]

  #stan settings
  ITER <- 6000
  WARMUP <- 3000
  THIN <- 1
  CHAINS<-3
  num<-length(df1$core1ta)
  colta<-df1$core1ta
  rowta<-df1$ta
  


  #Cycle through number of bridges
  try({
      set.seed(12102019)
    
      matal<-cbind(df1[,c(2:(len+1))])
      mataldf<-as.data.frame(matal)
      #Make all grades integers by multiplying by 2
      #matalt2<-matal*2
      #mataldf<-as.data.frame(matalt2)
      mataldf[is.na(mataldf)] <- -1
      wd <- mataldf
      N <- nrow(wd)
      J <- ncol(wd)
      data=list(N=N, J=J, wdata=wd)
      posterior.sim <-stan(file = 'bridgegapaml3.stan', data = data,
                           iter=ITER, thin=THIN, chains=CHAINS,
                           warmup = WARMUP,control = list(adapt_delta = 0.9, max_treedepth = 12))
      
      #extract alphas and get grader medians
      alpha.dat<- data.frame(extract(posterior.sim, pars="alpha", inc_warmup=F, permuted=F))
      
      #extract betas and get grader medians
      beta.dat<- data.frame(extract(posterior.sim, pars="beta", inc_warmup=F, permuted=F))
      
      #real scores
      z<- extract(posterior.sim, pars="Z", inc_warmup=F, permuted=F)
      rm(posterior.sim)
      
      #extract medians
      zmeds<-monitor(z, inc_warmup=F, print=F)[,6]
      #ranks of meds
      zmedsrank<-rank(zmeds, na.last="keep", ties.method= "average")
      #actual grades
      coregradmeds <- median(unlist(alpha.dat)) + zmeds*median(unlist(beta.dat))
      #meds MAE
      zmedsae<-mean(abs(df1$corpos-zmedsrank))
      #Squared error
      zmedssqerr<-sqrt(mean((df1$corpos-zmedsrank)^2))

      #extract means
      zmeans<-monitor(z, inc_warmup=F, print=F)[,1]
      #ranks of meds
      zmeansrank<-rank(zmeans, na.last="keep", ties.method= "average")
      #grades
      coregradmeans <- mean(unlist(alpha.dat),na.rm = T) + zmeans*mean(unlist(beta.dat),na.rm = T)
      #meds MAE
      zmeansae<-mean(abs(df1$corpos-zmeansrank))
      #Squared error
      zmeanssqerr<-sqrt(mean((df1$corpos-zmeansrank)^2))
  })

  #sum_results <- cbind(zmedsae,zmedssqerr,zmeansae,zmeanssqerr)
  #colnames(sum_results) <- c("zmedsae","zmedssqerr","zmeansae","zmeanssqerr")
  
  #where there are no grading data, make 
  coregradmeans[is.na(rowSums(df1[c(2:len+1)]))] <- NA
  results <- list(cbind(df1[,c(1:len+1)],corrected_grades = coregradmeans))
  return(results)
}

#results = bam(df=df)
#results$sum_results


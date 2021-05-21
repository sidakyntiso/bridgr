
library(rstan)

#' Load grading data
bridgr.data <- function(df){
  #identify the number of graders
  df$ta <- as.character(unlist(df$ta))
  len <- length(unique(df$ta))

  #remove irrelevant columns
  ta <- unique(df$ta)
  df <- df[,c("ta",ta)]

  #missing observations
  df$m = rowSums(is.na(df)) == ncol(df)-1

  #bridging observations
  df$bridges = 0
  df$bridges[rowSums(!is.na(df))==ncol(df)]=1

  #identify traditional score
  core1ta<-ifelse(df$ta==unique(df$ta)[1], df[,unique(df$ta)[1]], NA)
  for (i in c(2:len)){
    core1ta<-ifelse(df$ta==unique(df$ta)[i], df[,unique(df$ta)[i]], core1ta)
  }
  df$corelta <- core1ta

  #identify traditional rank
  df$coreltapos <-rank(-core1ta, na.last="keep", ties.method= "average")

  #identify average of grades
  df$avg  <- rowMeans(df[,c(2:(len+1))],na.rm =T)

  #Get rank of averages
  df$corpos<-rank(df$avg, na.last="keep", ties.method= "average")
  return(df)
}

#' Evaluate grading bias
#'
#' \code{bridgr.eval.bias} assesses evidence of bias in grading data.
#' Specifically, this function uses a set of common or bridging exams to
#' evaluate whether graders systematically differ from each other.
#' The grading data should contain the following columns: (a)
#' one column labeled \code{ta} that lists all graders. Remaining
#' columns \code{grader1,grader2,..} should be labeled by values
#' corresponding to the levels of the \code{ta} column.
#' @param df A dataframe with grading data.
#' @param plot Whether to plot empirical CDF of grades by grader.
#' @param tbl Whether to plot tabulate evidence of bias in grading data.
#' @return \code{mae} Vector containing the mean absolute error of grades
#' and ranks for bridging exams.
#' @return \code{rmse} Vector containing the root-mean squared error of
#' grades and ranks for bridging exams.
#' @return \code{bounds} Minimum and maximum values for the MAE and RMSE of ranks.
#' @return \code{fstat} F-statistic and associated p-value for grades.
#' @examples \donttest{
#' sim.midterm = readRDS("simdata_five.rds")
#' grading.bias.pre = bridgr.eval.bias(df=sim.midterm)
#' }
#' @export
#'
bridgr.eval.bias <- function(df,plot = TRUE,tbl = TRUE){
  library(tidyverse)
  library(huxtable)
  df1 <- bridgr.data(df=df)
  #identify bridges
  g = nrow(df1[df1$bridges==1,])
  # create ecdf function for each TA
  len <- length(unique(df1$ta))

  # for each grader
  for (i in c(1:len)){
    x = ecdf(df1[[paste("ta",i,sep = "")]])
    # scores for each bridging student
    assign(paste("xpoints",i,sep = ""), df1[[paste("ta",i,sep = "")]][df1$bridges==1] )
    # associated point on the grader's ecdf curve
    assign(paste("xecdf",i,sep = ""), x(df1[[paste("ta",i,sep = "")]][df1$bridges==1]))
  }
  #list of scores for each bridging students
  dat.list <- lapply(c(1:len), function(i) get(paste("xpoints",i,sep = "")))
  #convert list to dataframe
  dat  <-  as.data.frame(matrix(unlist(dat.list), nrow=length(unlist(dat.list[1]))))
  colnames(dat) <- paste("ta",c(1:len),sep = "")
  #reshape dataframe long
  dat <- dat %>% gather(ta, points, colnames(dat))


  #same procedure for list of ranks
  rank.list <- lapply(c(1:len), function(i) get(paste("xecdf",i,sep = "")))
  rank.dat  <-  as.data.frame(matrix(unlist(rank.list), nrow=length(unlist(rank.list[1]))))
  colnames(rank.dat) <- paste("ta",c(1:len),sep = "")
  rank.dat <- rank.dat %>% gather( ta, e_cdf,colnames(rank.dat))
  #merge with dataframe
  dat$e_cdf <- rank.dat[,2]

  dat$student <- as.factor(c(1:g))
  dat$ta <- as.factor(dat$ta)

  #get ranks
  tas = (unique(dat$ta))
  ta_ranks <- lapply(tas, function(i) rank(-dat$points[dat$ta==i]))
  ta_ranks_dat <- as.data.frame(matrix(unlist(ta_ranks), nrow=length(unlist(ta_ranks[1]))))
  dat$ta_ranks <- NA
  #assign ranks to dataset by ta
  for (i in c(1:len)){
    ta = as.character(tas[[i]])
    dat$ta_ranks[dat$ta==ta] = as.numeric(unlist(ta_ranks[i]))
  }



  #function ensures zeros are have two decimal points
  roundr <- function(t){
    t = round(t,2)
    if(match(TRUE, round(t, 1:20) == t)<=2){ #check num decimals after decimal point
      t = format(t,nsmall=2)
    }
    return(t)
  }

  #summary statistics: grades
  mae_grades = roundr(mean(abs(df1$corelta[df1$bridges==1] -
                                 df1$avg[df1$bridges==1])))
  rmse_grades = roundr(mean((df1$corelta[df1$bridges==1] -
                               df1$avg[df1$bridges==1])^2)^0.5)
  fval = roundr((anova(lm(points ~ ta, dat)))[[4]][1])
  pvalF = (anova(lm(points ~ ta, dat)))[[5]][1]

  #summary statistics: ranks
  coreltapos_bridges = rank(-df1$corelta[df1$bridges==1],
                            na.last="keep", ties.method= "average")
  corpos_bridges = rank(-df1$avg[df1$bridges==1], na.last="keep",
                        ties.method= "average")

  mae_ranks = roundr(mean(abs(coreltapos_bridges-corpos_bridges)))
  mae_ranks_upper = mean(abs(c(1:g)-rev(c(1:g))))
  rmse_ranks = roundr(mean((coreltapos_bridges - corpos_bridges)^2)^0.5)
  rmse_ranks_upper = roundr((mean((c(1:g) - rev(c(1:g)))^2))^0.5)

  #Figure with grades by grader
  if(plot==T){
    cat("\n")
    p <- ggplot() +
      stat_ecdf(data =subset(df1,!is.na(corelta)), aes(corelta,colour=ta),
                geom = "step",pad = F,lty=2)+theme_bw() +
      geom_point(aes(x=points,y=e_cdf,shape=student,colour=ta),dat,size=3)+
      scale_fill_grey()+xlab("Grade")+ylab("CDF")
    print(p)
  }
  if(tbl==T){
    #Table of Grading Bias for Bridging Students
    cat("\n")
    desc_dat <- data.frame(
      labels = c("Grades","Ranks "),
      maes = c(mae_grades,mae_ranks),
      rmse = c(rmse_grades,rmse_ranks),
      fstat = c(fval,""),
      pval = c(round(pvalF,3),""))
    colnames(desc_dat) = rep("",ncol(desc_dat))
    desc_tbl = desc_dat %>%
      as_hux(add_colnames = FALSE) %>%
      insert_row("", "MAE", "RMSE","F-statistic", "Pr(>F)", after = 0) %>%
      set_align(2, everywhere, "left") %>%
      theme_article() %>%
      set_position("left") %>%
      style_headers(bold = TRUE, text_color = "black") %>%
      set_width(1.5) %>%
      set_bottom_border(final(1), everywhere) %>%
      set_position("left") %>%
      set_header_rows(1, TRUE) %>%
      style_headers(bold = TRUE, text_color = "black") %>%
      set_all_padding(0) %>%
      set_outer_padding(0) %>%
      set_caption("Grading Bias for Bridging Students") %>%
      add_footnote(paste0(
        paste0("Bounds MAE Rank: [0,",mae_ranks_upper,"]",sep=""),"\n",
        paste0("Bounds RMSE Ranks: [0,",rmse_ranks_upper,"]",sep=""),"\n",
        paste0("__________________",sep=""),"\n",
        paste0("Grading Parameters",sep=""),"\n",
        paste0("# Exams: ",nrow(df1),sep=""),";\t",
        paste0("# Graders: ",len,sep=""),"\n",
        paste0("# Bridges: ",sum(df1$bridges ),sep=""),";\t",
        paste0("# Missing: ",sum(df1$m),sep=""),"\n",
        sep=""))
    print(desc_tbl)
  }
  return(invisible(
    list(mae.grades = as.numeric(mae_grades),mae.ranks = as.numeric(mae_ranks),
         rmse.grades =as.numeric(rmse_grades),rmse.ranks = as.numeric(rmse_ranks),
         mae.bounds = as.numeric(mae_ranks_upper),
         rmse.bounds = as.numeric(rmse_ranks_upper),
         fstat = as.numeric(fval),fstat.pval = round(pvalF,3))))
}


#' Implement bridging correction
#'
#' Implements a Bayesian version of the Aldrich Mckelvey model to
#' grade data.
#' The grading data should contain the following columns: (a)
#' one column labeled \code{ta} that lists all graders. Remaining
#' columns \code{grader1,grader2,..} should be labeled by values
#' corresponding to the levels of the \code{ta} column.
#' @param df A dataframe with grading data.
#' @param min_grade The minimum possible grade (by default is zero.)
#' @param max_grade The maximum possible grade (by default is the
#' maximum observed grade.)
#' @return \code{df1} A version of the provided grade data.
#' @return \code{coregradmeds} The post-processed (bridged) student
#' grades on the scale of the original grade data.
#' @return \code{zmedsrank} The post-processed (bridged) student ranks
#' @examples \donttest{
#' sim.midterm = readRDS("simdata_five.rds")
#' grading.bias.pre = bridgr.eval.bias(df=sim.midterm)
#' }
#' @export
bridgr <- function(df,min_grade = NA, max_grade = NA){
  library(tidyverse)
  library(rstan)
  df1 <- bridgr.data(df=df)

  if (!"rstan" %in% rownames(installed.packages())){
    install.packages("rstan")
  }
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  #number of graders
  len <- length(unique(df1$ta))
  #identify bridges
  g = sum(df1$bridges )
  # create ecdf function for each TA
  len <- length(unique(df1$ta))

  if(is.na(min_grade)){
    min_grade = 0
  }
  if(is.na(max_grade)){
    max_grade = ceiling(max(df1[,paste("ta",1:len,sep = "")],na.rm = T))
  }

  #missing observations
  m = sum(df1$m)

  #stan settings
  ITER <- 20000
  WARMUP <- 1000
  THIN <- 20
  CHAINS<-8
  num<-length(df1$core1ta)
  colta<-df1$core1ta
  rowta<-df1$ta

  try({
    set.seed(12102019)

    matal<-cbind(df1[,c(2:(len+1))])
    mataldf<-as.data.frame(matal)
    mataldf <- round(mataldf,0) #Make all grades integers
    mataldf[is.na(mataldf)] <- -1
    wd <- mataldf
    N <- nrow(wd)
    J <- ncol(wd)
    data=list(N=N, J=J, wdata=wd,
              k= min_grade,K=max_grade)
    posterior.sim <-rstan::stan(file = 'bridgegapaml3.stan', data = data,
                                iter=ITER, thin=THIN, chains=CHAINS,init_r=.1,
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
    zmedsrank<-rank(zmeds, na.last="keep", ties.method= "average")

    #where there are no grading data, make
    coregradmeds[df1$m==TRUE] <- NA

    return(invisible(list(df1,coregradmeds,zmedsrank)))
  })


}

#' Evaluate improvements from bridging process
#'
#' \code{bridgr.eval.post} evaluates improvements to grading bias from
#' the bridging process.
#' @param bridgr.results The output from \code{bridgr::bridgr}.
#' @param plot Whether to plot empirical CDF of post-processed (bridged)
#' grades.
#' @param tbl Whether to tabulate evidence of bias in post-processed grades.
#' @return \code{mae} Vector containing the values of the pre-processed
#' and post-processed mean absolute errors for the grades and ranks
#' @return \code{rmse} Vector containing the values of the pre-processed
#' and post-processed root mean-squared errors for the grades and ranks
#' @examples \donttest{
#' bridgr.results = bridgr(df = sim.midterm)
#' grading.bias.post = bridgr.eval.post(bridgr.results = results)
#' }
#' @export
bridgr.eval.post <- function(bridgr.results,plot = T,tbl=T){
  library(tidyverse)
  library(huxtable)

  results <- data.frame(cbind(bridgr.results[[1]],
                              corrected_grades = bridgr.results[[2]],
                              corrected_ranks = bridgr.results[[3]]))

  g = nrow(results[results$bridges==1,])
  e = ecdf(results$corrected_grades)


  #Figure with Average and Bridged Grades
  if(plot==T){
    meanpoints =
      meanpoints = results$avg[results$bridges==1]
    xpoints = results$corrected_grades[results$bridges==1]
    xecdf = as.numeric(unlist(lapply(c(1:g), function(i)
      e(results$corrected_grades[results$bridges==1][i]))))

    dat <- data.frame(student = as.factor(rep(c(1:g),2)),
                      points = c(xpoints,meanpoints),
                      Model = c(rep("Bridged",g),rep("Average",g)),
                      e_cdf=xecdf)
    p <- ggplot() +
      stat_ecdf(aes(results$corrected_grades),geom = "step",pad = F)+
      geom_point(aes(x=points,y=e_cdf,shape=student,colour=Model),
                 dat,size=3)+
      scale_color_grey(start=0.6, end=0.2)+
      scale_fill_grey()+xlab("Grade")+ylab("CDF")+theme_bw()
    print(p)
  }

  #Table Summarizing Improvements
  #function ensures zeros are have two decimal points
  roundr <- function(t){
    t = round(t,2)
    if(match(TRUE, round(t, 1:20) == t)<=2){  #check num decimals after decimal point
      t = format(t,nsmall=2)
    }
    return(t)
  }

  #summary statistics: grades
  mae_grades =round(mean(abs(results$corrected_grades[results$bridges==1]
                             -results$avg[results$bridges==1])),2)
  rmse_grades = round(mean((results$corrected_grades[results$bridges==1] -
                              results$avg[results$bridges==1])^2)^0.5,2)
  #summary statistics: ranks
  coreltapos_bridges = rank(-results$corrected_grades[results$bridges==1],
                            na.last="keep", ties.method= "average")
  corpos_bridges = rank(-results$avg[results$bridges==1],
                        na.last="keep", ties.method= "average")
  mae_ranks = roundr(mean(abs(coreltapos_bridges-corpos_bridges)))
  mae_ranks_upper = mean(abs(c(1:g)-rev(c(1:g))))
  rmse_ranks = roundr(mean((coreltapos_bridges - corpos_bridges)^2)^0.5)
  rmse_ranks_upper = roundr((mean((c(1:g) - rev(c(1:g)))^2))^0.5)

  if(tbl==T){
    #Initiate Table
    cat("\n")
    desc_dat <- data.frame(
      labels = c("Grades","Ranks "),
      maes = c(mae_grades,mae_ranks),
      rmse = c(rmse_grades,rmse_ranks))

    #Add unbridged summary stats
    coreltapos_bridges_old = rank(-results$corelta[results$bridges==1],
                                  na.last="keep", ties.method= "average")
    corpos_bridges_old = rank(-results$avg[results$bridges==1], na.last="keep",
                              ties.method= "average")
    mae_grades_old = roundr(mean(abs(results$corelta[results$bridges==1] -
                                       results$avg[results$bridges==1])))
    rmse_grades_old = roundr(mean((results$corelta[results$bridges==1] -
                                     results$avg[results$bridges==1])^2)^0.5)
    mae_ranks_old = roundr(mean(abs(coreltapos_bridges_old-corpos_bridges_old)))
    rmse_ranks_old = roundr(mean((coreltapos_bridges_old - corpos_bridges_old)^2)^0.5)

    x <- data.frame(
      labels = c("Grades","Ranks "),
      maes = c(mae_grades_old,mae_ranks_old),
      rmse = c(rmse_grades_old,rmse_ranks_old))

    #cbind with unbridged values
    desc_dat =cbind(x[,c(1:3)],desc_dat[,c(2:3)])
    colnames(desc_dat) = rep("",ncol(desc_dat))

    #Final Table
    desc_tbl= desc_dat %>%
      as_hux() %>%
      set_align(2, everywhere, "left") %>%
      set_contents(1, 2:5, c("MAE", "RMSE","MAE", "RMSE")) %>%
      insert_row("", "Naive", "","Bridged", "", after = 0) %>%
      merge_cells(1, 2:3) %>%
      merge_cells(1, 4:5) %>%
      set_align(1, everywhere, "center") %>%
      set_tb_padding(1, everywhere, 0) %>%
      set_bold(1, everywhere) %>%
      theme_article() %>%
      set_caption("Reductions in Grading Bias for Bridging Students") %>%
      set_position("left") %>%
      style_headers(bold = TRUE, text_color = "black") %>%
      set_width(1.5) %>%
      set_bottom_border(final(1), everywhere) %>%
      set_position("left") %>%
      set_header_rows(1, TRUE) %>%
      style_headers(bold = TRUE, text_color = "black") %>%
      set_all_padding(0) %>%
      set_outer_padding(0)
    print(desc_tbl)
    return(invisible(
      list(mae.grades.post = as.numeric(mae_grades),mae.ranks.post = as.numeric(mae_ranks),
           rmse.grades.post =as.numeric(rmse_grades),rmse.ranks.post = as.numeric(rmse_ranks),
           mae.grades.pre = as.numeric(mae_grades_old),mae.ranks.pre = as.numeric(mae_ranks_old),
           rmse.grades.pre =as.numeric(rmse_grades_old),rmse.ranks.pre = as.numeric(rmse_ranks_old)
      )))

  }
}

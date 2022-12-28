#' Load grading data
#'
#' \code{bridgr.data} loads grading data.
#'
#' @param df An object of class \code{data.frame}. Must contain a row for each
#' grade or evaluation, as well as columns indicating student and grader identifiers.
#' @param grade The grade to be scaled (numeric or integer)
#' @param student Identifier for the student, exam or stimuli associated with `grade` field.
#' @param grader Identifier for the grader or section associated with the `grade` field.
#' @param grader.assigned Identifier for the student's assigned grader or section
#' @return \code{grade.data} Processed version of the provided grade data.
#'
#' @examples
#' data("bridgr.sim.data")
#' # Re-structure the input grading dataset.
#' bridgr.dat <- bridgr.data(df=bridgr.sim.data,student="student",
#' grader.assigned = "grader.assigned", grader="grader",grade="grade")
#'
#' @export

bridgr.data <- function(df,student,grader.assigned,grader,grade){
  grade.data = df
  if(!class(grade.data$grade) %in% c("integer","numeric")){
    message("Error in bridgr.data : object 'grade' not numeric or integer")
  }

  #identify the number of graders
  grade.data$ta <- as.character(unlist(grade.data$grader))
  ta <- unique(grade.data$ta)
  len <- length(ta)

  #reshape wide
  for (i in ta){
    grade.data[grade.data$ta==i,i] = grade.data$grade[grade.data$ta==i]
  }
  for (i in ta){
    stdnt = grade.data$student
    grd = grade.data[,i]
    grd[is.na(grd)]=-1
    grade_fill = as.numeric(unlist(
      lapply(unique(stdnt),function(i) max(as.numeric(grd[stdnt==i]),na.rm = T))
    ))
    grade_fill[grade_fill %in% c(-1)]=NA
    grade.data[,i] = as.numeric(grade_fill[match(grade.data$student,unique(stdnt))])
  }
  #remove irrelevant columns
  grade.data <- grade.data[,c(student,grader.assigned,ta)]
  grade.data <- grade.data[!duplicated(grade.data),]

  #identify average of grades
  grade.data$avg  <- rowMeans(grade.data[,ta],na.rm =TRUE)

  #bridging observations
  grade.data$bridges =as.numeric(rowSums(!is.na(grade.data[,ta]))==len)

  #identify bridges
  g = nrow(grade.data[grade.data$bridges==1,])

  #missing observations
  grade.data$m = as.numeric((rowSums(is.na(grade.data[,ta])))==g)

  #identify traditional score
  core1ta<-ifelse(grade.data$grader.assigned==ta[1], grade.data[,ta[1]], NA)
  for (i in c(2:len)){
    core1ta<-ifelse(grade.data$grader.assigned==ta[i], grade.data[,ta[i]], core1ta)
  }
  grade.data$corelta <- core1ta

  #identify traditional rank
  grade.data$coreltapos <-rank(-as.numeric(core1ta), na.last="keep", ties.method= "average")

  #Get rank of averages
  grade.data$corpos<-rank(grade.data$avg, na.last="keep", ties.method= "average")
  return(grade.data)
}

#' Evaluate grading bias
#'
#' \code{bridgr.eval.bias} assesses evidence of bias in grading data.
#' Specifically, this function uses a set of common or bridging exams to
#' evaluate whether graders systematically differ from each other.
#'
#' @param bridgr.dat A \code{bridgr.data} object.
#' @param plot Whether to plot empirical CDF of grades by grader.
#' @param tbl Whether to plot tabulate evidence of bias in grading data.
#' @return \code{mae} Vector containing the mean absolute error of grades
#' and ranks for bridging exams.
#' @return \code{rmse} Vector containing the root-mean squared error of
#' grades and ranks for bridging exams.
#' @return \code{bounds} Minimum and maximum values for the MAE and RMSE of ranks.
#' @return \code{fstat} F-statistic and associated p-value for grades.
#'
#' @examples
#' data("bridgr.sim.data")
#' # Re-structure the input grading dataset.
#' bridgr.dat <- bridgr.data(df=bridgr.sim.data,student="student",
#' grader.assigned = "grader.assigned", grader="grader",grade="grade")
#'
#' # Evaluate grading bias using bridging observations
#' bridgr.eval.bias(bridgr.dat=bridgr.dat,plot=FALSE,tbl=TRUE)
#'
#' @export

bridgr.eval.bias <- function(bridgr.dat,plot = TRUE,tbl = TRUE){
  #define globals
  `%>%` = dplyr::`%>%`
  student = corelta = xecdf = NULL
  if(is.null(bridgr.dat$bridges)){
    message("Error in bridgr.eval.bias: Please input a `bridgr.data` object.")
  }
  #identify bridges
  g = nrow(bridgr.dat[bridgr.dat$bridges==1,])
  # create ecdf function for each TA
  bridgr.dat$ta = bridgr.dat$grader.assigned
  ta <- unique(bridgr.dat$ta)
  len <- length(ta)

  # for each grader
  for (i in c(1:len)){
    x = ecdf(bridgr.dat[[paste("ta",i,sep = "")]])
    # scores for each bridging student
    assign(paste("xpoints",i,sep = ""), bridgr.dat[[paste("ta",i,sep = "")]][bridgr.dat$bridges==1] )
    # associated point on the grader's ecdf curve
    assign(paste("xecdf",i,sep = ""), x(bridgr.dat[[paste("ta",i,sep = "")]][bridgr.dat$bridges==1]))
  }
  #list of scores for each bridging students
  dat.list <- lapply(c(1:len), function(i) get(paste("xpoints",i,sep = "")))
  #convert list to dataframe
  dat  <-  as.data.frame(matrix(unlist(dat.list), nrow=length(unlist(dat.list[1]))))
  colnames(dat) <- paste("ta",c(1:len),sep = "")
  #reshape dataframe long
  #dat1 <- dat %>% gather(ta, points, colnames(dat))
  dat =data.frame(ta = unlist(lapply(ta,function(i) rep(i,g))),
                  points=unlist(lapply(ta, function(i) as.numeric(dat[,i]))))

  #same procedure for list of ranks
  rank.list <- lapply(c(1:len), function(i) get(paste("xecdf",i,sep = "")))
  rank.dat  <-  as.data.frame(matrix(unlist(rank.list), nrow=length(unlist(rank.list[1]))))
  colnames(rank.dat) <- paste("ta",c(1:len),sep = "")
  # rank.dat <- rank.dat %>% gather( ta, e_cdf,colnames(rank.dat))
  rank.dat =data.frame(ta = unlist(lapply(ta,function(i) rep(i,g))),
                       points=unlist(lapply(ta, function(i) as.numeric(rank.dat[,i]))))
  #merge with dataframe
  dat$xecdf <- rank.dat[,2]

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



  #function ensures zeros up to two decimal points
  roundr <- function(t){
    t = round(t,2)
    if(match(TRUE, round(t, 1:20) == t)<=2){ #check num decimals after decimal point
      t = format(t,nsmall=2)
    }
    return(t)
  }

  #summary statistics: grades
  mae_grades = roundr(mean(abs(bridgr.dat$corelta[bridgr.dat$bridges==1] -
                                 bridgr.dat$avg[bridgr.dat$bridges==1])))
  rmse_grades = roundr(mean((bridgr.dat$corelta[bridgr.dat$bridges==1] -
                               bridgr.dat$avg[bridgr.dat$bridges==1])^2)^0.5)
  fval = roundr((anova(lm(points ~ ta, dat)))[[4]][1])
  pvalF = (anova(lm(points ~ ta, dat)))[[5]][1]

  #summary statistics: ranks
  coreltapos_bridges = rank(-bridgr.dat$corelta[bridgr.dat$bridges==1],
                            na.last="keep", ties.method= "average")
  corpos_bridges = rank(-bridgr.dat$avg[bridgr.dat$bridges==1], na.last="keep",
                        ties.method= "average")

  mae_ranks = roundr(mean(abs(coreltapos_bridges-corpos_bridges)))
  mae_ranks_upper = mean(abs(c(1:g)-rev(c(1:g))))
  rmse_ranks = roundr(mean((coreltapos_bridges - corpos_bridges)^2)^0.5)
  rmse_ranks_upper = roundr((mean((c(1:g) - rev(c(1:g)))^2))^0.5)

  #Figure with grades by grader
  if(plot==TRUE){
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      cat("\n")
      p <- ggplot2::ggplot() +
        ggplot2::stat_ecdf(data =subset(bridgr.dat,!is.na(corelta)), ggplot2::aes(corelta,colour=ta),
                           geom = "step",pad = F,lty=2)+
        ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=points,y=xecdf,shape=student,colour=ta),dat,size=3)+
        ggplot2::scale_fill_grey()+ggplot2::xlab("Grade")+ggplot2::ylab("CDF")
      print(p)
    } else {
      message("Error in requireNamespace(ggplot2) : object 'ggplot2' not found")
    }
  }


  if(tbl==TRUE){
    if (requireNamespace("huxtable", quietly = TRUE)&
        requireNamespace("dplyr", quietly = TRUE)) {
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
        huxtable::as_hux(add_colnames = FALSE) %>%
        huxtable::insert_row("", "MAE", "RMSE","F-statistic", "Pr(>F)", after = 0) %>%
        huxtable::set_align(2, huxtable::everywhere, "left") %>%
        huxtable::theme_article() %>%
        huxtable::set_position("left") %>%
        huxtable::style_headers(bold = TRUE, text_color = "black") %>%
        huxtable::set_width(1.5) %>%
        huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere) %>%
        huxtable::set_position("left") %>%
        huxtable::set_header_rows(1, TRUE) %>%
        huxtable::style_headers(bold = TRUE, text_color = "black") %>%
        huxtable::set_all_padding(0) %>%
        huxtable::set_outer_padding(0) %>%
        huxtable::set_caption("Grading Bias for Bridging Students") %>%
        huxtable::add_footnote(paste0(
          paste0("Bounds MAE Rank: [0,",mae_ranks_upper,"]",sep=""),"\n",
          paste0("Bounds RMSE Ranks: [0,",rmse_ranks_upper,"]",sep=""),"\n",
          paste0("__________________",sep=""),"\n",
          paste0("Grading Parameters",sep=""),"\n",
          paste0("# Exams: ",nrow(bridgr.dat),sep=""),";\t",
          paste0("# Graders: ",len,sep=""),"\n",
          paste0("# Bridges: ",sum(bridgr.dat$bridges ),sep=""),";\t",
          paste0("# Missing: ",sum(bridgr.dat$m),sep=""),"\n",
          sep=""))
      print(desc_tbl)
    } else {
      message("Error in requireNamespace(huxtable) : object 'huxtable' not found")
    }
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
#' @param bridgr.dat A \code{bridgr.data} object.
#' @param min_grade The minimum possible grade (by default is zero.)
#' @param max_grade The maximum possible grade (by default is the
#' maximum observed grade.)
#' @param stan_model Rstan model (see manuscript for default implementation.)
#' @param ITER RSTAN parameter. A positive integer specifying the number of iterations for
#' each chain (including warmup). The default is 20000.
#' @param WARMUP = Rstan parameter. A positive integer specifying the number of warmup (aka burnin) iterations
#' per chain. The default is 1000.
#' @param CHAINS  Rstan parameter. A positive integer specifying the number of Markov chains.
#' The default is 4.
#' @param CORES Rstan parameter. The number of cores to use when executing the Markov chains
#' in parallel. The default is to use the value of the "mc.cores"
#' @param THIN = A positive integer specifying the period for saving samples. The default is 1,
#'
#' @return \code{bridgr.dat} Processed version of the input dataset.
#' @return \code{coregradmeds} The post-processed (bridged) student
#' grades on the scale of the input dataset.
#' @return \code{zmedsrank} The post-processed (bridged) student ranks.
#' @return \code{student_id} Student identifier that can be linked with the input dataset.
#'
#' @examples
#' data("bridgr.sim.data")
#' # Re-structure the input grading dataset.
#' bridgr.dat <- bridgr.data(df=bridgr.sim.data,student="student",
#' grader.assigned = "grader.assigned", grader="grader",grade="grade")
#' \donttest{
#' # Correct grading bias using bridging observations. Set cores = NA to utilize more CPU cores.
#' bridgr.sim.results = bridgr(bridgr.dat=bridgr.dat,min_grade=NA,max_grade=NA, CORES = 2)
#' }
#' @export

bridgr <- function(bridgr.dat,min_grade = NA, max_grade = NA,stan_model = NA,
                   ITER=NA,WARMUP=NA,THIN=NA,CHAINS=NA,CORES=NA){
  if(is.null(bridgr.dat$bridges)){
    message("Error in bridgr: Please input a `bridgr.data` object.")
  }
  #stan settings
  #restore options before exiting the function even if the function breaks
  old <- options()
  on.exit(options(old))

  if (requireNamespace("rstan", quietly = TRUE)) {
    rstan::rstan_options(auto_write = TRUE)
    if (is.na(CORES)){
      options(mc.cores = parallel::detectCores())
    } else {
      old<-options(mc.cores = CORES)
    }

  } else {
    message("Error in requireNamespace(rstan) : object 'rstan' not found")
  }

  if (is.na(stan_model)){
    stan_model = paste(system.file(package = "bridgr"),"/bridgegapaml3.stan",sep="")
  }
  if(is.na(ITER)){
    ITER <- 20000
  }
  if(is.na(WARMUP)){
    WARMUP <- 1000
  }
  if(is.na(THIN)){
    THIN <- 1
  }
  if(is.na(CHAINS)){
    CHAINS<-8
  }
  #number of graders
  bridgr.dat$ta = bridgr.dat$grader.assigned
  ta <- unique(bridgr.dat$ta)
  len <- length(unique(bridgr.dat$ta))
  #identify bridges
  g = sum(bridgr.dat$bridges )



  if(is.na(min_grade)){
    min_grade = 0
  }
  if(is.na(max_grade)){
    max_grade = ceiling(max(bridgr.dat[,paste("ta",1:len,sep = "")],na.rm = T))
  }

  #missing observations
  m = sum(bridgr.dat$m)
  num<-length(bridgr.dat$core1ta)
  colta<-bridgr.dat$core1ta
  rowta<-bridgr.dat$ta
  if (requireNamespace("rstan", quietly = TRUE)) {
    try({
      matal<-cbind(bridgr.dat[,ta])
      mataldf<-as.data.frame(matal)
      mataldf <- round(mataldf,0) #Make all grades integers
      mataldf[is.na(mataldf)] <- -1
      wd <- mataldf
      N <- nrow(wd)
      J <- ncol(wd)
      data=list(N=N, J=J, wdata=wd,
                k= min_grade,K=max_grade)
      posterior.sim <-rstan::stan(file = stan_model, data = data,iter=ITER,
                                  thin=THIN, chains=CHAINS,init_r=.1,warmup = WARMUP,
                                  control = list(adapt_delta = 0.9,max_treedepth = 12))

      #extract alphas and get grader medians
      alpha.dat<- data.frame(rstan::extract(posterior.sim, pars="alpha", inc_warmup=F, permuted=F))

      #extract betas and get grader medians
      beta.dat<- data.frame(rstan::extract(posterior.sim, pars="beta", inc_warmup=F, permuted=F))

      #real scores
      z<- rstan::extract(posterior.sim, pars="Z", inc_warmup=F, permuted=F)
      rm(posterior.sim)

      #extract medians
      zmeds<-rstan::monitor(z, inc_warmup=F, print=F)[,6]
      #ranks of meds
      zmedsrank<-rank(zmeds, na.last="keep", ties.method= "average")
      #actual grades
      coregradmeds <- median(unlist(alpha.dat)) + zmeds*median(unlist(beta.dat))
      names(coregradmeds)=bridgr.dat$student
      zmedsrank<-rank(zmeds, na.last="keep", ties.method= "average")
      names(zmedsrank)=bridgr.dat$student

      #where there are no grading data, make NA
      coregradmeds[bridgr.dat$m==TRUE] <- NA
      zmedsrank[bridgr.dat$m==TRUE] <- NA

      bridgr.results = list(data = bridgr.dat,student_id =bridgr.dat$student,
                            corrected_grades = coregradmeds,corrected_ranks = zmedsrank)
      return(invisible(bridgr.results))
    })
  } else {
    message("Error in requireNamespace(rstan) : object 'rstan' not found")
  }



}

#' Evaluate improvements from bridging process
#'
#' \code{bridgr.eval.post} evaluates improvements to grading bias from
#' the bridging process.
#' @param bridgr.results A \code{bridgr} object.
#' @param plot Whether to plot empirical CDF of post-processed (bridged)
#' grades.
#' @param tbl Whether to tabulate evidence of bias in post-processed grades.
#'
#' @return \code{mae} Vector containing the values of the pre-processed
#' and post-processed mean absolute errors for the grades and ranks
#' @return \code{rmse} Vector containing the values of the pre-processed
#' and post-processed root mean-squared errors for the grades and ranks
#'
#' @examples
#' # Load pre-processed results
#' data("bridgr.sim.results")
#' # Evaluate improvements among commonly graded students
#' grading.bias.post = bridgr.eval.post(bridgr.results = bridgr.sim.results,plot = TRUE, tbl = FALSE)
#' @export

bridgr.eval.post <- function(bridgr.results,plot = TRUE,tbl=TRUE){
  #define globals
  `%>%` = dplyr::`%>%`
  student = Model = xecdf = NULL
  corrected_grades = NA
  results <- data.frame(cbind(bridgr.results$data,
                              corrected_grades = bridgr.results$corrected_grades,
                              corrected_ranks = bridgr.results$corrected_ranks))

  g = nrow(results[results$bridges==1,])
  e = ecdf(results$corrected_grades)


  #Figure with Average and Bridged Grades
  if(plot==TRUE){
    meanpoints = meanpoints = results$avg[results$bridges==1]
    xpoints = results$corrected_grades[results$bridges==1]
    xecdf = as.numeric(unlist(lapply(c(1:g), function(i)
      e(results$corrected_grades[results$bridges==1][i]))))

    dat <- data.frame(student = as.factor(rep(c(1:g),2)),
                      points = c(xpoints,meanpoints),
                      Model = c(rep("Bridged",g),rep("Average",g)),
                      xecdf=xecdf)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot2::ggplot(data = results,mapping = ggplot2::aes(x=corrected_grades))+
        ggplot2::stat_ecdf(inherit.aes =TRUE,geom = "step",pad = F)+
        ggplot2::geom_point(ggplot2::aes(x=points,y=xecdf,shape=student,colour=Model),
                            dat,size=3)+
        ggplot2::scale_color_grey(start=0.6, end=0.2)+
        ggplot2::scale_fill_grey()+ggplot2::xlab("Grade")+ggplot2::ylab("CDF")+ggplot2::theme_bw()
      print(p)
    } else {
      message("Error in requireNamespace(ggplot2) : object 'ggplot2' not found")
    }
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

  if(tbl==TRUE){
    if (requireNamespace("huxtable", quietly = TRUE)&
        requireNamespace("dplyr", quietly = TRUE)) {
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
        huxtable::as_hux() %>%
        huxtable::set_align(2, huxtable::everywhere, "left") %>%
        huxtable::set_contents(1, 2:5, c("MAE", "RMSE","MAE", "RMSE")) %>%
        huxtable::insert_row("", "Naive", "","Bridged", "", after = 0) %>%
        huxtable::merge_cells(1, 2:3) %>%
        huxtable::merge_cells(1, 4:5) %>%
        huxtable::set_align(1, huxtable::everywhere, "center") %>%
        huxtable::set_tb_padding(1, huxtable::everywhere, 0) %>%
        huxtable::set_bold(1, huxtable::everywhere) %>%
        huxtable::theme_article() %>%
        huxtable::set_caption("Reductions in Grading Bias for Bridging Students") %>%
        huxtable::set_position("left") %>%
        huxtable::style_headers(bold = TRUE, text_color = "black") %>%
        huxtable::set_width(1.5) %>%
        huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere) %>%
        huxtable::set_position("left") %>%
        huxtable::set_header_rows(1, TRUE) %>%
        huxtable::style_headers(bold = TRUE, text_color = "black") %>%
        huxtable::set_all_padding(0) %>%
        huxtable::set_outer_padding(0)
      print(desc_tbl)
    } else {
      message("Error in requireNamespace(huxtable) : object 'huxtable' not found")
    }
    return(invisible(
      list(mae.grades.post = as.numeric(mae_grades),mae.ranks.post = as.numeric(mae_ranks),
           rmse.grades.post =as.numeric(rmse_grades),rmse.ranks.post = as.numeric(rmse_ranks),
           mae.grades.pre = as.numeric(mae_grades_old),mae.ranks.pre = as.numeric(mae_ranks_old),
           rmse.grades.pre =as.numeric(rmse_grades_old),rmse.ranks.pre = as.numeric(rmse_ranks_old)
      )))

  }
}

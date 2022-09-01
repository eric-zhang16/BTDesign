
#' Title robability to stop for futility at IA
#'
#' @param r1 stopping response bar at stage-1
#' @param n1 sample size stage-1
#' @param p true response rate
#'
#' @return PET, probability to stop at stage-1
#' @export
#'
#' @examples
#'
PET <- function(r1,n1,p){
  pet <- pbinom(r1, size=n1, prob=p)
  return(pet)
}


#' Title Prob for stop futility at end of the trial
#'
#' @param r1 stopping response bar at stage-1
#' @param n1 sample size stage-1
#' @param p true response rate
#' @param r stopping response bar at stage-2
#' @param n sample size stage-2
#'
#' @return
#' @export
#'
#' @examples
ProbRej <- function(r1,n1,p, r,n){

  cp<-0

  for(j in (r1+1):min(n1, r) ){
    cp <- cp + dbinom(j, size=n1, prob=p)*pbinom(r-j, size=n-n1, prob=p)
  }


  prej <- PET(r1,n1,p) + cp
  return(prej)
}


#' Title Expected N
#'
#' @param r1 stopping response bar at stage-1
#' @param n1 sample size stage-1
#' @param p true response rate
#' @param n sample size stage-2
#'
#' @return
#' @export
#'
#' @examples
EN <- function(r1,n1,p,n){
  en <- n1 + (1-PET(r1,n1,p))*(n-n1)
  return(en)
}



#' Title Function to find the optimal bar at stage-1 for Simon's 2-stage design
#'
#' @param n.s2stg total sample size at the end of stage-2
#' @param n1.s2stg sample size at the end of stage-1
#' @param p0.s2stg response rate under Null
#' @param p1.s2stg response rate under alternative
#' @param r1.s2stg a vector of possible response bar at stage-1 to be search for the optimal bar
#' @param r.s2stg  upper bound in search for the stopping bar at end of stage. e.g.,r.s2stg=n.s2stg. A smaller value would save computation time
#' @param alpha FWER
#'
#' @return A list with the first element for the picked design and the second element for a data frame with all searched designs with design aspects including sample size across stages, stopping bar across stages, type 1 error, power and stopping prob. at stage-1
#' @export
#'
#' @examples
#' n.s2stg <- 40
#' n1.s2stg  <- 20
#' p0.s2stg  <- 0.2
#' p1.s2stg  <- 0.4
#' r1.s2stg <- seq(1,19,1)
#' r.s2stg  <- 40
#' sum.2stage <- Simon2stg_design(n.s2stg,n1.s2stg,p0.s2stg ,p1.s2stg,r1.s2stg,r.s2stg, alpha=1-(1-0.1)^(1/4) )
#' sum.2stage[[1]]


Simon2stg_design <- function(n.s2stg,n1.s2stg,p0.s2stg ,p1.s2stg,r1.s2stg,r.s2stg ,alpha){

  sum.2stage <- data.frame(n=NA,n1=NA,r1=NA,r=NA,Type1=NA, Power=NA, EN=NA, PET=NA )

  for(i in 1:length(r1.s2stg)){
    r1.tmp <- r1.s2stg [i]
    for(j in (r1.tmp+1):r.s2stg){
      # Type 1 error
      typ1 <- 1-ProbRej(r1.tmp,n1.s2stg,p=p0.s2stg, j,n.s2stg)

      # Power
      power <- 1-ProbRej(r1.tmp,n1.s2stg,p=p1.s2stg, j,n.s2stg)

      # Early stopping for futility
      early.stop <- PET(r1.tmp,n1.s2stg,p=p0.s2stg)

      # EN

      en <- EN(r1.tmp,n1.s2stg,p=p0.s2stg,n.s2stg)

      sum.2stage.tmp <- data.frame(n=n.s2stg,n1=n1.s2stg,r1=r1.tmp,r=j,Type1=round(typ1,6), Power=round(power,6), EN=round(en,6), PET=round(early.stop,6) )
      sum.2stage <- rbind(sum.2stage,sum.2stage.tmp)
    }
  }
  sum.2stage <- sum.2stage[-1,]
  sum.2stage.val <- subset(sum.2stage,Type1<alpha)
  sum.2stage.pick <- sum.2stage.val[sum.2stage.val$Power==max(sum.2stage.val$Power),]

  res <- list()
  res[[1]] <- sum.2stage.pick
  res[[2]] <- sum.2stage
  names(res) <- c('Picked',"Designs")
  return(res)

}


#' Title Function to implement Simon's 2-stage
#'
#' @param n1.s2stg sample size stage-1
#' @param n.s2stg  sample size stage-2
#' @param r1.s2stg stopping response bar at stage-1
#' @param r.s2stg stopping response bar at stage-2
#' @param r.ia observed number of response at end of stage-1
#' @param r observed total number of response at end of stage-2
#'
#' @return whether to reject null hypothesis (0 for fail to reject, 1 for to reject) and the sample size used
#' @export
#'
#' @examples
#' Simon2stg_analysis(n1.s2stg=20,n.s2stg=40,r1.s2stg=1,r.s2stg=13,r.ia=3,r=6)
Simon2stg_analysis <- function(n1.s2stg,n.s2stg ,r1.s2stg , r.s2stg ,r.ia,r){
  if(r.ia <=r1.s2stg ){
    rej <- 0
    n.tot <- n1.s2stg
  } else {
    n.tot <- n.s2stg
    if(r <=r.s2stg){
      rej <- 0

    } else {
      rej <- 1
    }
  }
  res <- data.frame(rej=rej,totN=n.tot)
  return(res)
}

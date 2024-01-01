#' Get significance,mean and sd without exception value
#'
#' @param data the data you wanna calculating.Make sure that the first column is diferent type,and the second is value.
#' @param alpha level of significance test
#' @param method it can be 'more' when there are more than three replicates per experimental treatment;otherwise,it should be 'less'
#' @return process data out
#' @export lsdd
#' @import agricolae
#' @import magrittr
#' @import dplyr
#' @importFrom tidyr spread
#' @author Sidong-Li <2875620735@qq.com>
#' @examples lsdd(data=yourdata)
lsdd <- function(data,alpha=0.05,method="more"){
  options(warn = -1)
  if (method=="more") {
    colnames(data) <- c("type","value")
    a=1
    q2<- unique(data$type)
    q3 <- c()
    for (i in 1:length(q2)) {
      data$type[c(data$type==q2[i])] %>% length()->q3[i]
    }
    b <- max(q3)
    ak <- function(x){
      x2 <- as.numeric(na.omit(as.numeric(x)))
      x <- as.numeric(na.omit(as.numeric(x)))
      out <- c()
      for (i in 1:length(x2)){
        bk <- sd(x[-i])
        mc <- mean(x[-i])
        if (abs(mc-x[i]) > bk*3){
          out[i] <- x[i]
          x2[i] <- NA
        }
        out <- as.numeric(na.omit(out))
      }
      return(list(Y=x2,N=out))}
    ww <- data$value
    for (i in 1:(nrow(data)/b)) {
      ak(ww[(1+(i-1)*b):(i*b)]) %>% .$Y->ww[(1+(i-1)*b):(i*b)]
    }
    data$value <- ww
    process<- data
    data %>%aov(value~type,data=.) %>%
      LSD.test("type",group = T,console = T,alpha = alpha) %>% .$groups ->qw
    data %<>% group_by(type) %>% mutate(id=1:n()) %>%
      spread(key = "type",value = "value",convert = T,drop = F) %>%
      t() %>% data.frame() %>% .[-1,]


    se <- function(x){
      q <- ak(x)
      x <- as.numeric(q$Y)
      x <- na.omit(x)
      w <- sd(x)
      return(w)
    }
    me <- function(x){
      q <- ak(x)
      x <- as.numeric(q$Y)
      x <- na.omit(x)
      w <- mean(x)
      return(w)
    }
    data[,length(data)+1] <- apply(X = data[,a:b],MARGIN = 1,FUN = me)
    colnames(data) <- c(colnames(data[-length(data)]),"mean")
    data[,length(data)+1] <- apply(X = data[,a:b],MARGIN = 1,FUN = se)
    colnames(data) <- c(colnames(data[-length(data)]),"sd")
    oute <- c()
    oute <- as.list(x = oute)
    for (i in 1:nrow(data)){
      p <- ak(data[i,a:b])
      oute[[i]]<- p$N
    }
    type <- rownames(data)
    rownames(data) <- NULL
    data <- cbind(type,data)
    type <- rownames(qw)
    rownames(qw) <- NULL
    qw <- cbind(type,qw)
    qw <- qw[,c(1,3)]
    data <- merge(data,qw,by="type")
    data <- data[,c(1,ncol(data)-2,ncol(data)-1,c(ncol(data)))]
    return(list("data"=data,"out"=oute,"process"=process))
  }else if (method=="less") {
    colnames(data) <- c("type","value")
    data$value <- as.numeric(data$value)
    data$type <- as.character(data$type)
    data%>% aov(value~type,data=.) %>%
      LSD.test("type",group = T,console = T,alpha = alpha) %>% .$groups %>% mutate(value=NULL)->e1
    e1$type <- row.names(e1)
    data %<>% group_by(type) %>% summarise(mean=mean(value),sd=sd(value)) %>% as.data.frame()
    data<- merge(data,e1,by = "type")
    return(list("data"=data))
  }
  else{
    message("method must be 'more' or 'less'!")
  }
}
.onLoad <- function(libname,pkgname){
  message("Thank you for using this package!")
}

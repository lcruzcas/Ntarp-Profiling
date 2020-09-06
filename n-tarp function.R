########################
#### N-tarp code #######
########################

install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
library(shinyBS)

Ntarp<-function(data, n , P)
{
  data0<-as.data.frame(data)
  a<-dim(data0)[1]
  b<-dim(data0)[2]
  numeric<-sapply(data0, is.numeric)
  all(numeric)
  if (all(numeric)==FALSE)
  {
    stop("All variables must be numeric")
    UNDECLARED()
  }
  
  else{
    S1Numeric<-as.matrix(data0)
    v=dim(S1Numeric)[2]
    m=dim(S1Numeric)[1]
    
    seeds<-sample(1:n*10, n, replace=F)
    Distribution<-matrix(0,m,n)
    
    for(z in 1:n)
    {
      for(i in 1:m)
      {
        set.seed(seeds[z])
        RandomVector<-rnorm(v,0,1)
        Vector1<-S1Numeric[i,]
        Product<-Vector1%*%RandomVector
        Distribution[i,z]<-Product
      }
    }
    
    DistributionD<-as.data.frame(Distribution)
    colnames(DistributionD)<- paste("V", seeds, sep="_")
    
    W=c()
    C=matrix(0L,m,n)
    C<-as.data.frame(C)
    
    for(f in 1:n)
    {
      clusterK<-kmeans(DistributionD[,f], 2)
      cluster<-clusterK$cluster
      clusterSize<-clusterK$size
      W[f]<-sum(clusterK$withinss)/(var(DistributionD[,f])*m)
      C[,f]<-cluster
      C<-as.data.frame(C)
    }
    
    C[a+1,]<-W
    w_opt<-min(W)
    C[a+2,]<-ifelse(C[a+1,]<0.36,1,0)
    percentage<-sum(C[a+2,])/n
    
    if(P==FALSE)
    {
      cluster<-C[,C[a+1,]==min(C[a+1,])]
      cluster<-cluster[-(a+2)]
      cluster<-cluster[-(a+1)]
      mylist<-list(cluster,percentage,w_opt )
      names(mylist)<-c("cluster","percentage","w opt")
      return(mylist)
      
    }
    
    else{
      
      j=sum(C[a+2,])
      C1<-C[,C[a+2,]==1]
      C1<-C1[,order(C1[a+1,])]
      C1<-C1[-(a+2),]
      C1<-C1[-(a+1),]
      cols <- colnames(C1)
      C1$profile<-apply( C1[ , cols ] , 1 , paste , collapse = "" )
      profile<-C1$profile
      U<-length(unique(C1$profile))
      mylist<-list(profile,U )
      names(mylist)<-c("Profile","Number of Unique profiles")
      return(mylist)
    }
    
    
  } 
}

setwd("C:/Users/lmcru/Google Drive/2. PHD/0. PHD/2.RESEARCH/Kerrie and Laura/CT_PROGRESSIONS/NTARP_R_PACKAGE")
create("Ntarp")

M<-matrix(0L,5,4)

data<-M
data<-as.data.frame(data)
data$V1<-rnorm(5,0,1)
data$V2<-rnorm(5,3,2)
data$V3<-rnorm(5,2,4)
data$V4<-rnorm(5,1,5)

data$names<-c("A","B","C","D","E")

c<-Ntarp(data, n , profile = FALSE )
  


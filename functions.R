read.arff<-function(fn="heart_train.arff"){
  rawdat<-read.delim(fn)
  dsep<-grep(pattern = "@data",x=rawdat[,1])
  att<-grep(pattern = "@attribute",x=rawdat[,1])[1]
  dat<-read.table(fn,skip = dsep+1,sep = ",")
  vname<-rep("0",dsep-1)
  vtype<-rep("0",dsep-1)
  for(i in 1:(dsep-att)){
    vname[i]<-sub(pattern=".*'(.*)'.*",replacement = "\\1",x=rawdat[i+att-1,1])
    vtype[i]<-sub(pattern=".*'.*' (.*)\\>",replacement = "\\1",x=rawdat[i+att-1,1])
  }
  colnames(dat)<-vname
  catog<-grep(pattern = "\\{",x=vtype)
  for(i in catog){
    dat[,i]<-as.factor(dat[,i])
    
  }
  return(list(dat=dat,vname=vname,vtype=vtype))
}




IsPure<- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

#IsPure(train)

entrophy<-function(data){
  if(IsPure(data)) return(0)
  else {
    n<-nrow(data)            ##n observation
    name<-unique(data[,ncol(data)])  ## values of class  
    l<-length(name)       ## l==2 in binary case, l is number of class value
    p<-numeric(l)          ##p is proportion of each feature value.
    for(i in 1:l){
      p[i]<-sum(data[,ncol(data)]==name[i])/n
    }
  return(-sum(p*log(p,2)))
  }
}  # entrophy(train)


##determine candidate numeric split
dcns<-function(data,x){
  name<-names(data)
  p<-which(name==x)       # feature X is pth column
  data<-data[order(data[,p]),]
  S<-split(data,f = data[,p],drop = T)
  v<-length(S);V<-numeric(v)
  C<-numeric(v-1)
  for(i in 1:v){
    V[i]<-S[[i]][1,p]
  }
  for(i in 1:(v-1)){
    C[i]<-(V[i]+V[i+1])/2
  }
  return(C)
}


IG<-function(data,A){    ## "age" , "sex" don't forget ""
  if(sum(colnames(data)==A)==0) return(0)
  else {
    p<-which(colnames(data)==A) ## split data by pth attribute, check if pth is numeric or categorical
    if (class(data[,p])=="numeric"|class(data[,p])=="integer") {  ## numeric variable split into 2.
      C<-dcns(data,A);k<-length(C)   ## candidate splits
      M<-numeric(k)       ##M  C(,....,) records the ig wrt each median
      for(i in 1:k){
        med<-C[i]
        data1<-data[data[,p]>med|data[,p]==med,];n1<-nrow(data1)
        data2<-data[data[,p]<med,];n2<-nrow(data2)
        n=n1+n2
        P<-c(n1/n,n2/n); H<-c(entrophy(data1),entrophy(data2)); ##pth column P proportion.
        entrophyafter<-sum(P*H);entrophybefore<-entrophy(data)
        M[i]<-entrophybefore-entrophyafter
      }
      return(c(max(M),C[M==max(M)][1]))
      
    } else{           # categorical vaiable
      l<-length(unique(data[,p]))   # l is the number of splits
      name<-unique(data[,p])
      P<-numeric(l);H<-numeric(l)
      for(i in 1:l){
        data1<-data[data[,p]==name[i],]
        P[i]<-nrow(data1);H[i]<-entrophy(data1)
      }
      P<-P/sum(P);entrophyafter<-sum(P*H);entrophybefore<-entrophy(data)
      return(entrophybefore-entrophyafter)
    }
    }  
}




##stop criteria

stp<-function(data,m){
  p<-(ncol(data)-1);n<-nrow(data);name<-names(data[,-ncol(data)])
  ig <- numeric(p)
  for (i in 1:p) {
    ig[i] <- IG(data, name[i])[1]
  }
  if((IsPure(data)==0)&(p>0)&(max(ig,na.rm = T)>0)&(n>m)) {
    return(0)
    } else {
      return(1)
      }
}



## find most frequent class
mostfreq<-function(data){
  n<-nrow(data);p<-ncol(data)
  featurevalue<-unique(data[,p])
  l<-length(unique(data[,p]))
  freq<-numeric(l)
  for(i in 1:l){
    freq[i]<-sum(data[,p]==featurevalue[i])
  }
  return(featurevalue[(freq==max(freq))][1])  ##[1] when #+ == #-
}






Isleaf<-function(tree){
  if(tree$feature=="class"){
    return(1)    
  } else {
    return(0)
    }
}


Printree <- function(tree, depth = 0) {
  if (Isleaf(tree)) {
    for (i in (1:depth)) {
      cat("     ")
    }
    cat(tree$name, " : " , tree[[5]]$name, "\n")
    #cat("depth",depth)
  } else{
    k <- (length(tree) - 4)
    
    if (depth != 0) {
      for (i in (1:depth)) {
        cat("     ")
      }
    }
    cat(tree$name, "\n")
    #cat("depth=",depth,"\n")
    
    for (i in 1:k) {
      child <- tree[[i + 4]]
      Printree(child, (depth + 1))
    }
  }
  
}




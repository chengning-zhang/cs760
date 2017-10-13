##prediction
#features<-test[1,-ncol(test)]



Predict<- function(tree, features) {
  if (Isleaf(tree)) return (tree[[5]]$name)
  if(tree$featype=="numeric"){
    #split variable is numeric
    char<-tree[[5]]$name
    med<- as.numeric(sub(".*<","",char))
    if(features[[tree$feature]]<med) {
      child<-tree[[5]];  return ( Predict(child, features))
    } else{
      child<-tree[[6]];  return ( Predict(child, features))
    }
    
  } else{      # split variable is factor
    k<-length(tree)
    for(i in 1:k){
      if(tree[[i+4]]$name==paste(tree$feature,"=",features[[tree$feature]])) break
    }
    child<- tree[[i+4]]   ## features[[]] factor []dataframe
    return ( Predict(child, features))
  }
  
}

#Predict(tree,test[1,-ncol(test)])
#n<-nrow(test)
#mistake<-0
#for(i in 1:n){
#  prd<-Predict(tree,test[i,-ncol(test)]);actl<-as.character(test[i,ncol(test)])
#  print(paste(i, ":" , "Actual: ",actl, "Predicted: ",prd))
#  if(prd!=actl) mistake<-mistake+1 
#}
#mistake

prediction<-function(tree,test){    ## only 90th error
  n<-nrow(test)
  mistake<-0
  for(i in 1:n){
    prd<-Predict(tree,test[i,-ncol(test)]);actl<-as.character(test[i,ncol(test)])
    print(paste(i, ":" , "Actual: ",actl, "Predicted: ",prd))
    if(prd!=actl) mistake<-mistake+1 
  }
  print(paste("number of correctly classified: ",
              n-mistake,"total number of test instance",n))
}

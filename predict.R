##prediction
#features<-test[1,-ncol(test)]
#Predict<- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child<- tree$children[[features[[tree$feature]]]]   ## features[[]] factor []dataframe
  return ( Predict(child, features))
}


Predict<- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  if(tree$featype=="numeric"){
    #split variable is numeric
    char<-tree$children[[1]]$name
    med<- as.numeric(sub(".*<","",char))
    if(features[[tree$feature]]<med) {
      child<-tree$children[[1]];  return ( Predict(child, features))
    } else{
      child<-tree$children[[2]];  return ( Predict(child, features))
    }
    
  } else{      # split variable is factor
  child<- tree$children[[features[[tree$feature]]]]   ## features[[]] factor []dataframe
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

prediction<-function(tree,test){
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

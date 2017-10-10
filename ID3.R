
#### mostfreq   !!!!! mistake

ID3 <- function(node, data, m) {
  node$obsCount <- nrow(data)
  #if the data-set is pure (e.g. all toxic), then
  if (stp(data,m)) {    ## satisfy stop criteria or not
    child <-
      node$AddChild(mostfreq(data))    # factor not working mostfreq(data) because of #+==#-
    node$feature <- tail(names(data), 1);node$featype<-''
    child$obsCount <- nrow(data)
    child$feature <- '';child$featype<-''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    name <-
      colnames(data)[-ncol(data)]
    p <- length(name)
    #p is number of attributes
    ig <- numeric(p)
    for (i in 1:p) {
      ig[i] <- IG(data, name[i])
    }
    
      feature <-
        name[which(ig == max(ig))][1]  ## find the feature with largest ig,if tie,first 1
      node$feature <- feature
      #take the subset of the data-set having that feature value
      ## delete that feature, then split data, childobs is a list
      if (class(data[, feature]) != "numeric") {
        # if feature is category
        node$featype<-"factor"
        childObs <-
          split(data[,!(names(data) %in% feature)], data[, feature], drop = TRUE)
        for (i in 1:length(childObs)) {
          ## childobs is a list,
          #construct a child having the name of that feature value (e.g. 'red')
          child <-
            node$AddChild(paste(feature, "=", names(childObs)[i]))
          #call the algorithm recursively on the child and the subset
          ID3(child, childObs[[i]], m)   ### [i] is still a list,[[i]] is dataframe.
        }
      } else{
        ## numeric feature
        node$featype<-"numeric"
        med <- median(data[, feature])
        childObs <-
          split(data[,!(names(data) %in% feature)], data[, feature] > med, drop = TRUE)
        #construct a child having the name of that feature value >50 <=50
        child1 <-
          node$AddChild(paste(feature, "<", med))
        ID3(child1, childObs[[1]], m)
        child2 <-
          node$AddChild(paste(feature, ">", med))
        ID3(child2, childObs[[2]], m)
      }
    
  }
}

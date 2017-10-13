

ID3 <- function(node, data, m=10) {
  node$obsCount <- nrow(data)
  #if the data-set is pure (e.g. all toxic), then
  if (stp(data,m)) {    ## satisfy stop criteria or not
    node$feature <- tail(names(data), 1);node$featype<-''
    child <-list(name=as.character(mostfreq(data)),obsCount=nrow(data),
                 feature='',
                 featype='')
    node[[length(node)+1]]<-child
    return(node)
      #node$AddChild(mostfreq(data))    # factor not working mostfreq(data) because of #+==#-
    #child$obsCount <- nrow(data)
    #child$feature <- '';child$featype<-''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    name <-
      colnames(data)[-ncol(data)]
    p <- length(name)
    #p is number of attributes
    ig <- numeric(p)
    for (i in 1:p) {
      ig[i] <- IG(data, name[i])[1]
    }
    
    feature <-
      name[which(ig == max(ig,na.rm = T))][1]  ## find the feature with largest ig,if tie,first 1
    node$feature <- feature
    #take the subset of the data-set having that feature value
    ## delete that feature, then split data, childobs is a list
    if (class(data[, feature]) != "numeric" & class(data[, feature]) != "integer") {
      # if feature is category
      node$featype<-"factor"
      childObs <-
        split(data[,!(names(data) %in% feature)], data[, feature], drop = TRUE)
      for (i in 1:length(childObs)) {
        ## childobs is a list,
        #construct a child having the name of that feature value (e.g. 'red')
        child <-list(name=paste(feature, "=", names(childObs)[i]),
                     obsCount=nrow(childObs[[i]]),
                     feature='',
                     featype='')
        #call the algorithm recursively on the child and the subset
            ### [i] is still a list,[[i]] is dataframe.
        node[[length(node)+1]]<-ID3(child, childObs[[i]], m); 
      }
        return(node)
      
      } else{
      ## numeric feature
      node$featype<-"numeric"
      med <- IG(data,feature)[2] 
      childObs <-
        split(data[,!(names(data) %in% feature)], data[, feature] > med, drop = TRUE)
      #construct a child having the name of that feature value >50 <=50
      child1 <-list(name=(paste(feature, "<", med)),
                    obsCount=nrow(childObs[[1]]),
                    feature='',
                    featype='')
      node[[length(node)+1]]<-ID3(child1, childObs[[1]], m)

      
      child2 <-list(name=(paste(feature, ">", med)),
                    obsCount=nrow(childObs[[2]]),
                    feature='',
                    featype='')
      node[[length(node)+1]]<-ID3(child2, childObs[[2]], m)
      return(node)
    }
    
  }

}

library(ggplot2)

margProb <- function(x) as.vector(table(x))/length(x)

nameProbs <- function(probs,cnames){
    if(is.vector(cnames)){
        stopifnot(all(sapply(probs,ncol)==length(cnames)))
        for(i in 1:length(probs)) colnames(probs[[i]]) <- cnames
        return(probs)
    }
    namediff <- setdiff(names(probs),names(cnames))
    if(length(namediff)) warning(paste('names(probs) not in names(cnames):',paste(namediff,collapse='\n')))
    for(nn in names(probs)) colnames(probs[[nn]]) <- cnames[[nn]]
    probs
}

rowMax <- function(x,...) apply(x,1,max,...)
rowMin <- function(x,...) apply(x,1,min,...)

makeplot2 <- function(mod,classNames=NULL,itemNames=NULL,cnames=NULL,probs=NULL,P=NULL,include.margin=FALSE,data=NULL,calc=NULL,wght=NULL){

    probs2 <- makeProbs(mod=mod,classNames=classNames,itemNames=itemNames,cnames=cnames,
                        probs=probs,P=P,include.margin=include.margin,data=data,calc=calc)



    if(!is.null(calc) | include.margin){
        plt <- ggplot(probs2,aes(response,prob,fill=orig))
    } else plt <- ggplot(probs2,aes(response,prob))

    plt <- plt+geom_col()+facet_grid(class~item,scales='free_x')+
        theme(axis.text.x = element_text(angle = 90, hjust =1,vjust=.2,size=8),axis.text.y=element_text(size=8))+
        scale_y_continuous(breaks=c(0.5,1),labels=c(0.5,1))+
            xlab(NULL)+ylab('Probability')
    if(!is.na(mod$probs.se[1]))
        plt <- plt+geom_errorbar(aes(ymin=ebmin,ymax=ebmax),width=0)
    if('orig'%in%names(probs2)) plt <- plt+theme(legend.position='none')
    plt
}


makeProbs <- function(mod,classNames=NULL,itemNames=NULL,cnames=NULL,probs=NULL,P=NULL,include.margin=FALSE,data=NULL,calc=NULL,wght=NULL){

    if(!missing(mod)){
        if(is.null(probs)) probs <- mod$probs
        if(is.null(P)) P <- mod$P
        if(is.null(data)) data <- mod$y
    }
    if(!is.null(cnames)) probs <- nameProbs(probs,cnames)
    if(is.null(classNames)) classNames <- paste('Class',1:nrow(probs[[1]]))
    if(!is.null(P))
        classNames <- paste0(classNames,'\n',round(P*100),
                             #ifelse(is.na(mod$P.se),'',paste0('\u00B1',round(200*mod$P.se))),
                             '%')
    if(include.margin){
        if(is.null(data)) stop('need data to include marginal probabilities')
        stopifnot(all(names(probs)%in%names(data)))
        for(nn in names(probs)) probs[[nn]] <- rbind(probs[[nn]],margProb(data[[nn]],wght))
        classNames <- c(classNames,paste0('Overall',ifelse(is.null(P),'','\n100%')))
    }

    if(is.null(itemNames)) itemNames <- if(is.null(names(probs))) 1:length(probs) else names(probs)
    K.j <- sapply(probs,ncol)
    probs2 <- NULL
    resps <- NULL
    for(i in 1:length(K.j)){
        pp <- as.data.frame(probs[[i]])
        resps <- c(resps,colnames(pp))
        pp$class <- classNames
        pp <- pp%>%gather(response,prob,1:K.j[i])
        pp$item <- itemNames[i]
        if(!is.null(calc)) pp$orig <- !names(probs)[i]%in%calc
        probs2 <- rbind(probs2,pp)
    }
    if(!is.na(mod$probs.se[1])){
        pse <- NULL
        for(i in 1:length(K.j)){
            pp <- as.data.frame(mod$probs.se[[i]])
            names(pp)=colnames(probs[[i]])
            if(include.margin) pp <- rbind(pp,0)
            pp$class <- classNames
            pp <- pp%>%gather(response,se,1:K.j[i])
            pp$item <- itemNames[i]
            pse <- rbind(pse,pp)
        }
        probs2 <- merge(probs2,pse,all.x=TRUE)
        probs2 <- within(probs2,{
                             ebmin=rowMax(cbind(prob-2*se,0))
                             ebmax=rowMin(cbind(prob+2*se,1))
                         }
                         )
    }
    probs2$response <- factor(probs2$response,levels=unique(resps))
    probs2$class <- factor(probs2$class,levels=classNames)
    probs2$item <- factor(probs2$item,levels=itemNames)

    if(include.margin){
        if(is.null(calc)) probs2$orig <- TRUE
        probs2$orig[grep('^Overall',probs2$class)] <- FALSE
    }

    probs2

}


library(poLCA)
library(ggplot2)

simLCA <- function(mod){
    sim <- poLCA.simdata(probs=mod$probs,P=mod$P)
}

compare <- function(mod,sim){
    if(missing(sim)) sim <- simLCA(mod)
    datO <- mod$y
    datS <- sim$dat
    names(datS) <- names(datO)

    transDatO <- function(i){
        tab <- table(datO[[i]])
        data.frame(prob=as.vector(tab)/nrow(datO),
                   value=attr(tab,'dimnames')[[1]],
                   varb=names(datO)[i],
                   real='orig',
                   stringsAsFactors=FALSE)
    }

    transDatS <- function(i){
        tab <- table(datS[[i]])
        data.frame(prob=as.vector(tab)/nrow(datS),
                   value=attr(tab,'dimnames')[[1]],
                   varb=names(datS)[i],
                   real='sim',
                   stringsAsFactors=FALSE)
    }



    pdat <- rbind(do.call('rbind',lapply(1:ncol(datO), transDatO)),
                  do.call('rbind',lapply(1:ncol(datS),transDatS)))

    ggplot(pdat,aes(value,prob,fill=real))+geom_bar(stat='identity',position='dodge')+facet_wrap(~varb,ncol=4)+
        ggtitle(paste('LCA with',length(mod$P),'groups'))
}


joint <- function(mod,sim,varb1,varb2,labels=FALSE){
    if(missing(sim)) sim <- simLCA(mod)
    probO <- table(mod$y[[varb1]],mod$y[[varb2]])
    labs <- attr(probO,'dimnames')
    probO <- as.vector(probO)/sum(probO)

    probS <- as.vector(table(sim$dat[[which(names(mod$y)==varb1)]],
                             sim$dat[[which(names(mod$y)==varb2)]]))
    probS <- probS/sum(probS)

    m <- max(c(probO,probS))
    plot(probO,probS,xlab='orig',ylab='sim',main=paste(varb1,varb2),cex=ifelse(labels,0,1),
         ylim=c(0,m),xlim=c(0,m))
    if(labels){
        labs <- expand.grid(labs[[1]],labs[[2]])
        labs <- do.call('paste',labs)
        text(probO,probS,labs)
        title(sub=paste('LCA with',length(mod$P),'groups'))
    }
    text(m/10,m-m/10,labels=paste('RMSE=\n',round(sqrt(mean((probO-probS)^2)),3)),cex=0.75)
    abline(0,1)
}

jointTot <- function(mod,sim){
    if(missing(sim)) sim <- simLCA(mod)
    omar <- par()$mar#par(mfrow=c(6,6))
    olayout <- par()$mfrow

    par(mar=c(3,2,2,1))
    mat <- matrix(0,nrow=6,ncol=6)
    mat[1,] <- 1:6
    mat[2,2:6] <- 7:11
    mat[3,3:6] <- 12:15
    mat[4,4:6] <- 16:18
    mat[5,5:6] <- 19:20
    mat[6,6] <- 21
    mat[4,2] <- 22

    layout(mat)
    for(i in 1:6) for(j in i:6) joint(mod,sim,names(mod$y)[i],names(mod$y)[j])

    plot(1:10,cex=0,xaxt='n',yaxt='n')
    text(5,5,paste('LCA with \n', length(mod$P),'classes'),cex=1)
                                        #par(mfrow=c(1,1))
    par(mar=omar)
    par(mfrow=olayout)
}

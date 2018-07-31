#par(mfrow=c(2,1))
ntreeRFts=50
t1=system.time({
	noftree=25
	OOB_error_rate_node=array(0,length(nofnodelevels))
	for(nd in 1:length(nofnodelevels)){ #for each nnumber of trees (J_{ins})
	    nofnode=nofnodelevels[nd]
	    RFins <- randomForest(as.matrix(finaltrain[,2:ncol(finaltrain)]),factor(finaltrain[,1]),ntree=noftree,maxnodes=nofnode)
	    train_terminal=attr(predict(RFins, finaltrain[,2:ncol(finaltrain)] ,nodes=TRUE), "nodes")
	    codetr=matrix(generatecodebook(RFins$forest$nodestatus,train_terminal,nofnode,ntrainobs),noftrain,noftree*nofnode)	
	    RFts <- randomForest(codetr,classtrain,ntree=ntreeRFts)
	    OOB_error_rate_node[nd]=1-sum(predict(RFts,type='response')==classtrain)/noftrain
	}
	#plot(nofnodelevels,OOB_error_rate_node,type="l",col="red",xlab="Alphabet size",ylab="Error rate",pch=4,lty=2)
	#points(nofnodelevels,OOB_error_rate_node,type="p",col="red",pch=4)
})

t2=system.time({
	OOB_error_rate=array(0,length(noftreelevels))
	nofnode=nofnodelevels[which.min(OOB_error_rate_node)]
	for(nd in 1:length(noftreelevels)){ #for each nnumber of trees (J_{ins})
	    noftree=noftreelevels[nd]
	    RFins <-randomForest(finaltrain[,2:ncol(finaltrain)],factor(finaltrain[,1]),ntree=noftree,maxnodes=nofnode)
	    train_terminal=attr(predict(RFins, finaltrain[,2:ncol(finaltrain)] ,nodes=TRUE), "nodes")
	    codetr=matrix(generatecodebook(RFins$forest$nodestatus,train_terminal,nofnode,ntrainobs),noftrain,noftree*nofnode)	
	    RFts <- randomForest(codetr,classtrain,ntree=ntreeRFts)
	    OOB_error_rate[nd]=1-sum(predict(RFts,type='response')==classtrain)/noftrain
	}
	#plot(noftreelevels,OOB_error_rate,type="l",col="red",xlab="Number of trees",ylab="Error rate",pch=4,lty=2)
	#points(noftreelevels,OOB_error_rate,type="p",col="red",pch=4)
	noftree=noftreelevels[which.min(OOB_error_rate)]
})

rm(RFins,RFts); gc();

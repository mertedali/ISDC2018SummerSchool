#classtest=testdata[,1] #classes of the training data
noftest=nrow(testdata)
seriesLen=apply(testdata[,2:ncol(testdata)],1,function(x) sum(!is.na(x)))
observations=array(0,sum(seriesLen)-noftest)
difference=array(0,sum(seriesLen)-noftest)
st=1
for(i in 1:noftest){
	curseries=testdata[i,!is.na(testdata[i,])]
	curclass=curseries[1]
	#standardize if necessary
	numseries=as.numeric(curseries[2:length(curseries)])
	numseries=(numseries-mean(numseries))/sd(numseries)		#WARNING!
	en=st+seriesLen[i]-2
	observations[st:en]=numseries[2:length(numseries)]
	difference[st:en]=diff(numseries)
	obsclass=rep(curclass,seriesLen[i]-1)
	if(i==1){
		allobsclass=obsclass
	} else {
		allobsclass=c(allobsclass,obsclass)
	}
	st=en+1
}
	
timeindices=unlist(lapply(seriesLen,function(x) c(2:x))) #create time indices
finaltest=data.frame(Class=allobsclass,timeindices,observations,difference)
ntestobs=seriesLen-1
#following codes obtains the training and test matrix in the paper
#these are the ones that goes into random forest model, RFobs

datatraintimestart=proc.time()
nofclass=length(unique(traindata[,1]))	#number of classes
classtrain=traindata[,1] #classes of the training time series
noftrain=nrow(traindata) #number of training series
seriesLen=apply(traindata[,2:ncol(traindata)],1,function(x) sum(!is.na(x))) #length of each series
observations=array(0,sum(seriesLen)-noftrain) #observation array (storing all observations as a column)
difference=array(0,sum(seriesLen)-noftrain)	#difference array (storing difference between consecutive observations as a column)

#for each time series observations should be standardized and
#we need to concatenate the observation of each time series as single column
#as well as the differences 
st=1
for(i in 1:noftrain){
	curseries=traindata[i,!is.na(traindata[i,])]
	curclass=curseries[1]
	#standardize if necessary
	numseries=as.numeric(curseries[2:length(curseries)])
	numseries=(numseries-mean(numseries))/sd(numseries)		# WARNING!
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
#final train matrix stores class,time index,observation and consecutive difference
finaltrain=data.frame(Class=allobsclass,timeindices,observations,difference)
ntrainobs=seriesLen-1
datatraintimeend=proc.time()
datatrainprepdur=datatraintimeend-datatraintimestart
datatrainprepdur=datatrainprepdur[3]

datatesttimestart=proc.time()
#test data
classtest=testdata[,1] #classes of the training data
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

datatesttimeend=proc.time()
datatestprepdur=datatesttimeend-datatesttimestart
datatestprepdur=datatestprepdur[3]

#algorithm for generating the distribution of symbols
generatecodebook <- function(nodestatus,terminal,nofterminal,nofobservations) {
  if(!is.loaded("generate_codebook")) dyn.load("mts_functions64bit.dll")
  nofseries=length(nofobservations)
  noftree=ncol(terminal)
  nofnode=nrow(nodestatus)
  total=sum(nofobservations)
  nofentry=nofseries*nofterminal*noftree
  out <- .C("generate_codebook", as.integer(as.matrix(nodestatus)), as.integer(nofnode), as.integer(noftree), as.integer(as.matrix(terminal)), as.integer(nofterminal), as.integer(nofobservations), as.integer(total), as.integer(nofseries), result=double(nofentry))
  return(out$result)
}


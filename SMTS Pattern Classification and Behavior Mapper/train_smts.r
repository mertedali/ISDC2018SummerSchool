params = result[, 1:2]
result = result[, -c(1:2)]
result = cbind(1, result)
testdata = result

#install following packages first
require(randomForest)
require(plyr)

#change your working directory
require(RCurl)

classes=read.csv(text = getURL("https://raw.githubusercontent.com/mertedali/ISDC2018SummerSchool/master/SMTS%20Pattern%20Classification%20and%20Behavior%20Mapper/classes.csv"), header=F)
dataset=read.csv(text = getURL("https://raw.githubusercontent.com/mertedali/ISDC2018SummerSchool/master/SMTS%20Pattern%20Classification%20and%20Behavior%20Mapper/DataSet.csv"),header=T)
dataset=dataset[,2:ncol(dataset)]
# folds=read.csv("C:/Users/Mert/Desktop/SMTS/fold1.csv",header=F)

noftreelevels=c(10,25,50) # number of trees for symbol generation J_{ins} -- example to add more levels -> c(25,50,100,200) 
nofnodelevels=c(10,25) # alphabet size R -- example to add more levels -> c(50,250,500) 
maxiter=20 		   # the maximum number of iterations for training trees
noftree_step=50	   # the number of trees to train per iteration
tolerance=0.05	   # tolerance level for OOB error rate improvement for RFts
nofrep=1		   # number of replications

results<-NULL #matrix to store the results for all levels
classresults<-NULL
set.seed(1)

foldid = 1

#generate random test/train data
trclass=as.data.frame(classes)
colnames(trclass)="x"
uniqueclass=unique(trclass$x)
#assign numeric class ids
uniqueclass=data.frame(x=uniqueclass,ID=c(1:length(uniqueclass)))
trclass=join(trclass,uniqueclass,type='left')

trdata=dataset
traindata=as.matrix(cbind(trclass$ID,trdata[,1:ncol(trdata)]))

#prepare data
source("data_preparation.r")
			
allclass=factor(c(classtrain,classtest))
classtrain=allclass[1:noftrain]
classtest=allclass[(noftrain+1):(noftrain+noftest)]
print(sprintf("Rep   Fold	R   Jins   Jts   OOB(E)   Test(E)   (T)R   (T)Jins   (T)Overall   (T)Train   (T)Test"))

rfinstrainstart=proc.time()
ptm <- proc.time()
source("parameter_selection_noparallel.r")  
RFins=randomForest(as.matrix(finaltrain[,2:ncol(finaltrain)]),factor(finaltrain[,1]),ntree=noftree,maxnodes=nofnode)

#get terminal node ids	
train_terminal=attr(predict(RFins, as.matrix(finaltrain[,2:ncol(finaltrain)]) ,nodes=TRUE), "nodes")	
codetr=matrix(generatecodebook(RFins$forest$nodestatus,train_terminal,nofnode,ntrainobs),noftrain,noftree*nofnode)
rfinstrainend=proc.time()	
rfinsduration=rfinstrainend-rfinstrainstart
rfinsduration=rfinsduration[3]

rfinsteststart=proc.time()
test_terminal=attr(predict(RFins, as.matrix(finaltest[,2:ncol(finaltest)]) ,nodes=TRUE), "nodes")
codetst=matrix(generatecodebook(RFins$forest$nodestatus,test_terminal,nofnode,ntestobs),noftest,noftree*nofnode)
rfinstestend=proc.time()	
rfinstestduration=rfinstestend-rfinsteststart
rfinstestduration=rfinstestduration[3]

rm(RFins,train_terminal,test_terminal); gc();
rftstrainstart=proc.time()
RFts=randomForest(codetr,classtrain,ntree=noftree_step)
prev_OOBerror=1; cur_OOBerror=1-sum(predict(RFts,type='response')==classtrain)/noftrain
iter=1
while(iter<20&&cur_OOBerror<(1-tolerance)*prev_OOBerror){    
	prev_OOBerror=cur_OOBerror
	RFtsmid=randomForest(codetr,classtrain,ntree=noftree_step,nodesize=2)
	RFts=combine(RFts, RFtsmid)
	cur_OOBerror=1-sum(predict(RFts,type='response')==classtrain)/noftrain
	iter=iter+1
}	   
OOB_error=1-sum(predict(RFts,type='response')==classtrain)/noftrain

rftstrainend=proc.time()	
rftsduration=rftstrainend-rftstrainstart
rftsduration=rftsduration[3]

rftsteststart=proc.time()
predicted=predict(RFts,codetst,type='response')
rftstestend=proc.time()	
rftstestduration=rftstestend-rftsteststart
rftstestduration=rftstestduration[3]
print("Training is finished!")
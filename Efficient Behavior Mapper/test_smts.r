params = result[, 1:2]
result = result[, -c(1:2)]
result = cbind(1, result)
testdata = result


source("prepare_test.r")


test_terminal=attr(predict(RFins, as.matrix(finaltest[,2:ncol(finaltest)]) ,nodes=TRUE), "nodes")
codetst=matrix(generatecodebook(RFins$forest$nodestatus,test_terminal,nofnode,ntestobs),noftest,noftree*nofnode)


predicted=predict(RFts,codetst,type='response')

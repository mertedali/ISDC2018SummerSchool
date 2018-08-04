library(rpart.plot)
test_generation_time = proc.time() 
result<-generate_initial_points(10000,21,1)
test_generation_time = proc.time()- test_generation_time
test_result_time =  proc.time()
source("test_smts.r")
test_data<-data.table(cbind(params,predicted))
test_result_time =  proc.time()-test_result_time

method_time = proc.time()
result<-generate_initial_points(50,21,1)


observ = data.table()
rr=0
previous_rule =data.table()
while(TRUE){
	source("test_smts.r")
	rr = rr+1
	
	for(i in 1:ncol(params)){
		if(i ==1){
			new_params=data.table(unlist(params[,i]))
		}else{
			new_params = cbind(new_params,unlist(params[,i]))
		}
	}

	params = new_params
	if(nrow(observ)<1){
		observ <- cbind(params,V3=as.factor(predicted))
	}else{
		tempe<-data.table(cbind(data.frame(params),as.factor(predicted)))
		colnames(tempe)<-colnames(observ)
		observ <- rbind(observ,tempe)
	}
	if(nrow(observ)>300){
	break
	}
	observ$V3 = as.factor(observ$V3)
	rpart_model <- rpart(V3 ~.,data = observ,control=rpart.control(maxnodes=2))
	#rpart.plot(rpart_model)
	#readline("durdu")
	temp_data<-data.table(Terminal_Node =as.factor(rownames(rpart_model$frame)[rpart_model$where]),Class=observ$V3)  

	
	rule_matrix<-temp_data[,list(Class=mostrepeated(Class), Prob = calc_prob(Class)),by="Terminal_Node"]


	t_nodes = as.numeric(rownames(rpart_model$frame)[rpart_model$frame$var == "<leaf>"])
	terminal_paths = path.rpart(rpart_model,t_nodes,print.it=FALSE)

	first = TRUE
	col_name=c()
	for(k in 1:ncol(params)){
		rule_matrix<-cbind(rule_matrix,rep(min(data.frame(observ)[,k]),nrow(rule_matrix)))
		rule_matrix<-cbind(rule_matrix,rep(max(data.frame(observ)[,k]),nrow(rule_matrix)))
		col_name = c(col_name,paste("V",k,"_lower",sep=""),paste("V",k,"_upper",sep=""))
	}
	colnames(rule_matrix) <- c(colnames(rule_matrix)[1:3],col_name)
	for(k in unique(rule_matrix$Terminal_Node)){
		t_temp <- terminal_paths[[k]]
		for(l in 2:length(t_temp)){
			partitioned = strsplit(t_temp[l],">=")[[1]]
			status = "_lower"
			if (length(partitioned)<2){
				partitioned = strsplit(t_temp[l],"<")[[1]]
				status = "_upper"
			}
			if(status == "_lower"){
				rule_matrix[Terminal_Node==k][[paste(partitioned[1],status,sep="")]]=as.numeric(partitioned[2])
			}
			else{
				rule_matrix[Terminal_Node==k][[paste(partitioned[1],status,sep="")]]=as.numeric(partitioned[2])
			}
		}
	}
	#print(rule_matrix)
	
	previous_rule = copy(rule_matrix)

	treshold = 0.99
	rule_matrix<-cbind(rule_matrix,data.table(Sample_Size = rep(0,nrow(rule_matrix))))
	rule_matrix[Prob<treshold]$Sample_Size<-round(10*(1-rule_matrix[Prob<treshold]$Prob)/sum((1-rule_matrix[Prob<treshold]$Prob)))
	if (nrow(rule_matrix)<1){
		break
	}
	result_matrix = data.table()
	dimens<-(ncol(rule_matrix)-4)/2
	for (i in unique(rule_matrix[Sample_Size>0]$Terminal_Node)){
		temp_sample <- rule_matrix[Terminal_Node==i]
		for ( j in 1:dimens){
			if(data.frame(temp_sample)[,3+2*j-1]==data.frame(temp_sample)[,3+2*j]){
				temp_temp_sample = rep(data.frame(temp_sample)[,3+2*j-1],temp_sample$Sample_Size*1)
			}else{
				temp_temp_sample = runif(1*temp_sample$Sample_Size,min=data.frame(temp_sample)[,3+2*j-1],max=data.frame(temp_sample)[,3+2*j])
			}
			if(j==1){
				new_sample = data.table(temp_temp_sample)
			}else{
				new_sample = cbind(new_sample,temp_temp_sample)
			}
			
		}
		if (nrow(result_matrix)==0){
			result_matrix = new_sample
		}else{
			result_matrix = rbind(result_matrix,new_sample)
		}
	}
	if(sum(is.na(result_matrix))>0){
	break
	}
	if(nrow(result_matrix)<1){
		break
	}
	
	result_matrix=rbind(result_matrix,data.table(temp_temp_sample=runif(2,min=min(data.frame(observ)[,1]),max=max(data.frame(observ)[,1])),temp_temp_sample=runif(1,min=min(data.frame(observ)[,2]),max=max(data.frame(observ)[,2]))))
	
	
	result = NULL
	for (i in 1:nrow(result_matrix)){
		result = rbind(result, c(result_matrix[i,1], result_matrix[i,2], temp(art0 = -10, prt0 = -10, alpha = as.numeric(result_matrix[i,1]), beta = as.numeric(result_matrix[i,2]), drt = 0, finaltime = 50, dt = 0.125)[, 1]))
	}
	temp_result_copy = copy(result)
	result = temp_result_copy
	
	#test_predicted = predict(rpart_model,test_data,type="class")
	#print(sum(as.numeric(test_predicted == test_data$predicted))/length(test_predicted))
}
method_time = proc.time()-method_time



test_generation_time = proc.time() 
result<-generate_initial_points(10000,21,1)
test_generation_time = proc.time()- test_generation_time
test_result_time =  proc.time()
source("test_smts.r")
test_data<-data.table(cbind(params,predicted))
test_result_time =  proc.time()-test_result_time




# #k-nn
# library(class)
# test_predicted = knn(train =observ[,c(1,2)],test_data[,c(1,2)],cl= as.factor(unlist(observ[,3])),k=1)
# merged_predictions = data.frame(test_predicted)
# train_predicted= knn(train =observ[,c(1,2)],observ[,c(1,2)],cl= as.factor(unlist(observ[,3])),k=1)
# merged_train = data.frame(train_predicted)

# #naive bayes
# library(e1071)
# NB=naiveBayes(as.factor(V3)~V1+V2, data=observ)
# train_predicted= predict(NB,observ[,c(1,2)])
# test_predicted = predict(NB,test_data[,c(1,2)])
# merged_predictions = cbind(merged_predictions,data.frame(test_predicted))
# merged_train = cbind(merged_train,data.frame(train_predicted))

# #svm
# svm_model=svm(observ[,c(1,2)], y = as.factor(unlist(observ[,3])),kernel ="radial", degree = 3, tolerance = 0.001, epsilon = 0.1)
# test_predicted = predict(svm_model,test_data[,c(1,2)])
# merged_predictions = cbind(merged_predictions,data.frame(test_predicted))
# train_predicted= predict(svm_model,observ[,c(1,2)])
# merged_train = cbind(merged_train,data.frame(train_predicted))

#random_forest
rf_train_time = proc.time()
rf = randomForest(observ[,c(1,2)], y = as.factor(as.numeric(unlist(observ[,3]))))
rf_train_time = proc.time()-rf_train_time
test_predicted = predict(rf,test_data[,c(1,2)])
#merged_predictions = cbind(merged_predictions,data.frame(test_predicted))
#train_predicted= predict(rf,observ[,c(1,2)])
#merged_train = cbind(merged_train,data.frame(train_predicted))

#combined_classifier
#merged_train = cbind(merged_train,observ[,1:2])
#colnames(merged_train) = 1:6
#rf = randomForest(merged_train, y = as.factor(as.numeric(unlist(observ[,3]))))
#merged_predictions=cbind(merged_predictions,test_data[,1:2])
#colnames(merged_predictions) = 1:6
#test_predicted = predict(rf,merged_predictions)






test_total_time = test_generation_time+test_result_time

grid_creation_time = proc.time()
grid_ <- generate_grid(10000,21,1)
grid_creation_time = proc.time()-grid_creation_time

predict_time = proc.time()
test_predicted = predict(rpart_model,data.table(grid_),type="class")
predict_time = proc.time()-predict_time

test_predicted=factor(test_predicted,levels=levels(predicted))
sum(as.numeric(test_predicted == test_data$predicted))/length(test_predicted)


label = c("nexgr", "pexgr", "gr1da", "gr1db", "gr2da", "gr2db", "d1peg", "d2peg", "sshgr", "plinr", "oscct")
resultfactor = factor(test_predicted, levels = 1:11, labels = c("nexgr", "pexgr", "gr1da", "gr1db", "gr2da", "gr2db", "d1peg", "d2peg", "sshgr", "plinr", "oscct"))

final_data = as.data.frame(cbind(data.table(grid_), test_predicted))

plot_time=proc.time()
map_ = final_data
colnames(map_) = c("alpha", "beta", "class")
color = rainbow(9:25)
x11()
layout(matrix(c(1,2), ncol=1, byrow = TRUE))
plot(x = map_$alpha, y = map_$beta, main = "Behavior Map", type = "n")
text(x = map_$alpha, y = map_$beta, labels = as.character(resultfactor), col = color[as.factor(map_$class)], cex = 0.5)
plot.new()
legend(x="center", ncol = 3,legend = unique(resultfactor), fill = unique(color[as.factor(map_$class)]), title = "Classes", cex = 0.6)
print("Map produced!")
plot_time=proc.time()-plot_time

x11()
rpart.plot(rpart_model, type = 3, extra = 0)

a=temp(art0 = -10, prt0 = -10, alpha = 6.67, beta = 19.90, drt = 0, finaltime = 50, dt = 0.125)[, 1]


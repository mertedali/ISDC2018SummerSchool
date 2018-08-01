temp = function(art0, prt0, alpha, beta, drt, finaltime, dt){

	# Stocks

	art = rep(NA, finaltime / dt + 1)
	art[1] = art0
	prt = rep(NA, finaltime / dt + 1)
	prt[1] = prt0
	
	#Auxilaries

	alpha = alpha
	beta = beta
	drt = drt
	
	# Simulate
	
	for (i in 1:(finaltime / dt)){

	rtc = ((drt - prt[i]) / alpha) * dt
	pc = ((art[i] - prt[i]) / beta) * dt
	
	art[i + 1] = art[i] + rtc
	prt[i + 1] = prt[i] + pc

	}

	art = art[seq(1, length(art), 1 / dt)]
	prt = prt[seq(1, length(prt), 1 / dt)]
	
	return(cbind(art, prt))

}

generate_grid<-function(n_point,max_lim,min_lim){
	result = NULL
	point_per = sqrt(n_point)
	
	for (i in seq(min_lim, max_lim, (max_lim-min_lim+1)/point_per)){
		for (j in seq(min_lim, max_lim,(max_lim-min_lim+1)/point_per)){
			result = rbind(result, c(i, j))
		}
	}
	return(result)
}
generate_initial_points<-function(n_point,max_lim,min_lim){
	result = NULL
	point_per = sqrt(n_point)
	
	for (i in seq(min_lim, max_lim, (max_lim-min_lim+1)/point_per)){
		for (j in seq(min_lim, max_lim,(max_lim-min_lim+1)/point_per)){
			result = rbind(result, c(i, j, temp(art0 = -10, prt0 = -10, alpha = i, beta = j, drt = 0, finaltime = 50, dt = 0.125)[, 1]))
		}
	}
	return(result)
}
mostrepeated <- function(x) as(names(which.max(table(x))), mode(x)) 
calc_prob<-function(temp){
	return(max(table(temp)/length(temp)))
}
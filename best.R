best<-function(state,outcome){
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	frame<-as.data.frame(cbind(data[,2],
							data[,7],
							data[,11],
							data[,17],
							data[,23]),
							stringsAsFactors=FALSE)
	colnames(frame)<-c('hospital','state','heart attack','heart failure','pneumonia')
	if (!state %in% frame[,'state']){
		stop('invalid state')}
	else if (!outcome %in% c('hospital','state','heart attack','pneumonia')){
		stop('invalid outcome')
	}
	else{
		wh<-which(frame[,'state']==state)
		cr<-frame[wh,]
		num<-as.numeric(cr[,outcome])
		minval<-min(num , na.rm=TRUE)
		result<-cr[,'hospital'][which(num==minval)]
		output<-result[order(result)]	
	}
	return(output)
	}
					
		

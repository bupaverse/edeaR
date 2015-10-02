

csv_from_xes <- function(xesfile) {

	parsed_xes <- parseXES(xesfile)

	n_case_att <- length(parsed_xes$trace.att)
	n_event_att <- length(parsed_xes$event.att)
	n_cases <- length(parsed_xes$traces$'concept:name')
	caseids <- parsed_xes$traces$'concept:name'
	result <- data.frame(stringsAsFactors = FALSE)

	n<-1
	for(i in 1:n_cases){
		data <- parsed_xes$events[[i]]
		n_events <- length(data[[1]])
		for(j in 1:n_events){
			result[n,1]  <- c(caseids[i])
			for(k in 1:length(data)){
				if(j > length(data[[k]]))
					result[n,(k+1)] <- NA
				else
					result[n,(k+1)] <- data[[k]][[j]]

			}
			n <- n +1
		}
	}
	colnames(result)[1]<-paste("trace",names(parsed_xes$traces), sep = ".")[1]
	for(i in 1:n_event_att)
		colnames(result)[i+1] <- paste("event",names(parsed_xes$events[[1]])[i], sep = ".")
	return(result)

}



##Handler function for XES parsing.Used by parseXES
handler <- function(){
	#states: log, trace, event
	state <- "log"
	trace.data <- list()
	event.data <- list()
	trace.counter <- 0
	event.counter <- 0


	trace <- function(x,atts){
		state <<- "trace"
		trace.counter <<- trace.counter + 1
		event.data[[trace.counter]] <<- list()
	}

	event <- function(x,atts){
		state <<- "event"
		event.counter <<- event.counter + 1
	}

	endElement <- function(x,...){
		if(x =="trace"){
			state <<- "log"
			event.counter <<- 0
		}
		else if(x == "event"){
			state <<- "log"
		}
	}

	attributes <- function(x,atts){
		if(state =="trace"){
			trace.data[[atts[["key"]]]][trace.counter] <<- atts[["value"]]
		}
		else if (state == "event"){
			event.data[[trace.counter]][[atts[["key"]]]][event.counter] <<- atts[["value"]]
		}
	}


	return(list(
		trace = trace,
		event = event,
		endElement = endElement,
		date = attributes,
		string = attributes,
		int = attributes,
		float = attributes,
		boolean = attributes,
		trace.data = function(){trace.data},
		event.data = function(){event.data}
	))

}

parseXES <- function(logfile){
	library(XML)
	temp <- xmlEventParse(logfile,handler())
	tracedata <- temp$trace.data()
	trace.att <- names(tracedata)
	eventdata <- temp$event.data()
	event.att <- unique(unlist(lapply(eventdata,function(x){names(x)})))
	return(list(
		traces = tracedata,
		events = eventdata,
		trace.att = trace.att,
		event.att = event.att
	))
}

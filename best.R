best <- function(state, outcome) {
	if(! validOutcome(outcome)) {
		stop("invalid outcome")
	}
	data <- read.csv("outcome-of-care-measures.csv")
	if(! validState(state, data)) {
		stop("invalid state")
	}
	unique(data[,7])
}

validState <- function(state, data) {
	state %in% unique(data[,7])
}

validOutcome <- function(outcome) {
	outcome %in% c("heart attack", "heart failure", "pneumonia")
}
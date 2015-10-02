best <- function(state, outcome) {
	if(! validOutcome(outcome)) {
		stop("invalid outcome")
	}
	data <- read.csv("outcome-of-care-measures.csv")
	if(! validState(state, data)) {
		stop("invalid state")
	}

	if (outcome == "heart attack") {
		min <- lowest_heart_attack(data, state)
		data[data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == sprintf("%.1f", min) & data$State == state, "Hospital.Name"]
	}
}

validState <- function(state, data) {
	state %in% unique(data[,7])
}

validOutcome <- function(outcome) {
	outcome %in% c("heart attack", "heart failure", "pneumonia")
}

lowest_heart_attack <- function(data, state) {
	v <- as.vector(data[data$State == state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
	v <- v[v != "Not Available"]
	v <- as.numeric(v)
	min(v)
}
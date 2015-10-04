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
		v <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == sprintf("%.1f", min) & data$State == state, "Hospital.Name"]
	} else if (outcome == "heart failure") {
		min <- lowest_heart_failure(data, state)
		v <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == sprintf("%.1f", min) & data$State == state, "Hospital.Name"]
	} else if (outcome == "pneumonia") {
		min <- lowest_pneumonia(data, state)
		v <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == sprintf("%.1f", min) & data$State == state, "Hospital.Name"]
	}

	as.character(v)
}

validState <- function(state, data) {
	state %in% unique(data[,7])
}

validOutcome <- function(outcome) {
	outcome %in% c("heart attack", "heart failure", "pneumonia")
}

lowest_heart_attack <- function(data, state) {
	lowest(data, state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
}

lowest_heart_failure <- function(data, state) {
	lowest(data, state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
}

lowest_pneumonia <- function(data, state) {
	lowest(data, state, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
}

lowest <- function(data, state, measure) {
	v <- as.vector(data[data$State == state, measure])
	v <- v[v != "Not Available"]
	v <- as.numeric(v)
	min(v)
}
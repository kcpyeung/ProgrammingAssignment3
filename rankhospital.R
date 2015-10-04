rankhospital <- function(state, outcome, num="best") {
	if(! validOutcome(outcome)) {
		stop("invalid outcome")
	}
	data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available")
	if(! validState(state, data)) {
		stop("invalid state")
	}

	if (outcome == "heart attack") {
		v <- ranked_heart_attack(data, state)
	} else if (outcome == "heart failure") {
		v <- ranked_heart_failure(data, state)
	} else if (outcome == "pneumonia") {
		v <- ranked_pneumonia(data, state)
	}

	choose(v, num)
}

choose <- function(data, num) {
	if (num == "best") {
		data[1, ]
	} else if (num == "worst") {
		data[nrow(data), ]
	} else if (num > nrow(data)) {
		NA
	} else {
		data[num, ]
	}
}

validState <- function(state, data) {
	state %in% unique(data[,7])
}

validOutcome <- function(outcome) {
	outcome %in% c("heart attack", "heart failure", "pneumonia")
}

ranked_heart_attack <- function(data, state) {
	v <- data[data$State == state & !is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
	v <- v[order(v[,2], v[,1]), ]
	v
}

ranked_heart_failure <- function(data, state) {
	v <- data[data$State == state & !is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
	v <- v[order(v[,2], v[,1]), ]
	v
}

ranked_pneumonia <- function(data, state) {
	v <- data[data$State == state & !is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
	v <- v[order(v[,2], v[,1]), ]
	v
}

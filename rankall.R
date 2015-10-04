rankall <- function(outcome, num="best") {
	if(! validOutcome(outcome)) {
		stop("invalid outcome")
	}
	data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available")

	if (outcome == "heart attack") {
		v <- ranked_heart_attack(data)
	} else if (outcome == "heart failure") {
		v <- ranked_heart_failure(data)
	} else if (outcome == "pneumonia") {
		v <- ranked_pneumonia(data)
	}

#	v <- cleanse(v)
	choose(v, num)
}

cleanse <- function(list) {
	new_list <- list()
	for (h in list) {
		h <- as.data.frame(h)
		new_list <- rbind(new_list, h[,1:2])
	}
	new_list
}

choose <- function(list, num) {
	if (num == "best") {
		best(list)
	} else if (num == "worst") {
		worst(list)
	} else {
		n(list, num)
	}
}

best <- function(list) {
	n(list, 1)
}

worst <- function(list) {
	df <- data.frame()
	for (h in list) {
		df <- rbind(df, h[nrow(h), ])
	}
	df
}

n <- function(list, n) {
	df <- data.frame()
	for (h in list) {
		if (n <= nrow(h)) {
			df <- rbind(df, h[n, ])
		} else {
			empty_df <- data.frame("<NA>", as.character(h[1, ][2][1, ]))
			colnames(empty_df)[1] <- "hospital"
			colnames(empty_df)[2] <- "state"
			df <- rbind(df, empty_df)
		}
	}
	df
}

validOutcome <- function(outcome) {
	outcome %in% c("heart attack", "heart failure", "pneumonia")
}

ranked_heart_attack <- function(data) {
	v <- data[!is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"), c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
	v <- v[order(v[,2], v[,3]), 1:2]
	colnames(v)[1] <- "hospital"
	colnames(v)[2] <- "state"
	v <- split(v, v$state)
	v
}

ranked_heart_failure <- function(data, state) {
	v <- data[!is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
	v <- v[order(v[,2], v[,3]), 1:2]
	colnames(v)[1] <- "hospital"
	colnames(v)[2] <- "state"
	v <- split(v, v$state)
	v
}

ranked_pneumonia <- function(data, state) {
	v <- data[!is.na(data$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
	v <- v[order(v[,2], v[,3]), 1:2]
	colnames(v)[1] <- "hospital"
	colnames(v)[2] <- "state"
	v <- split(v, v$state)
	v
}

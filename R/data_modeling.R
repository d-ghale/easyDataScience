#' This function plots the correlationship between variables, insignificant correlations are leaved blank
#'
#' @param data
#' @return Correlationship between numeric variables
#' @export
plot_rcor <- function( data ) {
	numeric_data <- data[, lapply(data, is.numeric) == TRUE, with = FALSE]
	var_cor <- Hmisc::rcorr( as.matrix( numeric_data ) )
	corrplot::corrplot( var_cor$r, type = "lower", order="hclust", p.mat = var_cor$P, sig.level = 0.01, insig = "blank")
}

#' Function places the dependent variable as the first/last column in the data will make things easier
#'
#' Remember to keep only the variables that can be used for building model
#'
#' @param data
#' @param output_col column to reorder
#' @param position first or last
#' @return data with output_col as position defines
#' @export
change_output_col_pos <- function( data, output_col, position = "first" ) {
	stopifnot( is.character(position))
	all_col_names <-  names(data)
	remove_col <- output_col
	remove_rejected_col <- all_col_names[ !all_col_names %in% remove_col ]
	rejected_col <- all_col_names[ all_col_names %in% remove_col ]
	if(position == "first") {
		col_order <- c(rejected_col, remove_rejected_col)
	} else if(position == "last") {
		col_order <- c(remove_rejected_col, rejected_col)
	} else {
		NULL
	}
	data[, ..col_order]
}

#' This function produces all combinations of variables with the dependent variable
#'
#' Remember to keep the dependent variable as the first column
#'
#' @param data
#' @return create dataframe with nrow 2 and ncol ncol(df) - 1 such that the first row in each column is the dependent variable and the second row in each column is one of the independent variables
#' @export
single_combn <- function(data) {
	vec <- names(data)
	pos <- 1
	as.data.frame(rbind(vec[pos], vec[-pos]))
}

#' This function creates a column with formula for the dependent variable with each of the independent variable
#'
#' @param data
#' @return transpose the table created by single_combn then add a new column that has output ~ input structure
#' @inheritParams single_combn
#' @export
trans_vars_func <- function(data){
	t_vars_data <- transpose(single_combn(data))
	colnames(t_vars_data) <- c("output", "input")
	t_vars_data %>%
		mutate(formula = paste(t_vars_data$output, "~", t_vars_data$input))
}

#' Function converts text format into formula format
#'
#' @param data after passing trans_vars_func()
#' @return list of formulae with one-one pair of each independent variables with the dependent variable
#' @import stats
#' @inheritParams trans_vars_func
#' @export
make_formula_1var <- function(data){
	vars_data <- trans_vars_func(data)
	lst <- c()
	n_predictors <- ncol(data) - 1
	for (i in 1:n_predictors){
		lst[[i]] <- as.formula(vars_data[eval(i), 3])
	}
	lst
}

#' Function writes a logistic regression model
#'
#' @param pkg name of packages as a vector
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
#' @inheritParams make_formula_1var
#' @export
glm_model_1var <- function(data){
	formula_lst <- make_formula_1var(data)
	output_lst <- list()
	for (i in 1:length(formula_lst)){
		output_lst[[i]] <- summary(glm(eval(formula_lst[[i]]), data = eval(data), family = binomial ))$coefficients
	}
	output_lst
}

#' Function to print logistic regression model using formula
#'
#' @param formula output ~ input
#' @return Logistic regression model
#' @export
glm_model <- function(data, formula){
	glm(eval(formula), data = eval(data), family = binomial )
}

#' Function takes logistic model and converts the coeficients in odds scale.
#'
#' Logistic regression model's coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable. So covert log odds to odds, and also compute 95% confidence interval using standard errors.
#' @param model logistic regression model
#' @return odds of coefficients of independent variables in logistic model
#' @export
odds_coef <- function(model){
	coef_predictor <- coefficients(model)
	odds_predictor <- exp(coef_predictor)
	confint_odds_predictor <- exp(confint(model))
	cbind(OddsRatio = round(odds_predictor, 4), round(confint_odds_predictor, 4))
}

#' Function takes logistic model and converts the coeficients in probability scale
#'
#' compute log odds to odds and confidence interval, then compute values of coefficients in probability scale
#' @param model logistic regression model
#' @return probability of coefficients of independent variables in logistic model
#' @export
log_odds_prob <- function(model){
	coef_predictor <- coefficients(model)
	odds_predictor <- exp(coef_predictor)
	confint_odds_predictor <- exp(confint(model))
	cbind(Probability = round(odds_predictor/(1 + odds_predictor), 4), round(confint_odds_predictor/(1 + confint_odds_predictor), 4))
}

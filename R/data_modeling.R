#' This function plots the correlationship between variables, insignificant correlations are leaved blank
#'
#' @param data
#' @return Correlationship between numeric variables
#' @export
plot_rcor <- function( data ) {
	numeric_data <- data[, lapply(data, is.numeric) == TRUE, with = FALSE]
	var_cor <- rcorr( as.matrix( numeric_data ) )
	corrplot( var_cor$r, type = "lower", order="hclust", p.mat = var_cor$P, sig.level = 0.01, insig = "blank")
}

#' This function produces all combinations of variables with the dependent variable
#'
#' @param data
#' @return keep the dependent variable as the first column
#' @export
single_combn <- function(data) {
	vec <- names(data)
	pos <- 1
	as.data.frame(rbind(vec[pos], vec[-pos]))
}

#' This function loads packages without warnings
#'
#' @param pkg name of packages as a vector
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return get row and colnames in order,
#' @export
trans_vars_func <- function(data){
	t_vars_data <- transpose(single_combn(data))
	colnames(t_vars_data) <- c("output", "input")
	t_vars_data %>%
		mutate(formula = paste(t_vars_data$output, "~", t_vars_data$input))
}


#' This function loads packages without warnings
#'
#' @param pkg name of packages as a vector
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
#' @export
make_formula <- function(data, index){
	as.formula(data[eval(index), 3])
}

#' This function write a logistic regression model
#'
#' @param pkg name of packages as a vector
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
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

#' This function loads packages without warnings
#'
#' @param pkg name of packages as a vector
#' @param join_summary uses summary_numbers and summary_dates to give a tidy summary
#' @return distinct numbers, and basic statistics for each variable in the data
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
glm_model <- function(formula){
	summary(glm(eval(formula), data = eval(data), family = binomial ))$coefficients
}

#' Function takes logistic model and converts the coeficients in odds scale. Logistic regression model's coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable.
#'
#' @param model logistic regression model
#' @return odds of coefficients of independent variables in logistic model
#' @export
odds_coef <- function(model){
	coef_predictor <- coef(model)
	#' log odds to odds
	odds_predictor <- exp(coef_predictor)
	#' confidence interval of predictors
	confint_odds_predictor <- exp(confint(model))
	#' odds ratios and 95% CI using standard errors
	cbind(OddsRatio = round(odds_predictor, 4), round(confint_odds_predictor, 4))
}

#' Function takes logistic model and converts the coeficients in probability scale
#'
#' @param model logistic regression model
#' @return probability of coefficients of independent variables in logistic model
#' @export
log_odds_prob <- function(model){
	coef_predictor <- coef(model)
	#' log odds to odds
	odds_predictor <- exp(coef_predictor)
	#' confidence interval of predictors
	confint_odds_predictor <- exp(confint(model))
	#' probability and 95% CI using standard errors
	cbind(Probability = round(odds_predictor/(1 + odds_predictor), 4), round(confint_odds_predictor/(1 + confint_odds_predictor), 4))
}

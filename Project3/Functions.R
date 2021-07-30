###--------------------------------------------------------------------------###
# Function: Prof. David Shilane, Lecture 04, APAN S5902, Columbia University SPS
###--------------------------------------------------------------------------###

crossect.adh <- function(adh_data, timeframe, rx_start, rx_stop,
                         patient_id){

    require(data.table)
    setDT(adh_data)
    setorderv(x = adh_data, cols = c(patient_id, rx_start))
    cs.ad <- adh_data[get(rx_stop) > timeframe][, .SD[1], by = patient_id]
    return(cs.ad);

}
###--------------------------------------------------------------------------###
# Function: Prof. David Shilane, Lecture 04, APAN S5902, Columbia University SPS
###--------------------------------------------------------------------------###

round_numerics <- function(x, digits){

    if (is.numeric(x)) {
        x <- round(x = x, digits = digits)
    }
    return(x)

}

###--------------------------------------------------------------------------###

create_table <- function(values, digits = 2){

  tabled_values <- table(values);
  percents <- 100*tabled_values/(sum(tabled_values));
  rounded_table <- round_numerics(x = percents, digits = digits)
  return(rounded_table)

}

###--------------------------------------------------------------------------###

ovDiff <- function(q4_1, ovEngVar1, ovEngVar2){

  if(ovEngVar1 == 'Awareness'){

    if(ovEngVar2 == 'Consideration'){
      ovDiff <- abs(q4_1$Awareness - q4_1$Consideration)
    }

    else if(ovEngVar2 == 'Consumption'){
      ovDiff <- abs(q4_1$Awareness - q4_1$Consumption)
    }

    else if(ovEngVar2 == 'Satisfaction'){
      ovDiff <- abs(q4_1$Awareness - q4_1$Satisfaction)
    }

    else if(ovEngVar2 == 'Advocacy'){
      ovDiff <- abs(q4_1$Awareness - q4_1$Advocacy)
    }

  }

  else if(ovEngVar1 == 'Consideration'){

    if(ovEngVar2 == 'Consumption'){
      ovDiff = abs(q4_1$Consideration - q4_1$Consumption)
    }

    else if(ovEngVar2 == 'Satisfaction'){
      ovDiff = abs(q4_1$Consideration - q4_1$Satisfaction)
    }

    else if(ovEngVar2 == 'Advocacy'){
      ovDiff= abs(q4_1$Consideration - q4_1$Advocacy)
    }

  }

  else if(ovEngVar1 == 'Consumption'){

    if(ovEngVar2 == 'Satisfaction'){
      ovDiff = abs(q4_1$Consumption - q4_1$Satisfaction)
    }

    else if(ovEngVar2 == 'Advocacy'){
      ovDiff = abs(q4_1$Consumption - q4_1$Advocacy)
    }

  }

  else if(ovEngVar1 == 'Satisfaction'){

    if(ovEngVar2 == 'Advocacy'){
      ovDiff = abs(q4_1$Satisfaction - q4_1$Advocacy)
    }

  }

  else if((ovEngVar1 == 'Awareness' & ovEngVar2 == 'Awareness') |
          (ovEngVar1 == 'Consideration' & ovEngVar2 == 'Consideration' ) |
          (ovEngVar1 == 'Consumption' & ovEngVar2 == 'Consumption') |
          (ovEngVar1 == 'Satisfaction' & ovEngVar2 == 'Satisfaction') |
          (ovEngVar1 == 'Advocacy' & ovEngVar2 == 'Advocacy' )){

    ovDiff <- 0

  }

  ovDiffs <- ovDiff

  return(ovDiffs)

}

###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

round_table <- function(values, digits = 2){

    tabled_values <- table(values);
    percents <- 100 * tabled_values/(sum(tabled_values));
    rounded_table <- round_numerics(x = percents, digits = digits);

    return(rounded_table)

}
###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

engage <- function(values, output_values, input_values, model_type){

  eng <- fit(values = values, output_values = output_values,
    input_values = input_values, model_type = model_type);

  return(eng);
}

###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

fit <- function(values, output_values, input_values, model_type, digits = 3){

  final_formula <- create.formula(outcome.name = output_values,  input.names =
    input_values, dat = data, reduce = TRUE);

  if(model_type == 'logistic'){
    mod <- glm(formula = final_formula, family = 'binomial', values = data);
    mod_summary <- logistic_regression_summary(glm_mod = mod, digits = digits);
  }

  if(model_type == 'linear'){
    mod <- lm(formula = final_formula, values = data);
    mod_summary <- linear_regression_summary(lm_mod = mod, digits = digits);
  }

  mod_summary_rounded <- mod_summary[, lapply(X = .SD, FUN = 'round_numerics',
    digits = digits)];

  return(mod_summary_rounded);

}

###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

logistic_regression_summary <- function(glm_mod, digits = 3, alpha = 0.05){

  glm_coefs <- as.data.table(summary(glm_mod)$coefficients, keep.rownames = TRUE);
  setnames(x = glm_coefs, old = 'rn', new = 'Variable');
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1);
  glm_coefs[, Odds.Ratio := exp(Estimate)];
  glm_coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)];
  glm_coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)];

  return(glm_coefs[])

}

###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

linear_regression_summary <- function(lm_mod, digits = 3, alpha = 0.05){

  lm_coefs <- as.data.table(summary(lm_mod)$coefficients, keep.rownames = TRUE);
  setnames(x = lm_coefs, old = 'rn', new = 'Variable');

  z <- qnorm(p = 1 - alpha/2, mean = 0, sd = 1);

  lm_coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`];
  lm_coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`];

  return(lm_coefs)

}

###--------------------------------------------------------------------------###
# Function: Haotian Yue, APAN S5902, Columbia University SPS (Current Student)
###--------------------------------------------------------------------------###

agg <- function(agg){

  mean_agg <- mean(get(input$aeEngState)[-agg],na.rm = TRUE);

  return(mean_agg)

}

###-----------------------------------EOF------------------------------------###

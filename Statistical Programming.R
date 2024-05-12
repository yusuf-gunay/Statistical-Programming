# Yusuf G??nay

DatasetNA <- read.table(file.choose(),header = T,col.names = c("IdNo","Group","Gender","Var1","Var2","Var3","Var4","Var5","Var6","Var7","Var8"),na.strings = "NA",
                        colClasses = c("numeric","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), sep = " ")
DatasetNA
#1
#a

#number of obs

number_of_obs <- function(x) {
  non_missing_count <- sum(!is.na(x))
  return(non_missing_count)
}

number_of_obs(DatasetNA$Var1)

#median

calculate_median <- function(x){
  size <- length(x)
  x_sort <- sort(x)
  if(size %% 2 == 0){
    med <- (x_sort[size/2] + x_sort[size/2 + 1]) / 2
  } else{
    med <- x_sort[(size+1) / 2]
  }
  return(med)
}

calculate_median(DatasetNA$Var1)

#standard_deviation

calculate_standard_deviation <- function(x) {
  x <- x[!is.na(x)] 
  n <- length(x)
  
  if (n < 2) {
    return(NA)  
  }
  
  mean_val <- mean(x)
  sum_squared_diff <- 0
  
  for (i in 1:n) {
    diff <- x[i] - mean_val
    sum_squared_diff <- sum_squared_diff + diff^2
  }
  
  standard_deviation <- sqrt(sum_squared_diff / (n - 1))
  
  return(standard_deviation)
}

calculate_standard_deviation(DatasetNA$Var1)

#min

calculate_min <- function(x) {
  if (length(x) == 0) {
    return(NULL) 
  }
  
  min_val <- x[1]  
  
  for (i in 2:length(x)) {
    if (!is.na(x[i]) && x[i] < min_val) {
      min_val <- x[i]  
    }
  }
  
  return(min_val)
}

calculate_min(DatasetNA$Var1)

#max

calculate_max <- function(x) {
  if (length(x) == 0) {
    return(NULL)  
  }
  
  max_val <- x[1] 
  
  for (i in 2:length(x)) {
    if (!is.na(x[i]) && x[i] > max_val) {
      max_val <- x[i]  
    }
  }
  
  return(max_val)
}

calculate_max(DatasetNA$Var1)

#range

calculate_range <- function(x) {
  if (length(x) == 0) {
    return(NULL)  
  }
  
  min_val <- x[1]  
  max_val <- x[1]  
  
  for (i in 2:length(x)) {
    if (!is.na(x[i])) {
      if (x[i] < min_val) {
        min_val <- x[i]  
      }
      if (x[i] > max_val) {
        max_val <- x[i]  
      }
    }
  }
  
  return(max_val - min_val)
}

calculate_range(DatasetNA$Var1)

#sum

calculate_sum <- function(vector) {
  result <- 0  
  
  for (element in vector) {
    if (!is.na(element)) {
      result <- result + element  
    }
  }
  
  return(result)
}

calculate_sum(DatasetNA$Var1)

#mean

calculate_mean <- function(vector) {
  n <- length(vector)  
  sum_val <- 0      
  count <- 0        
  
  for (element in vector) {
    if (!is.na(element)) {
      sum_val <- sum_val + element 
      count <- count + 1          
    }
  }
  
  if (count > 0) {
    mean_val <- sum_val / count  
    return(mean_val)
  } else {
    return(NA)  
  }
}

calculate_mean(DatasetNA$Var1)

#sum of squares

calculate_sum_of_squares <- function(vector) {
  sum_squares <- 0 
  
  for (element in vector) {
    if (!is.na(element)) {
      sum_squares <- sum_squares + element^2  
    }
  }
  
  return(sum_squares)
}

calculate_sum_of_squares(DatasetNA$Var1)

#variance

calculate_variance <- function(vector) {
  n <- length(vector)  
  mean_val <- mean(vector,na.rm = TRUE)  
  
  sum_squares_diff <- 0 
  
  for (element in vector) {
    if (!is.na(element)) {
      diff <- element - mean_val  
      sum_squares_diff <- sum_squares_diff + diff^2  
    }
  }
  
  variance <- sum_squares_diff / (n - sum(is.na(vector)))  
  return(variance)
}


calculate_variance(DatasetNA$Var1)

#b

#cross-products

calculate_cross_product <- function(vector1, vector2) {
  n <- length(vector1) 
  
  if (length(vector1) != length(vector2)) {
    stop("Vectors must have the same length")
  }
  
  cross_product <- 0  
  
  for (i in 1:n) {
    if (!is.na(vector1[i]) && !is.na(vector2[i])) {
      cross_product <- cross_product + vector1[i] * vector2[i]  
    }
  }
  
  return(cross_product)
}


calculate_cross_product(DatasetNA$Var1,DatasetNA$Var2)

#covariance

calculate_covariance <- function(vector1, vector2) {
  n <- length(vector1) 
  
  if (length(vector1) != length(vector2)) {
    stop("Vectors must have the same length")
  }
  
  mean_vector1 <- mean(vector1, na.rm = TRUE) 
  mean_vector2 <- mean(vector2, na.rm = TRUE)  
  
  covariance <- 0  
  
  for (i in 1:n) {
    if (!is.na(vector1[i]) && !is.na(vector2[i])) {
      covariance <- covariance + (vector1[i] - mean_vector1) * (vector2[i] - mean_vector2)  
    }
  }
  
  covariance <- covariance / (n - sum(is.na(vector1) | is.na(vector2))) 
  
  return(covariance)
}

calculate_covariance(DatasetNA$Var1,DatasetNA$Var2)

#correlation

calculate_correlation <- function(vector1, vector2) {
  n <- length(vector1) 
  
  if (length(vector1) != length(vector2)) {
    stop("Vectors must have the same length")
  }
  
  mean_vector1 <- mean(vector1, na.rm = TRUE)  
  mean_vector2 <- mean(vector2, na.rm = TRUE)  
  
  numerator <- 0  
  denominator1 <- 0  
  denominator2 <- 0 
  
  for (i in 1:n) {
    if (!is.na(vector1[i]) && !is.na(vector2[i])) {
      deviation1 <- vector1[i] - mean_vector1  
      deviation2 <- vector2[i] - mean_vector2  
      
      numerator <- numerator + (deviation1 * deviation2) 
      denominator1 <- denominator1 + (deviation1^2)  
      denominator2 <- denominator2 + (deviation2^2) 
    }
  }
  
  denominator <- sqrt(denominator1 * denominator2)  
  
  correlation <- numerator / denominator 
  
  return(correlation)
}

calculate_correlation(DatasetNA$Var1,DatasetNA$Var2)

#Q2

DatasetNA$Group <- as.factor(DatasetNA$Group)
DatasetNA$Gender <- as.factor(DatasetNA$Gender)


number_of_obs(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
number_of_obs((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
number_of_obs(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_max(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_max((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_max(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_min(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_min(DatasetNA$Var1[DatasetNA$Gender=="Female"])
calculate_min(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_median(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_median(DatasetNA$Var1[DatasetNA$Gender=="Female"])
calculate_median(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_mean(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_mean((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_mean(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_standard_deviation(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_standard_deviation((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_standard_deviation(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_range(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_range((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_range(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_sum(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_sum((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_sum(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_sum_of_squares(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_sum_of_squares((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_sum_of_squares(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_variance(DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])
calculate_variance((DatasetNA$Var1[DatasetNA$Gender=="Female"]))
calculate_variance(DatasetNA$Var1[DatasetNA$Group=="Group1"])


calculate_correlation((DatasetNA$Var1[DatasetNA$Gender=="Female"]),(DatasetNA$Var2[DatasetNA$Gender=="Female"]))
calculate_correlation((DatasetNA$Var1[DatasetNA$Group=="Group1"]),(DatasetNA$Var2[DatasetNA$Group=="Group1"]))
calculate_correlation((DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"]),DatasetNA$Var2[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])


calculate_covariance((DatasetNA$Var1[DatasetNA$Gender=="Female"]),(DatasetNA$Var2[DatasetNA$Gender=="Female"]))
calculate_covariance((DatasetNA$Var1[DatasetNA$Group=="Group1"]),(DatasetNA$Var2[DatasetNA$Group=="Group1"]))
calculate_covariance((DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"]),DatasetNA$Var2[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])


calculate_cross_product((DatasetNA$Var1[DatasetNA$Gender=="Female"]),(DatasetNA$Var2[DatasetNA$Gender=="Female"]))
calculate_cross_product((DatasetNA$Var1[DatasetNA$Group=="Group1"]),(DatasetNA$Var2[DatasetNA$Group=="Group1"]))
calculate_cross_product((DatasetNA$Var1[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"]),DatasetNA$Var2[DatasetNA$Gender=="Female"&DatasetNA$Group=="Group1"])




#Q3

datam <- data.frame(DatasetNA$Var1,DatasetNA$Var2 , DatasetNA$Var3)


scatterplot_func <- function(data, x_variable, y_variable) {
  
  
  if (!all(c(x_variable, y_variable) %in% names(data))) {
    stop("Invalid variables.")
  }
  
  
  x <- data[[x_variable]]
  y <- data[[y_variable]]
  
  
  plot(x, y, xlab = x_variable, ylab = y_variable)
  
  
  points(x, y, pch = 19, cex = 0.5)
}

scatterplot_func(datam,"DatasetNA.Var1","DatasetNA.Var2")



scatterplot_matrix <- function(data) {
  
  variables <- names(data)
  
  
  num_variables <- length(variables)
  par(mfrow = c(num_variables, num_variables))
  
  
  for (i in 1:num_variables) {
    for (j in 1:num_variables) {
      
      x <- variables[i]
      y <- variables[j]
      
      
      scatterplot_func(data, x, y)
    }
  }
}


scatterplot_matrix(datam)



#Q4

scale_variables <- function(data, vars) {
  scaled_data <- data
  
  for (var in vars) {
    column <- scaled_data[[var]]
    mean_val <- mean(column, na.rm = TRUE)
    sd_val <- sd(column, na.rm = TRUE)
    scaled_column <- (column - mean_val) / sd_val
    scaled_data[[var]] <- scaled_column
  }
  
  return(scaled_data)
}

scale_variables(DatasetNA, "Var1")






















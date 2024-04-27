# Example data
x_values <- c("A", "B", "C", "D", "E")  # Categorical values (character array)
y_values <- c(10, 20, 15, 25, 30)       # Numeric values

# Create bar plot
barplot(y_values, names.arg = x_values, col = "blue", main = "Bar Plot", xlab = "X values", ylab = "Y values")

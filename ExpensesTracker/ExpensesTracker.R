# Expenses Tracker

# Create an empty dataframe with column names
expenses <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                     c("Date", "Item", "Quantity", "Amount", "Category"))

# Ask user how many items need to be recorded
NoOfItem <- readline(prompt = "How many items would you like to record? :")

# Recording expenses
for (i in 1:as.numeric(NoOfItem)){
  date <- readline(prompt = "What is the date of purchase? (YYYY-MM-DD) :")
  item <- readline(prompt = "What did you buy? :")
  quantity <- readline(prompt = "How many did you buy? :")
  amount <- readline(prompt = "How much is it? :")
  category <- readline(prompt = "Please choose the appropriate category for the expenses :")
  expenses[nrow(expenses)+1,] <- c(date, item, as.integer(quantity), as.numeric(amount), category)

  if (i == as.numeric(NoOfItem)){
    print("The record is saved successfully.")
    expenses
  }
}



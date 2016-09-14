
# If shiny not installed, install it!
test = try(find.package("shiny"))
if(inherits(test,"try-error")) install.packages("shiny")
library(shiny)

# run the app
runApp(appDir = "Data")

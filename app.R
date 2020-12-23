library(shiny)
#library(datasets)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
#mpgData <- mtcars
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


NGN20vs20 <- read.csv(file = '/home/yehudalab/work/StemCellProject/20vs20_111020/result/NGN20vs20_VOOMnormalized.txt', row.names = 1, sep = '\t')
trait_20vs20 <-  read.table(file = '/home/yehudalab/test/shinyTest/trait_20vs20.txt', stringsAsFactors = F, header = T, sep = '\t')
dim(trait_20vs20)
rownames(trait_20vs20) <- trait_20vs20$sampleID
trait_20vs20 <- trait_20vs20[colnames(NGN20vs20),]


data = cbind(t(NGN20vs20), trait_20vs20)
# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Gene expression level"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Gene:",
                  sort(rownames(NGN20vs20)), multiple = F),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$variable, "~Hcort + CellPlate")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = data,
            outline = input$outliers,
            col = "#75AADB", pch = 19, las=2)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
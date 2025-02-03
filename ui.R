shinyUI(pageWithSidebar(
  headerPanel('Multinomial Logistic Regression Probabilities'),
  sidebarPanel(
    sliderInput('b_1', 'Select coefficient parameter', value = 1, min = -10, max = 10, step = 0.25),
    sliderInput('alpha_1', 'Select threshold for j=1 versus j=2', value = -1, min = -10, max = 10, step = 0.25),
    sliderInput('alpha_2', 'Select threshold for j=2 versus j=3', value = 0, min = -10, max = 10, step = 0.25),
    sliderInput('alpha_3', 'Select threshold for j=3 versus j=4', value = 1, min = -10, max = 10, step = 0.25),
    sliderInput('alpha_4', 'Select threshold for j=4 versus j=5', value = 2, min = -10, max = 10, step = 0.25)),
  mainPanel(
    plotOutput('plot1', width = "600px", height = "600px"),
    plotOutput('plot2', width = "600px", height = "600px"),
    plotOutput('plot3', width = "600px", height = "600px"),
    plotOutput('plot4', width = "600px", height = "600px")
  )
))

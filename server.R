pts <- seq(from = -5, to = 5, length = 700)

shinyServer(function(input, output, session) {

  output$plot1 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_1 <- input$b_1
    alpha_1 <- input$alpha_1
    alpha_2 <- input$alpha_2
    alpha_3 <- input$alpha_3
    alpha_4 <- input$alpha_4

    probs1 <- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs2 <- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts)) - exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs3 <- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts)) - exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts))
    probs4 <- exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts)) - exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts))
    probs5 <- 1 - exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts))
    

    plot(pts, probs1, xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "x", ylab = "Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
         col = "#CC6677", type = "l"#,
         # xlab = "X", ylab = "Prob(Y = 2|X = x)"
         )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    
    
  })
  
  output$plot2 <- renderPlot({
    
    b_1 <- input$b_1
    alpha_1 <- input$alpha_1
    alpha_2 <- input$alpha_2
    alpha_3 <- input$alpha_3
    alpha_4 <- input$alpha_4
    
    probs1 <- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs2 <- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts)) - exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs3 <- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts)) - exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts))
    probs4 <- exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts)) - exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts))
    probs5 <- 1 - exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts))
    
    
    Odds_le1_g1 <- exp(alpha_1 - b_1* pts)
    Odds_le1_g2 <- exp(alpha_2 - b_1* pts)
    Odds_le1_g3 <- exp(alpha_3 - b_1* pts)
    Odds_le1_g4 <- exp(alpha_4 - b_1* pts)
    Odds_le1_g5 <- rep(1, length(pts)) #exp(alpha_1 - b_1* pts)
    
    templower <- min(c(0, Odds_le1_g1, Odds_le1_g2, Odds_le1_g3, Odds_le1_g4, Odds_le1_g5))
    tempupper <- max(c(0, Odds_le1_g1, Odds_le1_g2, Odds_le1_g3, Odds_le1_g4, Odds_le1_g5))
    
    plot(pts, Odds_le1_g1, xlim = c(-5, 5), ylim = c(templower, tempupper), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "w1", ylab = "odds, Pr(Y<=j)/Pr(Y>j)")
    lines(pts, Odds_le1_g2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2,
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, Odds_le1_g3, col = "#DDCC77", type = "l")
    lines(pts, Odds_le1_g4, col = "#117733", type = "l")
    lines(pts, Odds_le1_g5, col = "#332288", type = "l")
    
    
  })
  
  output$plot3 <- renderPlot({
    
    b_1 <- input$b_1
    alpha_1 <- input$alpha_1
    alpha_2 <- input$alpha_2
    alpha_3 <- input$alpha_3
    alpha_4 <- input$alpha_4
    
    probs1 <- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs2 <- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts)) - exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    probs3 <- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts)) - exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts))
    probs4 <- exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts)) - exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts))
    probs5 <- 1 - exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts))
    
    
    logOdds_le1_g1 <- (alpha_1 - b_1* pts)
    logOdds_le1_g2 <- (alpha_2 - b_1* pts)
    logOdds_le1_g3 <- (alpha_3 - b_1* pts)
    logOdds_le1_g4 <- (alpha_4 - b_1* pts)
    logOdds_le1_g5 <- rep(0, length(pts)) #exp(alpha_1 - b_1* pts)
    
    templower <- min(c(0, logOdds_le1_g1, logOdds_le1_g2, logOdds_le1_g3, logOdds_le1_g4, logOdds_le1_g5))
    tempupper <- max(c(0, logOdds_le1_g1, logOdds_le1_g2, logOdds_le1_g3, logOdds_le1_g4, logOdds_le1_g5))
    
    plot(pts, logOdds_le1_g1, xlim = c(-5, 5), ylim = c(templower, tempupper), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "w1", ylab = "log odds, log(Pr(Y<=j)/Pr(Y>j))")
    lines(pts, logOdds_le1_g2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, logOdds_le1_g3, col = "#DDCC77", type = "l")
    lines(pts, logOdds_le1_g4, col = "#117733", type = "l")
    lines(pts, logOdds_le1_g5, col = "#332288", type = "l")
    
    
  })
  
  
  output$plot4 <- renderPlot({
    
    b_1 <- input$b_1
    alpha_1 <- input$alpha_1
    alpha_2 <- input$alpha_2
    alpha_3 <- input$alpha_3
    alpha_4 <- input$alpha_4
    
    F1 <- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    F2 <- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts)) #- exp(alpha_1 - b_1* pts) / (1 + exp(alpha_1 - b_1* pts))
    F3 <- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts)) #- exp(alpha_2 - b_1* pts) / (1 + exp(alpha_2 - b_1* pts))
    F4 <- exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts)) #- exp(alpha_3 - b_1* pts) / (1 + exp(alpha_3 - b_1* pts))
    # F5 <- 1 - exp(alpha_4 - b_1* pts) / (1 + exp(alpha_4 - b_1* pts))
    
    
    PE1 <- - b_1 *(  - F1*(1 - F1)  ) 
    PE2 <-  - b_1 *( F1*(1 - F1)  - F2*(1 - F2)  ) 
    PE3 <-  - b_1 *( F2*(1 - F2)  - F3*(1 - F3)  ) 
    PE4 <-  - b_1 *( F3*(1 - F3)  - F4*(1 - F4)  ) 
    PE5 <-  - b_1 *( F4*(1 - F4)    ) 

    templower <- min(c(0, PE1, PE2, PE3, PE4, PE5))
    tempupper <- max(c(0, PE1, PE2, PE3, PE4, PE5))
    
    plot(pts, PE1, xlim = c(-5, 5), ylim = c(templower, tempupper), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "w1", ylab = "Partial Effects of x on Pr(Y_i=j|X=x)")
    lines(pts, PE2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, PE3, col = "#DDCC77", type = "l")
    lines(pts, PE4, col = "#117733", type = "l")
    lines(pts, PE5, col = "#332288", type = "l")
  })
  
  
})

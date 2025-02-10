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
         xlab = "x", ylab = "Prob(Y = j|X = x)", main = "Ordered Logit Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
         col = "#CC6677", type = "l"#,
         # xlab = "X", ylab = "Prob(Y = 2|X = x)"
         )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    legend(-5, 1, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
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
         xlab = "w1", ylab = "odds, Pr(Y<=j)/Pr(Y>j)", main = "odds, Pr(Y<=j)/Pr(Y>j)")
    lines(pts, Odds_le1_g2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2,
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, Odds_le1_g3, col = "#DDCC77", type = "l")
    lines(pts, Odds_le1_g4, col = "#117733", type = "l")
    lines(pts, Odds_le1_g5, col = "#332288", type = "l")
    legend(-5, tempupper, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
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
         xlab = "w1", ylab = "log odds, log(Pr(Y<=j)/Pr(Y>j))", main = "log odds, log(Pr(Y<=j)/Pr(Y>j))")
    lines(pts, logOdds_le1_g2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, logOdds_le1_g3, col = "#DDCC77", type = "l")
    lines(pts, logOdds_le1_g4, col = "#117733", type = "l")
    lines(pts, logOdds_le1_g5, col = "#332288", type = "l")
    legend(-5, tempupper, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
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
         xlab = "w1", ylab = "Partial Effects of x on Pr(Y_i=j|X=x)", main = "Partial Effects of x on Pr(Y_i=j|X=x)")
    lines(pts, PE2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, PE3, col = "#DDCC77", type = "l")
    lines(pts, PE4, col = "#117733", type = "l")
    lines(pts, PE5, col = "#332288", type = "l")
    legend(-5, tempupper, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
  })
  
  
  
  output$plot5 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_11 <- input$b_11
    b_12 <- input$b_12
    b_13 <- input$b_13
    b_14 <- input$b_14
    # b_15 <- input$b_15
    alpha_1 <- input$alpha_1
    alpha_2 <- input$alpha_2
    alpha_3 <- input$alpha_3
    alpha_4 <- input$alpha_4
    
    probs1 <- exp(alpha_1 - b_11* pts) / (1 + exp(alpha_1 - b_11* pts))
    probs2 <- exp(alpha_2 - b_12* pts) / (1 + exp(alpha_2 - b_12* pts)) - exp(alpha_1 - b_11* pts) / (1 + exp(alpha_1 - b_11* pts))
    probs3 <- exp(alpha_3 - b_13* pts) / (1 + exp(alpha_3 - b_13* pts)) - exp(alpha_2 - b_12* pts) / (1 + exp(alpha_2 - b_12* pts))
    probs4 <- exp(alpha_4 - b_14* pts) / (1 + exp(alpha_4 - b_14* pts)) - exp(alpha_3 - b_13* pts) / (1 + exp(alpha_3 - b_13* pts))
    probs5 <- 1 - exp(alpha_4 - b_14* pts) / (1 + exp(alpha_4 - b_14* pts))
    
    
    minprob <- min(c(0,probs1,probs2,probs3,probs4,probs5))
    
    plot(pts, probs1, xlim = c(-5, 5), ylim = c(minprob, 1), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "x", ylab = "Prob(Y = j|X = x)", main = "GOL Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    legend(-5, 1, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
  })
  
  
  
  
  
  output$plot6 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_1_AC <- input$b_1_AC
    b_02 <- input$b_02
    b_03 <- input$b_03
    b_04 <- input$b_04
    b_05 <- input$b_05
    # b_15 <- input$b_15
    # alpha_1 <- input$alpha_1
    # alpha_2 <- input$alpha_2
    # alpha_3 <- input$alpha_3
    # alpha_4 <- input$alpha_4
    
    expAC_2 <- exp(b_02 + b_1_AC*pts) 
    expAC_3 <- exp(b_02 + b_03 + 2*b_1_AC*pts)
    expAC_4 <- exp(b_02 + b_03 + b_04 + 3*b_1_AC*pts)
    expAC_5 <- exp(b_02 + b_03 + b_04 + b_05 + 4*b_1_AC*pts)
    
    
    probs1 <- 1 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs2 <- expAC_2 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs3 <- expAC_3 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs4 <- expAC_4 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs5 <- expAC_5 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    
    
    minprob <- min(c(0,probs1,probs2,probs3,probs4,probs5))
    
    plot(pts, probs1, xlim = c(-5, 5), ylim = c(minprob, 1), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "x", ylab = "Prob(Y = j|X = x)", main = "Adjacent Categories Logit Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    legend(-5, 1, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
  })
  
  
  output$plot7 <- renderPlot({
    
    ## Evaluate probabilities and plot
    b_1_SL <- input$b_1_SL
    b_02 <- input$b_02
    b_03 <- input$b_03
    b_04 <- input$b_04
    b_05 <- input$b_05

    phi_2 <- input$phi_2
    phi_3 <- input$phi_3
    phi_4 <- input$phi_4
    # b_15 <- input$b_15
    # alpha_1 <- input$alpha_1
    # alpha_2 <- input$alpha_2
    # alpha_3 <- input$alpha_3
    # alpha_4 <- input$alpha_4
    
    expAC_2 <- exp(b_02 + phi_2*b_1_SL*pts) 
    expAC_3 <- exp(b_03 + phi_3*b_1_SL*pts)
    expAC_4 <- exp(b_04 + phi_4*b_1_SL*pts)
    expAC_5 <- exp(b_05 + b_1_SL*pts)
    
    
    probs1 <- 1 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs2 <- expAC_2 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs3 <- expAC_3 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs4 <- expAC_4 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    probs5 <- expAC_5 / (1 + expAC_2 + expAC_3 + expAC_4 + expAC_5)
    
    
    minprob <- min(c(0,probs1,probs2,probs3,probs4,probs5))
    
    plot(pts, probs1, xlim = c(-5, 5), ylim = c(minprob, 1), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "x", ylab = "Prob(Y = j|X = x)", main = "Stereotype Logit Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    legend(-5, 1, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
  })
  
  
  
  output$plot8 <- renderPlot({
    
    ## Evaluate probabilities and plot
    # b_1_SL <- input$b_1_SL
    b_01_CR <- input$b_01_CR
    b_02_CR <- input$b_02_CR
    b_03_CR <- input$b_03_CR
    b_04_CR <- input$b_04_CR
    # b_05_CR <- input$b_05_CR
    b_11_CR <- input$b_11_CR
    b_12_CR <- input$b_12_CR
    b_13_CR <- input$b_13_CR
    b_14_CR <- input$b_14_CR
    # b_15_CR <- input$b_15_CR
    
    phi_2 <- input$phi_2
    phi_3 <- input$phi_3
    phi_4 <- input$phi_4
    # b_15 <- input$b_15
    # alpha_1 <- input$alpha_1
    # alpha_2 <- input$alpha_2
    # alpha_3 <- input$alpha_3
    # alpha_4 <- input$alpha_4
    
    expCR_1 <- exp(b_01_CR + b_11_CR*pts) 
    expCR_2 <- exp(b_02_CR + b_12_CR*pts) 
    expCR_3 <- exp(b_03_CR + b_13_CR*pts)
    expCR_4 <- exp(b_04_CR + b_14_CR*pts)
    
    Prj1_jge1 <- expCR_1/(1 + expCR_1)
    Prj2_jge2 <- expCR_2/(1 + expCR_2)
    Prj3_jge3 <- expCR_3/(1 + expCR_3)
    Prj4_jge4 <- expCR_4/(1 + expCR_4)
    # Prj5_jge5 <- expCR_5/(1 + expCR_5)
    
    probs1 <- Prj1_jge1
    probs2 <- Prj2_jge2 * (1 - Prj1_jge1)
    probs3 <- Prj3_jge3 * (1 - Prj2_jge2) * (1 - Prj1_jge1)
    probs4 <-  Prj4_jge4 * (1 - Prj3_jge3) * (1 - Prj2_jge2) * (1 - Prj1_jge1)
    probs5 <- 1 - probs1 - probs2 - probs3 - probs4
    
    
    minprob <- min(c(0,probs1,probs2,probs3,probs4,probs5))
    
    plot(pts, probs1, xlim = c(-5, 5), ylim = c(minprob, 1), lwd = 2, col = "#88CCEE", type = "l",
         xlab = "x", ylab = "Prob(Y = j|X = x)", main = "Continuation Ratio Logit Prob(Y = j|X = x)")
    lines(pts, probs2, #xlim = c(-5, 5), ylim = c(0, 1), lwd = 2, 
          col = "#CC6677", type = "l"#,
          # xlab = "X", ylab = "Prob(Y = 2|X = x)"
    )
    lines(pts, probs3, col = "#DDCC77", type = "l")
    lines(pts, probs4, col = "#117733", type = "l")
    lines(pts, probs5, col = "#332288", type = "l")
    legend(-5, 1, legend=c("j = 1", "j = 2","j = 3","j = 4","j = 5"),
           col=c("#88CCEE", "#CC6677","#DDCC77","#117733","#332288"), lty= c(1,1,1,1,1), cex=0.8,
           box.lty=2, box.lwd=2, box.col="green")
    
  })
  
})


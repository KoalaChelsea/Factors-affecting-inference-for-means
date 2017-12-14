library(shiny)
library(ggplot2)
library(mosaic)
library(data.table)
library(truncnorm)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$design = renderUI({
    if(input$designcheckbox)
    {
      h4("A researcher plans to take a random sample of size n students to do a survey about their experience on the SAT math test. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the SAT math scores of the students in her study and compares it to the mean of 508 for the population of all seniors in the U.S. This app shows  how confidence intervals of that type would come out when there is no bias.")
    }
  })
  
  #population mean plot with true mean
  output$popMean  = renderPlot({
    
    test <- read.table(textConnection("score precentage
                    200-290 3.5
                    300-390 14.3
                    400-490 29.8
                    500-590 28.2
                    600-690 17.0
                    700-800 7.1"),header=TRUE,stringsAsFactors=FALSE)
    midval <- sapply(strsplit(test$score,"-"),function(x) mean(as.numeric(x)))
    # hist(rep(midval,test$precentage),prob=TRUE,breaks=seq(200, 800, by = 100), col="#6495ED",
    #      xlab="Socre", 
    #      ylab = "% percentage in total population",
    #      main="Histogram of SAT Math Score with Normal Curve")
    # 
    df <- data.frame(X = c(rep(midval,test$precentage)))
    ggplot(df, aes(x = X)) + 
      geom_histogram(breaks=seq(200, 800, by = 100), aes(y = ..density..), color="#0000b2", fill="#7f7fff") + 
      stat_function(fun=dnorm,xlim = c(200,800), args=list(mean=508, sd=121), size = 1)+
      geom_vline(xintercept = 508, color = "forestgreen", size = 1.5)+
        labs(
          title = paste0("Population (μ = 508, σ = 121, N = 1637589)"),
          x ="true mean in green color")+
      scale_x_continuous(breaks=c(200,300,400,508,600,700,800))+
      scale_y_continuous(labels = percent_format())
  })
      
  
  
  #Calculating alpha by the confidence level input
  alpha <- reactive({
    (1 - input$level) / 2
  })
  
  #Updating Sample Size
  N <- reactive({
    as.integer(input$nsamp)
  })
  
  
  #generate 50 new sample
  Data <- reactive({
    input$new
      data.frame(
        x =
          do.call(
            paste0("rtruncnorm"),
            c(list(n = as.integer(input$nsamp) * 50), list(a=200, b=800, mean=508,sd=121)))
      ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
   
  })
  
  #calculate the interval
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        sampleMean = mean(x),
        lowerbound = sampleMean + qnorm(alpha()) * 121 / sqrt(N()),
        upperbound = sampleMean - qnorm(alpha()) * 121 / sqrt(N()),
        cover = (lowerbound < 508) & (508 < upperbound) ) %>%
      ungroup()
  })
  
  
  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  
  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  #print the CIplot
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
            message = "Please input samle size")
      )
    
    validate(
      need(input$nsamp >=30,
           message = "Please input samle size larger than 30")
    )
    
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(x=idx, ymin = lowerbound, ymax = upperbound, y = sampleMean, colour = cover,
            alpha = idx == selectedSample(),
            size = idx == selectedSample()
        )) +
      geom_hline(yintercept = 508, size = 2, colour = "forestgreen", alpha = 0.5) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7), guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"), guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
      lims(y = c(300, 700)) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the mean"),
           x = "50 samples are generated every time",y="true mean in green color") +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  output$sampMean<- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    validate(
      need(input$nsamp >=30,
           message = "Please input samle size larger than 30")
    )
    ggplot( data = OneSample()) +
      geom_histogram( aes(x = x), bins = 15,
                      fill = OneSampleColor(), alpha=0.5) +
      lims(x = c(200,800)) +
      geom_vline(xintercept = mean(OneSample()$x, color = "navy"), size = 1, alpha = 0.7) +
      geom_vline(xintercept = 508, color = "forestgreen", size = 1) +
      labs(title = paste("Sample (mean = ",
                         round(mean(OneSample()$x), 2), ", s = ",
                         round(sd(OneSample()$x), 2), ")"),
           x="")
  })
  
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })
  
  
  observeEvent( c( input$A, input$B, input$n, input$level),
                { rate$cover <- sum(Intervals()$cover); rate$total <- nrow(Intervals()) }
  )
  
  # text messages
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value. And coverage rate is ",
          round(100 *  sum(Intervals()$cover)/ nrow(Intervals()), 2),
          "% (",  rate$total, " samples)")
  })
  ############################################################
  ############################################################
  
  #Calculating alpha
  zalpha <- reactive({
    (1 - input$zlevel) / 2
  })
  
  zlowerbound <- reactive({
    qnorm(zalpha())
  })
  
  zupperbound <- reactive({
    -qnorm(zalpha())
  })
  
  
  output$zplot = renderPlot({
    
    # draw the normal curve
    curve(dnorm(x, mean = 0, sd = 1), xlim=c(-3,3),xaxt = "n", main="Normal Distribution Plot (Mean = 0, StDev = 1)")
    cord.x <- c(zlowerbound(),seq(zlowerbound(),zupperbound(),0.01),zupperbound())
    cord.y <- c(0,dnorm(seq(zlowerbound(),zupperbound(),0.01)),0)
    
    polygon(cord.x, cord.y, col='skyblue')
    axis(side=1,at=round(c(zlowerbound(),zupperbound()),3))
    
  })
  
  output$feedback <- renderText({
    input$submit
    isolate({
      validate(
        need(input$question1 == 1.645 || input$question1 == 1.65 || input$question1 == 1.6 || input$question1 == 2, 'Question 1 is not correctly answered.'),
        need(input$question2 == '1.960' || input$question2 == 1.96 || input$question2 == 2.0, 'Question 2 is not correctly answered.'),
        need(input$question3 == '2.576' || input$question3 == 2.58 || input$question3 == 2.6 || input$question3 == 3, 'Question 3 is not correctly answered.'),
        need(input$question4 == 'y', 'Question 4 is not correctly answered.')
      )
      paste("All correct. Great Job!")
    })
  })
  
  ####################################################################
  #####################################################################
  
  # output$matrixScore <- renderTable({
  #   
  #  
  #   testtakers <- c(762247, 875342)
  #   meanScore <- c(524,494)
  #   sdScore <- c(126,116)
  #   groupScore <- c(testtakers, meanScore, sdScore)
  #   
  #   matrixfinal <- matrix(groupScore, nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(rownames = c("n", "μ", "σ"), c("Male","Female")))
  #   matrixfinal
  # })
  
  output$testdesign = renderUI({
    if(input$testdesigncheckbox)
    {
      h4("A researcher wants to sample a group of n male and n female students about their experiences with the SAT writing test.  Although the average SAT writing score for females is 12 higher than for males, a critic believes her sampling technique would provide a sample of students with a mean (µ) that did not depend on gender (the null hypothesis). The researcher uses her samples to conduct a test of that null hypothesis and this test shows how that test would behave when the sampling is really unbiased and the females have a mean that is 12 higher.")
    }
  })
  
  #Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })

  #Updating Sample Size
  maleN <- reactive({
    as.integer(input$nSamp)
  })
  
  femaleN <- reactive({
    as.integer(input$nSamp)
  })

  standardError <- reactive({
    sqrt(118^2/(maleN())+113^2/(femaleN()))
    
  })
  
  #population mean plot with true diffmean
  output$dpopMean  = renderPlot({
    maledata <- read.table(textConnection("score precentage
                                      200-290 2.5
                                      300-390 9.4
                                      400-490 15.4
                                      500-590 11.7
                                      600-690 5.7
                                      700-800 1.8"),header=TRUE,stringsAsFactors=FALSE)
    malemid <- sapply(strsplit(maledata$score,"-"),function(x) mean(as.numeric(x)))
    maledf <- data.frame(S = c(rep(malemid,maledata$precentage)))
    
    femaledata <- read.table(textConnection("score precentage
                                          200-290 1.8
                                          300-390 9.5
                                          400-490 18.1
                                         500-590 14.7
                                          600-690 7.1
                                          700-800 2.2"),header=TRUE,stringsAsFactors=FALSE)
    femalemid <- sapply(strsplit(femaledata$score,"-"),function(x) mean(as.numeric(x)))
    femaledf <- data.frame(S = c(rep(femalemid,femaledata$precentage)))
  #   ggplot(df, aes(x = X)) + geom_histogram(breaks=seq(200, 800, by = 100), aes(y = ..density..,fill=..count..),binwidth=100) + 
  #     stat_function(fun=dnorm, color="red", args=list(mean=508, sd=121))+
  #     geom_vline(xintercept = 508, color = "forestgreen", size = 2)+
  #     labs(
  #       title = paste0("Population (μ = 508, σ = 121, n = 1637589)"),
  #       x ="true mean in green color")+
  #     scale_x_continuous(breaks=c(200,300,400,508,600,700,800))
  # })
  
    #Now, combine your two dataframes into one.  First make a new column in each.
    maledf$Gender <- 'Male'
    femaledf$Gender <- 'Female'
    
    #and combine into your new data frame
    genderdf <- rbind(femaledf, maledf)
    
    #now make your lovely plot
    #ggplot(Gender, aes(m,f, fill = gender)) + geom_density(alpha = 0.2)
    ggplot(genderdf, aes(x=S, fill = Gender))+
      geom_histogram(alpha = 0.3,breaks=seq(200, 800, by = 100), 
                     aes(y = ..density..), position = 'identity')+
      stat_function(fun=dnorm, color="blue",xlim = c(200,800), args=list(mean=475, sd=118), size = 0.8)+
      stat_function(fun=dnorm, color="#FF69B4",xlim = c(200,800), args=list(mean=487, sd=113),size = 0.8)+
      geom_vline(xintercept =475, color = "#4682B4", size = 0.9)+
      geom_vline(xintercept =487, color = "#FF1493", size = 0.9)+
        labs(
          title = paste0("population(male-female) = -12, σ(x(male)-x(female)) = ",round(sqrt(113^2+118^2),3)),
          x ="true mean in blue and pink color")+
      scale_x_continuous(breaks=c(200,300,400,480,600,700,800))
    
  })
  
  MaleS <- reactive({
    input$newSample
    rtruncnorm(n=maleN(), a=200, b=800, mean=475, sd=118)
  })
  
  FemaleS <- reactive({
    input$newSample
    rtruncnorm(n=femaleN(),a=200, b=800, mean=487, sd=113)
  })
  
  Diff <- reactive({
    mean(MaleS()) - mean(FemaleS())
  })
  
  output$sampleDiff  = renderPlot({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    input$newSample
  #generate new sample
  malesample <- data.frame(sampleGen = MaleS())
  femalesample <- data.frame(sampleGen = FemaleS())
    
  #Now, combine your two dataframes into one.  First make a new column in each that will be a variable to identify where they came from later.
  malesample$Gender <- 'Male'
  femalesample$Gender <- 'Female'
 
  #and combine into your new data frame 
  sampleTWO <- rbind(femalesample, malesample)
  ggplot(sampleTWO, aes(sampleGen, fill = Gender)) + 
    geom_density(alpha = 0.2)+
    geom_vline(xintercept = mean(MaleS()), color = "#4682B4", size = 0.9)+
    geom_vline(xintercept = mean(FemaleS()), color = "#FF1493", size = 0.9)+
    labs(
      title = paste0("xbar(male-female) = ",round(Diff(),2),",  σ(xbar(male)-xbar(female)) = ",round(standardError(),3),", male sample = ",maleN(),", female sample = ",femaleN()),
      x ="sample mean in blue and pink color")+
    scale_x_continuous(breaks=c(200,300,400,480,600,700,800))
  
})
  

  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })
  
  output$CItable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$CIcheckbox)
    {
      ctable = matrix(c(dlowerbound(),dupperbound()),nrow=1)
      colnames(ctable) = c("Lower bound","Upper bound")
      #rownames(ctable) = paste((input$dlevel),"% CI",sep="")
      ctable
    }
  })
  
  pvalue <- reactive({
    2*(1-pnorm(abs(zstatistic())))
  })
  
  zstatistic <- reactive({
    Diff()/standardError()
    
  })
  
  output$sampleinfotable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    
      ctable = matrix(c(mean(MaleS()), mean(FemaleS())),nrow=1)  
      #ctable = matrix(c(mean(MaleS()),sd(MaleS()), mean(FemaleS()),sd(FemaleS())),nrow=2)
      colnames(ctable) = c("Male","Female")
      #rownames(ctable) = c("sample mean","sample sd")
      ctable
  })
  
  output$Diffinfo = renderUI({
    validate(
      need(is.numeric(input$nSamp),
           message = "")
    )
    paste("The difference between male and female sample (male minus female) is ", round(Diff(),2))
  })
  
  output$testtable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$testcheckbox)
    {
      ctable = matrix(c(zstatistic(),pvalue()),nrow=1)
      colnames(ctable) = c("z-statistic","p-value")
      #rownames(ctable) = paste((input$dlevel),"% CI",sep="")
      ctable
    }
  })
  
  zstandard <- reactive({
    -qnorm(dalpha())
  })
  
  output$decisionZ = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$decisioncheckbox)
    {
      if(abs(zstatistic()) <= zstandard()){
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," is less than z*score = ",round(zstandard(),3),", the null hypothesis provides a reasonable explanation of the data so we can NOT conclude that males and females have a different average  SAT Writing score when student's are chosen by the researcher's sampling procedure.")
        
      }else{
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," is larger than z*score = ",round(zstandard(),3),", the null hypothesis is not a reasonable explanation of the data so we have evidence that there is a difference between the male and female average SAT Writing score when students are chosen by the researcher's sampling procedure.")
      }
    }
      
  })
  
  output$decisionP = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$decisioncheckbox)
    {
      if(pvalue() >= (2*dalpha())){
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is larger than ",round(2*dalpha(),3),", the null hypothesis provides a reasonable explanation of the data so we can NOT conclude that males and females have a different average  SAT Writing score when student's are chosen by the researcher's sampling procedure.")
      }else{
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is less than ",round(2*dalpha(),3),", the null hypothesis is not a reasonable explanation of the data so we have evidence that there is a difference between the male and female average SAT Writing score when students are chosen by the researcher's sampling procedure.")
      }
    }
  })
  
})

  


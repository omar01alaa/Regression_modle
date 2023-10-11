ui <- navbarPage(
  "Multi&Single Linear Regression Modle", id = "tabs",
  tags$style(HTML("
    body {
      background-color:LightBlue ;
    }

  ")),
  useShinyjs(),
  #titlePanel("3 Inputs and 3 Outputs"),
  tabPanel("Home",
           fluidRow(
             column(5,
                    radioButtons( 
                      label = "Select your modle", 
                      inputId="radio1",
                      choices = c("Single", "Multi")),
                    radioButtons( 
                      label = "Select your prefer way you want to Enter Data", 
                      inputId="radio2",
                      choices = c("URL","manually" )),
                    #######
                    radioButtons( 
                      label = "Select feature (independent)", 
                      inputId="radiof",
                      choices = c("", "")
                    ),
                    sliderInput("value", "Select confidence level:", min = 0, max = 1, step = 0.05,value = 0),
                    
                    textInput("x0", 
                              label = "Enter your value", 
                              value = "", 
                              width = "100%",
                              placeholder = "x0"
                              ),
                    
                    textInput("demo_text2", 
                              label = "Enter your urL", 
                              value = "", 
                              width = "100%",
                              placeholder = "url"),
                    checkboxGroupInput("multifeatuer", "Select your features:", choices = c("")),
                    numericInput("num_inputs", "Number of features", value = 1, min = 1),
                    numericInput("num_row", "Number of your data", value = 1, min = 1),
                    uiOutput("text_inputs"),
                    textInput("manul", 
                              label = "Enter your feature", 
                              value = "", 
                              width = "100%",
                              placeholder = "feature"),
                    textInput("manul2", 
                              label = "Enter your predicted feature", 
                              value = "", 
                              width = "100%",
                              placeholder = "predicted feature")

                    
             ),column(3, radioButtons( 
               label = "Select CI for", 
               inputId="radioconf",
               choices = c("B0", "B1","mean response", "new observation","B")),
               
               
               radioButtons( 
                 label = "Select predicated feature (dependent)", 
                 inputId="radiopf",
                 choices = c("", "")
               )
             ),
             
             column(1,
                    tableOutput("output1")
             ),
             
             column(3,
                    
                    verbatimTextOutput("output2"),
                    verbatimTextOutput("output3"),
                    verbatimTextOutput("output4"),
                    tableOutput("anov"),
                    verbatimTextOutput("f_test"),
                    verbatimTextOutput("inf"),
                    verbatimTextOutput("varcovr"),
                    actionButton(
                      inputId = "count_fav_things",
                      label = "calculat modle",style='
  background-color:DeepSkyBlue ; 
  border: none;
  font-weight: bold;
  color: black;
  padding: 10px 25px;
  text-align: center;
  font-style: italic;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
  -webkit-transition-duration: 0.4s;
  transition-duration: 0.4s;
  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);
'
                    ), actionButton(
                      inputId = "confi",
                      label = "confidence",style='background-color:DeepSkyBlue ; 
  border: none;
  font-weight: bold;
  color: black;
  padding: 10px 25px;
  text-align: center;
  font-style: italic;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
  -webkit-transition-duration: 0.4s;
  transition-duration: 0.4s;
  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);'
                    ),
                    actionButton(
                      inputId = "nova",
                      label = "anova",style='background-color:DeepSkyBlue ; 
  border: none;
  font-weight: bold;
  color: black;
  padding: 10px 25px;
  text-align: center;
  font-style: italic;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
  -webkit-transition-duration: 0.4s;
  transition-duration: 0.4s;
  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);'
                    ),
                    actionButton(
                      inputId = "F_test",
                      label = "F_test",style='background-color:DeepSkyBlue ; 
  border: none;
  font-weight: bold;
  color: black;
  padding: 10px 25px;
  text-align: center;
  font-style: italic;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
  -webkit-transition-duration: 0.4s;
  transition-duration: 0.4s;
  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);
  '        ),actionButton(
    inputId = "info",
    label = "info_multi",style='background-color:DeepSkyBlue ; 
           border: none;
           font-weight: bold;
           color: black;
           padding: 10px 25px;
           text-align: center;
           font-style: italic;
           text-decoration: none;
           display: inline-block;
           font-size: 16px;
           margin: 4px 2px;
           cursor: pointer;
           -webkit-transition-duration: 0.4s;
           transition-duration: 0.4s;
           box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);
           ' )
                    
             )
           )
  ),
  tabPanel("Data", tableOutput("outputpanal")),
  tabPanel("Plot",
           fluidRow(
             column(6,
            HTML("<h4 style='color:blue;font-family:Fantasy'>Plot X and Y.</h4>"),
            plotOutput("plotdata",width = "500px", height = "400px")),
            column(6,HTML("<h4 style='color:blue;font-family:Fantasy'>Plot your model.</h4>"), plotOutput("plotmodle",width = "700px", height = "400px")),
            column(6,HTML("<h4 style='color:blue;font-family:Fantasy'>Plot Error of  model.</h4>"),plotOutput("ploterror",width = "500px", height = "400px")),
            column(6,HTML("<h4 style='color:blue;font-family:Fantasy'>Plot distribution of Error </h4>"),plotOutput("plotdsit",width = "500px", height = "400px")))
           )
       
  
  
)

# Define server
server <- function(input, output,session) {
  # count favourite things
  Data=reactive({
    if(input$radio2=="URL"){
      if(file.exists(input$demo_text2)){
        if(input$radio1=="Single"){
          dt=input$demo_text2
          df_reg<<-read.csv(dt)
          df_reg=data.frame(df_reg)
          col=colnames(df_reg)
          #updateRadioButtons(session, "radiof", choices =col )
          #updateRadioButtons(session, "radiopf", choices =col )
          feat<<-input$radiof
          pfeat<<-input$radiopf
          x<<-df_reg[[feat]]
          y<<-df_reg[[pfeat]]
          data.frame(x,y)
        }else if(input$radio1=="Multi"){
          dt=input$demo_text2
          df_reg<<-read.csv(dt)
          df_reg=data.frame(df_reg)
          col=colnames(df_reg)
          df_reg<<-data.matrix(df_reg)
          feat<<-input$multifeatuer
          pfeat<<-input$radiopf
          x<<-df_reg[,feat]
          y<<-df_reg[,pfeat]
          x<<- cbind(1,x)
          #updateCheckboxGroupInput(session, "multifeatuer", choices = col)
          #updateRadioButtons(session, "radiopf", choices = col)
          data.frame(x,y)
        }
        
        
      }else{ 
        NULL
      }
    } else if(input$radio2=="manually"){
      if(input$radio1=="Multi"){
        x <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
        for(i in 1:input$num_inputs){
          textgenert<<-paste0("feature",i)
          vect<<-strsplit(input[[textgenert]]," ")
          vect<<-as.numeric(vect[[1]])
          if(length(vect)==input$num_row){
            vect<<-na.omit(vect)
            x[,i]<<-vect
          }else{}
          
        }
        x<<-cbind(1,x)
        vec<<-strsplit(input$manul2," ")
        vec<<-as.numeric(vec[[1]])
        if(length(vec)==input$num_row&!(any(is.na(x)))){
          vec=na.omit(vec)
          y <<- matrix(nrow = input$num_row, ncol = 1)
          y[,1]<<-vec
          data.frame(x,y)
        }else{
          paste0("complete your data")
        }
        
      }else{
        x <<- strsplit(input$manul," ")
        x <<- as.numeric(x[[1]])
        x<<-na.omit(x)
        y<<-strsplit(input$manul2," ")
        y <<- as.numeric(y[[1]])
        y<<-na.omit(y)
        if(length(x)==length(y)&length(x)>2){
          df_reg=data.frame(x,y)
          df_reg
        }else{"complete your data"}
        
      }
      
      
    }
    
  })
  filecheck= reactive({
    if(input$radio2=="URL"){
      if(file.exists(input$demo_text2)){
        if(input$radio1=="Single"){
          dt=input$demo_text2
          df_reg<<-read.csv(dt)
          df_reg=data.frame(df_reg)
          col=colnames(df_reg)
          updateRadioButtons(session, "radiof", choices =col )
          updateRadioButtons(session, "radiopf", choices =col )
          
        }else if(input$radio1=="Multi"){
          dt=input$demo_text2
          df_reg<<-read.csv(dt)
          col=colnames(df_reg)
          df_reg<<-data.matrix(df_reg)
          updateCheckboxGroupInput(session, "multifeatuer", choices = col)
          updateRadioButtons(session, "radiopf", choices = col)
          
        }
        
        # df_reg 
      }else{ 
        NULL
      }
    } else if(input$radio2=="manually"){
      
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      if(length(x)==length(y)&length(x)>2){
        df_reg=data.frame(x,y)
        #df_reg
      }else{#"complete your data"
      }
      
    }
    
    
  })
  observeEvent(input$confi,{
    if(file.exists(input$demo_text2)&input$radio1=="Single"&input$radio2=="URL"){
      feat<<-input$radiof
      pfeat<<-input$radiopf
      x<<-df_reg[[feat]]
      y<<-df_reg[[pfeat]]
      Simple_Fitted(x,y)
      if(input$radioconf=="B1"){
        output$output3<-renderPrint(confiadnce_B1(input$value))
      }else if(input$radioconf=="B0"){
        
        output$output3<-renderPrint(confiadnce_B0(input$value))
      }else if(input$radioconf=="mean response"){
        
        output$output3<-renderPrint(confiadnce_Meanrespons(as.double(input$x0),input$value))
        
      }else if(input$radioconf=="new observation"){
        
        output$output3<-renderPrint(confiadnce_newobsevation(as.double(input$x0),input$value))
      }
    }else if(input$radio2=="manually"&input$radio1=="Single"){
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      if(length(x)==length(y)&length(x)>2){
        Simple_Fitted(x,y)
        if(input$radioconf=="B1"){
          
          output$output3<-renderPrint(confiadnce_B1(input$value))
        }else if(input$radioconf=="B0"){
          
          output$output3<-renderPrint(confiadnce_B0(input$value))
        }else if(input$radioconf=="mean response"){
          
          output$output3<-renderPrint(confiadnce_Meanrespons(as.double(input$x0),input$value))
          
        }else if(input$radioconf=="new observation"){
          
          output$output3<-renderPrint(confiadnce_newobsevation(as.double(input$x0),input$value))
        }
      }else{
        output$output3<-renderPrint(paste0("complete your data"))
      }
    }else if(file.exists(input$demo_text2)&input$radio2=="URL"&input$radio1=="Multi"){
      
      if(input$radioconf=="B"){
        df_reg<<-data.matrix(df_reg)
        feat<<-input$multifeatuer
        pfeat<<-input$radiopf
        x<<-df_reg[,feat]
        y<<-df_reg[,pfeat]
        x<<- cbind(1,x)
        Multiple_Fitted(x,y)
        output$output3<-renderPrint(Confidance_Interval_B(input$value,x,y))
        ###########################
      }else if(input$radioconf=="mean response"){
        df_reg<<-data.matrix(df_reg)
        feat<<-input$multifeatuer
        pfeat<<-input$radiopf
        x<<-df_reg[,feat]
        y<<-df_reg[,pfeat]
        x<<- cbind(1,x)
        Multiple_Fitted(x,y)
        vecx0<<-strsplit(input$x0," ")
        vecx0<<-as.numeric(vecx0[[1]])
        vecx0<<-na.omit(vecx0)
        if(length(vecx0)==length(input$multifeatuer)){
          output$output3<-renderPrint(mean_response_m(x,y,vecx0,input$value))
        }else{
          output$output3<-renderPrint(paste0("your data (X0) not complete"))
        }
        
      }else if(input$radioconf=="new observation"){
        df_reg<<-data.matrix(df_reg)
        feat<<-input$multifeatuer
        pfeat<<-input$radiopf
        x<<-df_reg[,feat]
        y<<-df_reg[,pfeat]
        x<<- cbind(1,x)
        Multiple_Fitted(x,y)
        vecx0<<-strsplit(input$x0," ")
        vecx0<<-as.numeric(vecx0[[1]])
        vecx0<<-na.omit(vecx0)
        if(length(vecx0)==length(input$multifeatuer)){
          output$output3<-renderPrint(New_observation_m(x,y,vecx0,input$value))
        }else{
          output$output3<-renderPrint(paste0("your data (X0) not complete"))
        }
        
      }
      
      
    }else if(input$radio2=="manually"&input$radio1=="Multi"){
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      for(i in 1:input$num_inputs){
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }   
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM <<- matrix(nrow = input$num_row, ncol = 1)
        pMM[,1]<<-vec
        #Multiple_Fitted(x,y)
        if(input$radioconf=="B"){
          output$output3<-renderPrint(Confidance_Interval_B(input$value,MM,pMM))
          ############
        }else if(input$radioconf=="mean response"){
          vecx0<<-strsplit(input$x0," ")
          vecx0<<-as.numeric(vecx0[[1]])
          vecx0<<-na.omit(vecx0)
          if(length(vecx0)==input$num_inputs){
            output$output3<-renderPrint(mean_response_m(MM,pMM,vecx0,input$value))
          }else{
            output$output3<-renderPrint(paste0("your data (X0) not complete"))
          }
        }else if(input$radioconf=="new observation"){
          vecx0<<-strsplit(input$x0," ")
          vecx0<<-as.numeric(vecx0[[1]])
          vecx0<<-na.omit(vecx0)
          if(length(vecx0)==input$num_inputs){
            output$output3<-renderPrint(New_observation_m(MM,pMM,vecx0,input$value))
          }else{
            output$output3<-renderPrint(paste0("your data (X0) not complete"))
          }
        }
      }else{
        output$output3<-renderPrint(paste0("complete your data"))
      }
    }
    
    
  }) #for confidence interval
  observeEvent(input$nova,{
    if(input$radio2=="manually"&length(x)==length(y)&length(x)>2){
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      Simple_Fitted(x,y)
      output$anov<-renderTable(anova())
    }else if(input$radio1=="Single"&file.exists(input$demo_text2)&input$radio2=="URL"){
      feat<<-input$radiof
      pfeat<<-input$radiopf
      x<<-df_reg[[feat]]
      y<<-df_reg[[pfeat]]
      Simple_Fitted(x,y)
      output$anov<-renderTable(anova())
    }else if(input$radio1=="Multi"&file.exists(input$demo_text2)&input$radio2=="URL"){
      df_reg<<-data.matrix(df_reg)
      feat<<-input$multifeatuer
      pfeat<<-input$radiopf
      x<<-df_reg[,feat]
      y<<-df_reg[,pfeat]
      x<<- cbind(1,x)
      Multiple_Fitted(x,y)
      output$anov<-renderTable(anova_m())
    }else if(input$radio1=="Multi"&input$radio2=="manually"){
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      pMM <<- matrix(nrow = input$num_row, ncol = 1)
      for(i in 1:input$num_inputs){
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM[,1]<<-vec
        output$anov<-renderTable(anova_m())
      }else{
        output$anov<-renderPrint(paste0("complete your data"))
      }
      
      
    }
    
  })   #for anova table 
  observeEvent(input$radio2, {
    if(input$radio2=="manually"&input$radio1=="Single"){
      hide("demo_text2")
      hide("radiopf")
      hide("radiof")
      show("manul")
      show("manul2")
      hide("text_inputs")
      hide("num_inputs")
      hide("multifeatuer")
      hide("num_row")
      show("plotdata")
      show("plotmodle")
    }else if(input$radio2=="URL"&input$radio1=="Single"){
      show("demo_text2")
      show("radiopf")
      show("radiof")
      hide("manul")
      hide("manul2")
      hide("text_inputs")
      hide("num_inputs")
      hide("multifeatuer")
      hide("num_row")
      show("plotdata")
      show("plotmodle")
    }else if(input$radio2=="manually"&input$radio1=="Multi"){
      hide("demo_text2")
      hide("radiopf")
      hide("radiof")
      hide("manul")
      show("manul2")
      show("text_inputs")
      show("num_inputs")
      hide("multifeatuer")
      show("num_row")
      hide("plotdata")
      hide("plotmodle")
    }else if(input$radio2=="URL"&input$radio1=="Multi"){
      show("demo_text2")
      show("radiopf")
      hide("radiof")
      hide("manul")
      hide("manul2")
      hide("text_inputs")
      hide("num_inputs")
      show("multifeatuer")
      hide("num_row")
      hide("plotdata")
      hide("plotmodle")
    }
    
    
    
  })
  observeEvent(input$radio1, {
    if(input$radio1=="Multi"&input$radio2=="manually"){
      hide("demo_text2")
      hide("radiopf")
      hide("radiof")
      hide("manul")
      show("manul2")
      show("text_inputs")
      show("num_inputs")
      hide("multifeatuer")
      show("num_row")
      hide("plotdata")
      hide("plotmodle")
    }else if(input$radio1=="Multi"&input$radio2=="URL"){
      show("demo_text2")
      show("radiopf")
      hide("radiof")
      hide("manul")
      hide("manul2")
      hide("text_inputs")
      hide("num_inputs")
      show("multifeatuer")
      hide("num_row")
      hide("plotdata")
      hide("plotmodle")
    }else if(input$radio1=="Single"&input$radio2=="URL"){
      show("radiopf")
      show("radiof")
      show("demo_text2")
      hide("text_inputs")
      hide("num_inputs")
      hide("multifeatuer")
      hide("num_row")
      show("plotdata")
      show("plotmodle")
    }else if(input$radio1=="Single"&input$radio2=="manually"){
      hide("radiopf")
      hide("radiof")
      hide("demo_text2")
      show("manul")
      show("manul2")
      hide("text_inputs")
      hide("num_inputs")
      hide("multifeatuer")
      hide("num_row")
      show("plotdata")
      show("plotmodle")
    }
    
    
  })
  observeEvent(input$num_inputs, {
    num_inputs <- input$num_inputs
    output$text_inputs <- renderUI({
      text_inputs <- lapply(1:num_inputs, function(i) {
        textInput(inputId = paste0("feature", i), label = paste0("feature ", i), value = "")
      })
      do.call(tagList, text_inputs)
    })
  }) #to generat many text input
  observeEvent(input$count_fav_things,{
    if(input$radio1=="Single"&file.exists(input$demo_text2)&input$radio2=="URL"){
      feat<<-input$radiof
      pfeat<<-input$radiopf
      x<<-df_reg[[feat]]
      y<<-df_reg[[pfeat]]
      output$output2<-renderPrint(Simple_Fitted(x,y))
    }else if(input$radio1=="Single"&input$radio2=="manually"){
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      output$output2<-renderPrint({
        if(length(x)==length(y)&length(x)>2){
          Simple_Fitted(x,y)
        }else{output$output2<-renderPrint(paste0("complete your data"))}
      })
      
    }else if(input$radio1=="Multi"&file.exists(input$demo_text2)&input$radio2=="URL"){
      df_reg<<-data.matrix(df_reg)
      feat<<-input$multifeatuer
      pfeat<<-input$radiopf
      x<<-df_reg[,feat]
      y<<-df_reg[,pfeat]
      x<<- cbind(1,x) #column for B0 
      output$output2<-renderPrint(Multiple_Fitted(x,y))
    }else if(input$radio1=="Multi"&input$radio2=="manually"){
      pfeat="predicted features "
      feat="features"
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      pMM <<- matrix(nrow = input$num_row, ncol = 1)
      for(i in 1:input$num_inputs){
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM[,1]<<-vec
        output$output2<-renderPrint(Multiple_Fitted(MM,pMM))
      }else{
        output$output2<-renderPrint(paste0("complete your data"))
      }
    }else{        output$output2<-renderPrint(paste0("complete your data"))
}
  }) #  fitted modle
  observe({if(input$radio2=="URL"){
    if (is.null(filecheck())) {
      if(input$radio1=="Multi"){
        col=c(" ", " ")
        updateCheckboxGroupInput(session, "multifeatuer", choices =col )
        updateRadioButtons(session, "radiopf", choices =col )
        
      }else if(input$radio1=="Single"){
        col=c(" ", " ")
        updateRadioButtons(session, "radiof", choices =col )
        updateRadioButtons(session, "radiopf", choices =col )
        
      }
      
    } else {
      filecheck()
    }
  }else{
    filecheck()
  }
  })
  observeEvent(input$F_test,{
    if(input$radio2=="manually"&input$radio1=="Single"){
      feat<<-"selscted feature "
      pfeat<<-"predicted feature"
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      if(length(x)==length(y)&length(x)>2){
        output$f_test<-renderText(f_test(input$value))
      }else{
        output$f_test<-renderText(paste0("complete your data"))
      }
      
    }else if (file.exists(input$demo_text2)&input$radio2=="URL"&input$radio1=="Single"){
      output$f_test<-renderText(f_test(input$value))
    }else if(file.exists(input$demo_text2)&input$radio2=="URL"&input$radio1=="Multi"){
      output$f_test<-renderText(f_test_m(input$value))
    }else if(input$radio2=="manually"&input$radio1=="Multi"){
      pfeat="predicted features "
      feat="features"
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      pMM <<- matrix(nrow = input$num_row, ncol = 1)
      for(i in 1:input$num_inputs){
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM[,1]<<-vec
        output$f_test<-renderText(f_test_m(input$value))
      }else{
        output$f_test<-renderPrint(paste0("complete your data"))
      }
      
      
    }
    
  }) #f_test
  #output$anov<-renderTable(anovfun())
  output$outputpanal <-renderTable({
    if(input$radio2=="URL"){
      if (is.null(filecheck())) {
        if(input$radio1=="Multi"){
          #col=c(" ", " ")
          #updateCheckboxGroupInput(session, "multifeatuer", choices =col )
          #updateRadioButtons(session, "radiopf", choices =col )
          paste0("File path ", input$filepath, " does not exist")
        }else if(input$radio1=="Single"){
          #col=c(" ", " ")
          #updateRadioButtons(session, "radiof", choices =col )
          #updateRadioButtons(session, "radiopf", choices =col )
          paste0("File path ", input$filepath, " does not exist")
        }
        
      } else {
        Data()
      }
    }else{
      Data()
    }
    
  })
  observeEvent(input$count_fav_things,{
    if(input$radio1=="Single"&file.exists(input$demo_text2)&input$radio2=="URL"){
      feat<<-input$radiof
      pfeat<<-input$radiopf
      x<<-df_reg[[feat]]
      y<<-df_reg[[pfeat]]
      Simple_Fitted(x,y)
      output$plotdata <- renderPlot({
        plot(x, y, main = "Scatter Plot", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
      })
      
      output$plotmodle <- renderPlot({
        plot(x, y, main = " Plot of modle", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
        lines(x,y_hat_forall)
        segments(x,y_hat_forall,x,y)
      })
      output$ploterror <- renderPlot({
        plot(ei, y, main = " Plot of Error", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
      })
      output$plotdsit <- renderPlot({
        d <- density(ei)
        plot(d, main="Kernel Density of Error ")
        polygon(d, col="red", border="blue")
      })
      
    }else if(input$radio1=="Multi"&file.exists(input$demo_text2)&input$radio2=="URL"){
      feat<<-input$multifeatuer
      pfeat<<-input$radiopf
      x<<-df_reg[,feat]
      y<<-df_reg[,pfeat]
      x<<- cbind(1,x)
      Multiple_Fitted(x,y)
      output$ploterror <- renderPlot({
        plot(di, y, main = " Plot of Error", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
      })
      output$plotdsit <- renderPlot({
        d <- density(di)
        plot(d, main="Kernel Density of Error ")
        polygon(d, col="red", border="blue")
      })
      
    }else if(input$radio1=="Single"&input$radio2=="manually"){
      x <<- strsplit(input$manul," ")
      x <<- as.numeric(x[[1]])
      x<<-na.omit(x)
      y<<-strsplit(input$manul2," ")
      y <<- as.numeric(y[[1]])
      y<<-na.omit(y)
      if(length(x)==length(y)&length(x)>2){
        Simple_Fitted(x,y)
        output$plotdata <- renderPlot({
          plot(x, y, main = "Scatter Plot", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
        })
        output$plotmodle <- renderPlot({
          plot(x, y, main = " Plot of modle", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
          lines(x,y_hat_forall)
          segments(x,y_hat_forall,x,y)
        })
        output$ploterror <- renderPlot({
          plot(ei, y, main = " Plot of Error", xlab = "X", ylab = "Y", col = "DeepSkyBlue")
        })
        output$plotdsit <- renderPlot({
          d <- density(ei)
          plot(d, main="Kernel Density of Error ")
          polygon(d, col="red", border="blue")
        })
        
      }
    }else{
      output$plotmodle <- renderPlot({paste0("Enter your data")})
      output$plotdata <- renderPlot({paste0("Enter your data")})
      output$ploterror <- renderPlot({paste0("Enter your data")})
      output$plotdsit <- renderPlot({paste0("Enter your data")})
      
    }
  })
  observeEvent(input$count_fav_things,{
    
    if(input$radio1=="Multi"&input$radio2=="manually"){
      pfeat="predicted features "
      feat="features"
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      pMM <<- matrix(nrow = input$num_row, ncol = 1)
      for(i in 1:input$num_inputs){
        
      
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM[,1]<<-vec
        Multiple_Fitted(MM,pMM)
        output$ploterror <- renderPlot({
          plot(di, pMM, main = " Plot of Error", xlab = "y", ylab = "erorr", col = "DeepSkyBlue")
        })
        output$plotdsit <- renderPlot({
          d <- density(di)
          plot(d, main="Kernel Density of Error ")
          polygon(d, col="red", border="blue")
        })
      }
      
    }
    
  })
  observeEvent(input$info,{
    if(input$radio1=="Multi"&input$radio2=="manually"){
      
      MM <<- matrix(nrow = input$num_row, ncol = input$num_inputs)
      pMM <<- matrix(nrow = input$num_row, ncol = 1)
      for(i in 1:input$num_inputs){
        textgenert<<-paste0("feature",i)
        vect<<-strsplit(input[[textgenert]]," ")
        vect<<-as.numeric(vect[[1]])
        if(length(vect)==input$num_row){
          vect<<-na.omit(vect)
          MM[,i]<<-vect
        }else{
        }
      }
      MM<<-cbind(1,MM)
      vec<<-strsplit(input$manul2," ")
      vec<<-as.numeric(vec[[1]])
      if(length(vec)==input$num_row&!(any(is.na(MM)))){
        vec=na.omit(vec)
        pMM[,1]<<-vec  
        Multiple_Fitted(MM,pMM)
        Confidance_Interval_B(input$value,MM,pMM)
        output$inf<-renderPrint(feature_selection_m(MM,pMM,input$value))
        output$varcovr<-renderPrint(data.frame( (as.double(mse)*variance_covariance_M)))
        
        
      }
    }else if(input$radio1=="Multi"&file.exists(input$demo_text2)&input$radio2=="URL"){
      feat<<-input$multifeatuer
      pfeat<<-input$radiopf
      x<<-df_reg[,feat]
      y<<-df_reg[,pfeat]
      x<<- cbind(1,x)
      Multiple_Fitted(x,y)
      Confidance_Interval_B(input$value,x,y)
      output$inf<-renderPrint(feature_selection(x,y,input$value))
      output$varcovr<-renderPrint(data.frame(variance_covariance_M))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)


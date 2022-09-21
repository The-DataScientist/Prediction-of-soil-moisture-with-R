library(Metrics)
library(lubridate)
D=1000
porosité=0.49
wmax=490
B=9
Sfc=0.47
Ks=124.3
neta=0.75
wbc=neta*wmax
wbc
x<-exp(B*(1-Sfc))-1
y<-log(Ks*(1/x))
a<-wmax*(Sfc-y/B)
b<-wmax/B
c<-Ks/x
sigma<-0.7
wmax<-490





library(readxl)
df <- read_excel("D:/KHALIL/ESSAI/2 éme/P3/10) Atelier/df.xlsx")
Ep<-df$`Ep mm/jour`
Pr<-df$`PLUIE+IRR mm/jour`
w<-(df$`SWI observé`*wbc)/100



q<-w/(sigma*wmax)
s<-c(rep(1,364))
M<-c()
for (i in  1:364) {
  M[i]<-min(q[i],s[i])
}


Gd<-exp((w-a)/b)-c
Gd
E<- M*Ep
E
#if (w=wmax) and (Wbc=nu*Wmax) and (Pr>(E+Gd)) {
#  Rs<-Pr-E-Gd
#} else if (W<Wbc) and (Pr>(E+Gd+Wbc-W)) {Rs<-Pr-E-Gd-(Wbc-W)}





ref<-c()
ref[1]<-0
for (i in 2:364){
  ref[i]<-df$`SWI observé`[i-1]
}

modref<-c()
for (i in 1:363){
  modref[i]<-abs(df$`SWI observé`[i]-df$`SWI observé`[i+1])
}


cc<-seq(from =0.1, to =3, by =0.1)
n=3.013603
for (sigma in c  ){
  
  q<-w/(sigma*wmax)
  s<-c(rep(1,364))
  M<-c()
  for (i in  1:364) {
    M[i]<-min(q[i],s[i])
  }
  E<- M*Ep
  w19<-c()
  w19[1]<-0
  for (i in 2:364){
    
    M[i-1]<-min(w19[i-1]/(sigma*wmax),1)
    E[i-1]<-M[i-1]*Ep[i-1]
    Gd[i-1]<- exp((w19[i-1]-a)/b)-c
    w19[i]<-w19[i-1]+Pr[i-1]-E[i-1]-Gd[i-1]
    
  }
  
  swiest<-(w19/wbc)*100
  modest<-abs(swiest-df$`SWI observé`)
  modest<-abs(swiest-df$`SWI observé`)
  k=rmse(df$`SWI observé`,swiest)
  if (k<n){
    n<-k
    r<-sigma
  }
}
n
r

plot(w19,type='l',col='red')
lines(df$w,col='green')


zz<-df[363,7]
zz
x1<-unlist(zz)
x1
Gdta<-exp((zz-a)/b)-c
Gdta

Gdt1<-unlist(Gdta)
Gdt1

c


#****************************************shiny******************************************************
# Load R packages
library(plotly)
library(ggplot2)
library(shiny)
library(shinythemes)

# Define server function  
server<- function(input,output, session) {
   
   output$statdes=renderText({
     summary(df[,input$quanti])
   })
   
   output$plot2<-renderPlotly({
     z = unlist(df[,input$quanti1])
     p <- plot_ly(y = ~z,x=df$DATE, type = "scatter",line = list(color = "green"))%>%layout(title = names(df[,input$quanti1]))
   })
   
  
   df3<-eventReactive(input$button1,{
     
     w0=x1+as.numeric(input$txt0)-as.numeric(input$txt1)-Gdt1
     
     Gdt2<-unlist(exp((w0-a)/b)-c)
     w1=w0+as.numeric(input$txt2)-as.numeric(input$txt3)-Gdt2
     
     Gdt3<-unlist(exp((w1-a)/b)-c)
     w2=w0+as.numeric(input$txt4)-as.numeric(input$txt5)-Gdt3
     
     Gdt4<-unlist(exp((w2-a)/b)-c)
     w3=w0+as.numeric(input$txt6)-as.numeric(input$txt7)-Gdt4
     
     Gdt5<-unlist(exp((w3-a)/b)-c)
     w4=w0+as.numeric(input$txt8)-as.numeric(input$txt9)-Gdt5
     
     Gdt6<-unlist(exp((w4-a)/b)-c)
     w5=w0+as.numeric(input$txt10)-as.numeric(input$txt11)-Gdt6
     
     Gdt7<-unlist(exp((w5-a)/b)-c)
     w6=w0+as.numeric(input$txt12)-as.numeric(input$txt13)-Gdt7
     
     print(paste("W(t+1)=",round(w0,digits=3) ,
                 "W(t+2) = ", round(w1,digits=3),
                 "W(t+3) = ", round(w2,digits=3),
                 "W(t+4) = ", round(w3,digits=3),
                 "W(t+5) = ", round(w4,digits=3),
                 "W(t+6) = ", round(w5,digits=3),
                 "W(t+7) = ", round(w6,digits=3)))
     
     
     
     
     
        
     
   })
   
   output$pred <- renderText({
     df3()
   })
   
   p <- reactive({
     
     w0=x1+as.numeric(input$txt0)-as.numeric(input$txt1)-Gdt1
     
     Gdt2<-unlist(exp((w0-a)/b)-c)
     w1=w0+as.numeric(input$txt2)-as.numeric(input$txt3)-Gdt2
     
     Gdt3<-unlist(exp((w1-a)/b)-c)
     w2=w0+as.numeric(input$txt4)-as.numeric(input$txt5)-Gdt3
     
     Gdt4<-unlist(exp((w2-a)/b)-c)
     w3=w0+as.numeric(input$txt6)-as.numeric(input$txt7)-Gdt4
     
     Gdt5<-unlist(exp((w3-a)/b)-c)
     w4=w0+as.numeric(input$txt8)-as.numeric(input$txt9)-Gdt5
     
     Gdt6<-unlist(exp((w4-a)/b)-c)
     w5=w0+as.numeric(input$txt10)-as.numeric(input$txt11)-Gdt6
     
     Gdt7<-unlist(exp((w5-a)/b)-c)
     w6=w0+as.numeric(input$txt12)-as.numeric(input$txt13)-Gdt7
  
     
     dj <- df$DATE[363] + days(1)
     dj1 <- dj + days(1)
     dj2 <- dj1 + days(1)
     dj3 <- dj2 + days(1)
     dj4 <- dj3 + days(1)
     dj5 <- dj4 + days(1)
     dj6 <- dj5 + days(1)
     
     
     plot(y=w19,x=df$DATE,xlab="DATE",ylab = "w_estimé & w_observé",main = "Estimation des précipitations de l'année 2017",type='l',col='blue')
     lines(y=df$w,x=df$DATE, type = 'l',col='red') 
     
     points(dj,w0)
     points(dj1,w1)
     points(dj2,w2)
     points(dj3,w3)
     points(dj4,w4)
     points(dj5,w5)
     points(dj6,w6)
     
     
     
     
     
     
     legend("topleft", legend=c("W_observé", "W_estimé"),
            col=c("red", "blue"), lty=1:2, cex=0.8)
   })
   
   output$plot <- renderPlot({
     p()
   })
   
    

} # server

# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(h3(strong( "Estimation des précipitations de l'année 2017")),
                            tabPanel("Statistiques Descriptives",
                                     
                                          tabsetPanel(
                                       
                                          tabPanel("Visualisation",mainPanel(selectInput("quanti1","Choisir un paramétre :",
                                                   choices=c(colnames(df[c(2,3,5,7)]))),
                                       
                                       
                                                    h4("Evolution au cours du temps : "),
                                                    plotlyOutput("plot2")
                                                   
                                       
                                     )))
                                     
                                     
                                     
                                     ),
                  
                  tabPanel("Prédiction",
                           sidebarPanel(
                             tags$h3("Entrez les nouvelles données :"),
                             
                             
                             numericInput("txt0", "PR(1):", value = 0),
                             numericInput("txt1", "E(1):", value = 0),
                             numericInput("txt2", "PR(2):", value = 0),
                             numericInput("txt3", "E(2):", value = 0),
                             numericInput("txt4", "PR(3):", value = 0),
                             numericInput("txt5", "E(3):", value = 0),
                             numericInput("txt6", "PR(4):", value = 0),
                             numericInput("txt7", "E(4):", value = 0),
                             numericInput("txt8", "PR(5):", value = 0),
                             numericInput("txt9", "E(5):", value = 0),
                             numericInput("txt10", "PR(6):", value = 0),
                             numericInput("txt11", "E(6):", value = 0),
                             numericInput("txt12", "PR(7):", value = 0),
                             numericInput("txt13", "E(7):", value = 0),
                             actionButton("button1", "predict"),
                             
                             
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Résultats des prédictions(+ 7 jrs) : "),
                             textOutput("pred")
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Plots",h1("Superposition plots"), plotOutput("plot"))
                  
                  
                  
                ) # navbarPage
) # fluidPage




# Create Shiny object
shinyApp(ui = ui, server = server)













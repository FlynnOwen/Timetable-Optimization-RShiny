#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
source('../R_Scripts/libraries.R')

DataDir <- '../Data'

#### Server
shinyServer(function(input, output){
    
  #### Loading in the data ####
  
  timetable1 <- read.csv(file.path(DataDir,"Timetable_data.csv"))
  timetable2 <- read.csv(file.path(DataDir,"Timetable_data2.csv"))
  timetable3 <- read.csv(file.path(DataDir,"Timetable_data_small.csv"))
  timetable <- reactive({switch(input$Dataset,"Original Dataset" = timetable1,"Alternative Dataset" = timetable2, "Small school Dataset" = timetable3)})
  
  #### Creating an edgelist to use ####
  
  observeEvent(input$Dataset,{
    subjects <- as.vector(as.matrix(timetable()[-1]))
    Subjects <- c(unique(subjects))[unique(subjects)!= ""]
    Large <- c(rep(10,length(Subjects)))
    Group <- c(1:length(Subjects))
    vertid <- c(0:(length(Subjects)-1))
    vertices <- data.frame(vertid,Subjects,Group,Large)
    IDvector <- vertices[,c(1,2)]
    
    Timetable <- as.matrix(timetable()[-1])
    
    for (i in 1:nrow(Timetable)){
      for (j in 1:ncol(Timetable)){
        Timetable[i,j] <- IDvector["vertid"][IDvector["Subjects"] == Timetable[i,j]]
      }
    }
    fromvec <- c()
    for (i in 1:nrow(Timetable)){
      for (j in 1:ncol(Timetable)){
        fromvec <- append(fromvec, rep(Timetable[i,j],ncol(Timetable)-j))
      }
    }
    fromvec <- c(as.numeric(fromvec))
    
    tovec <- c()
    for (i in 1:nrow(Timetable)){
      for (j in 1:ncol(Timetable)){
        tovec <- append(tovec,Timetable[i,][-c(1:j)]) 
      }
    }
    tovec <- c(as.numeric(tovec))
    
    Value <- c(rep(1,length(fromvec)))
    
    edges <- data.frame(fromvec,tovec,Value)
    vertices <- vertices[-1]
    
    #### Colouring the network ####
    
    Grouphelp <- edges[,1:2]
    Group <- rep(0,length(vertices))
    
    rowsadj <- c(rep(0,length(Subjects)))
    adjacencymatrix <- replicate(length(Subjects),rowsadj)
    
    for (i in 1:nrow(Grouphelp)){
      for (j in 1:ncol(Grouphelp)){
        if (j == 1){
          adjacencymatrix[Grouphelp[i,j]+1,Grouphelp[i,j+1]+1] <- 1
          adjacencymatrix[Grouphelp[i,j+1]+1,Grouphelp[i,j]+1] <- 1
        }
        else {
          adjacencymatrix <- adjacencymatrix
        }
      }
    }
    
    
    adjacencymatrix <- adjacencymatrix == 1
    colorvector <- getColoring(adjacencymatrix)
    vertices$Group_opt <- colorvector
    
    #### Printing the optimum timetable ####
    Classdata1 <- list()
    for (i in 1:getNColors(adjacencymatrix)){
      Classdata1[[i]] <- vertices$Subjects[vertices$Group_opt == i]
    }
    ClassdataF <- c()
    for (i in 1:length(Classdata1)){
      ClassdataF[i] <- str_c(as.vector(Classdata1[[i]]), collapse = " ")
    }
    Final_Timetable <- as.data.frame(matrix(ClassdataF,nrow = 2), row.names = c("9am","1pm"))
    if (Final_Timetable[1,1] == Final_Timetable[2,ncol(Final_Timetable)]){
      Final_Timetable[2,ncol(Final_Timetable)] <- ""
    }
    for(i in 1:ncol(Final_Timetable)){ 
      colnames(Final_Timetable)[i] <- paste('Day',i)}
    
    #### Reactive download button to download the optimal timetable ####
    output$report = downloadHandler(
      filename = function(){
        paste(input$Dataset,"-Timetable.csv",sep = "")
      },
      content = function(file) {
        write.csv(Final_Timetable,file)
      }
    )
    
    #### Subjects barplots ####
    edges1 <- edges
    timetable <- timetable()[-1]
    subjectslist <- list()
    for (i in Subjects){
      subjectslist[[i]] <- data.frame(timetable[timetable$V1 == i | timetable$V2 == i | timetable$V3 == i | timetable$V4 ==i,])
    }
    
    #### Altering node sizes for star and clique plots ####
    countsvector <- data.frame(table(subjects))
    colnames(countsvector)[1] <- "Subjects"
    vertices <- merge(x = vertices, y = countsvector, by = "Subjects",sort = FALSE)
    
    #### Creating reactive UI variables ####
    output$SubjectSelector <- renderUI({
      selectInput("Subject", tippy("Subject:",tooltip = "The class that will be plotted as the center of star. Use this to gain insights about a particular subject."), Subjects)
    })
    
    output$AdjSize <- renderUI({
      selectInput("CliqueSize", tippy("Clique Size",tooltip = "Number of nodes (classes) that will be plotted in the clique graph (complete subgraph)"),
                  choices = 2:getNColors(adjacencymatrix))
    }) 
    
    output$Title1 <- renderText({
      if(input$colourme){
        "Coloured Network of Classes Students Are Taking"
      }
      else{
        "Network of Classes Students Are Taking"
      }
    }) 
    
    
    #### Plotting Complete Network - conditionals depending on chromatic colouring / plotting as a cirlce ####
    tempedge <- Grouphelp + 1
    tempnet <- network(tempedge, matrix.type = "edgelist", directed = FALSE)
    colars <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
    COL <- vertices$Group_opt
    
    Subjects1 <- c()
    for (i in Subjects){
      Subjects1[i] <- paste(match(i,Subjects),i)
    }
    Subjects1 <- as.vector(Subjects1)
    
    
    tempnet%v%"Coolers" <- colars[COL]
    
    output$net <- renderPlot(if(input$colourme){
      if(input$Circle){
        plot.network(tempnet,
                     vertex.col = colars[COL],
                     vertex.cex = 2,
                     edge.col = "gray", label = 1:30,mode = "circle", main = "Coloured Timetable Network")
        legend("topleft",legend = Subjects1,inset=c(-0.1,0),xpd = TRUE,cex = 0.7,fill = colars[COL],pt.bg = 1:30)
      }
      else{
        plot.network(tempnet,
                     vertex.col = colars[COL],
                     vertex.cex = 2,
                     edge.col = "gray", label = 1:30, main = "Coloured Timetable Network")
        legend("topleft",legend = Subjects1,inset=c(-0.1,0),xpd = TRUE,cex = 0.7,fill = colars[COL],pt.bg = 1:30)
      }
    }
    else{    
      if(input$Circle){
        plot.network(tempnet,
                     vertex.col = colours(1)[1:30],
                     vertex.cex = 2,
                     edge.col = "gray", label = 1:30, mode = "circle", main = "Timetable Network")
        legend("topleft",legend = Subjects1,inset=c(-0.1,0),xpd = TRUE,cex = 0.7,fill = colours(1)[1:30],pt.bg = 1:30)
      }
      else {
        plot.network(tempnet,
                     vertex.col = colours(1)[1:30],
                     vertex.cex = 2,
                     edge.col = "gray", label = 1:30, main = "Timetable Network")
        legend("topleft",legend = Subjects1,inset=c(-0.1,0),xpd = TRUE,cex = 0.7,fill = colours(1)[1:30],pt.bg = 1:30)
      }
    }
    )
    
    #### Creating a list of edges with weight 1 ####
    a <- table(tempedge)
    if (nrow(a) == ncol(a)){
      for(i in 1:nrow(a)){
        for(j in 1:ncol(a)){
          a[i,j] <- a[i,j] + a[j,i]
        }
      }
      a[lower.tri(a)] <- 0
      b <- which(a==1,arr.ind=T)
      
      output$SingleWeights <- renderUI({
        selectInput("Single Weights", tippy("Weight 1 edges",tooltip = "This is a list of edges (the two numbers identify nodes that they are connected between) that are of weight only one. Deleting or rearranging some of these entries in the original CSV file before inputting may change the timetable - ie make it more efficient"),
                    choices = paste(b[,1],",",b[,2]))
      }) 
    }
    
    else{
      output$SingleWeights <- renderUI({
        selectInput("Single Weights", tippy("Weight 1 edges",tooltip = "This is a list of edges (the two numbers identify nodes that they are connected between) that are of weight only one. Deleting or rearranging some of these entries in the original CSV file before inputting may change the timetable - ie make it more efficient"),
                    choices = "Unavaible for this graph")})
    }
    
    #### Plotting reactive Star Newtwork ####
    
    output$net2 <- renderForceNetwork({
      number <- which(Subjects == input$Subject)-1
      common_nums <- edges1[(edges1$tovec == number | edges1$fromvec == number),c(1,2)][1]+edges1[(edges1$tovec == number | edges1$fromvec == number),c(1,2)][2] - number
      common_vec <- c()
      newtable <- table(common_nums)
      
      for (i in 1:input$NumClasses){
        common_vec[i] <- as.numeric(names(which.max(newtable)))
        newtable <- newtable[names(newtable) != names(which.max(newtable))]
      }
      
      common_vec <- append(common_vec, number)
      edges_mostcommon <- edges1[0,]
      edges_temp <- edges1
      for (i in 1:input$NumClasses){
        edges_temp <- edges1[(edges1$fromvec == number | edges1$tovec == number) & (edges1$fromvec == common_vec[i] | edges1$tovec == common_vec[i]),]
        edges_mostcommon <- rbind(edges_mostcommon,edges_temp)
      }
      vertices_mostcommon <- matrix(nrow = length(common_vec), ncol = 3)
      
      for(i in 1:length(common_vec)){
        vertices_mostcommon[i,] <- vertices[vertices[2] == sort((common_vec+1))[i]][c(1,2,3)]
      }
      
      vertices_mostcommon <- as.data.frame(vertices_mostcommon)
      names(vertices_mostcommon) <- c("Subjects","Group","Large")
      edges_mostcommon <- edges_mostcommon[,c(1,2)]
      
      for (i in 0:length(common_vec-1)){
        edges_mostcommon[edges_mostcommon == sort(common_vec)[i+1]] <- i
      }
      
      edges_mostcommon <- data.frame(edges_mostcommon,"Group" = rep(1,nrow(edges_mostcommon)))
      edges_mostcommon$Value <- rep(1,nrow(edges_mostcommon))
      
      vertices_mostcommon <- left_join(vertices_mostcommon,vertices, by = c("Subjects" = "Subjects"))
      
      if(input$Clique == "Star"){
        
        forceNetwork(Links  = edges_mostcommon, Nodes   = vertices_mostcommon,
                     Source = "fromvec", Target  = "tovec",
                     Value  = "Value",  NodeID  = "Subjects",
                     Group  = "Group.x",  opacity = input$opacity,
                     fontSize = input$fontsize, linkDistance = 100,
                     Nodesize = "Freq",
                     clickAction = 'Shiny.onInputChange("Subject", d.name)')
      }
      
      #### Plotting Clique Network ####
      
      else{ cliquelist1 <- cliques(graph_from_adjacency_matrix(adjacencymatrix),min=input$CliqueSize,max=input$CliqueSize)
      set.seed(input$Seed)
      cliquelist <- unlist(cliquelist1[sample(1:length(cliquelist1),1)])
      
      test2vertices <- array()
      test2vertices <- vertices[unlist(cliquelist),]
      
      fromvecSUB <- c()
      placevec <- c()
      
      for (i in 0:(length(cliquelist)-2)){
        placevec <- rep(i,length(cliquelist)-i-1)
        fromvecSUB <- append(fromvecSUB,placevec)
      }
      
      
      tovecSUB <- c()
      place1vec <- c()
      for (i in 1:(length(cliquelist)-1)){
        place1vec <- c(i:(length(cliquelist)-1))
        tovecSUB <- append(tovecSUB,place1vec)
      }
      
      Value <- rep(1,length(fromvecSUB))
      test1edges <- data.frame(fromvecSUB,tovecSUB,Value)
      
      forceNetwork(Links = test1edges, Nodes = test2vertices, Source = "fromvecSUB", Target = "tovecSUB",
                   Value = "Value", NodeID = "Subjects", linkDistance = 150, fontSize = input$fontsize,Group = "Group_opt", opacity = input$opacity, zoom = FALSE,Nodesize = "Freq") }
    })
    
    output$`time-table` <- renderTable(
      Final_Timetable
    )
    output$subjecthist <- renderPlot({
      num = input$Subject
      testplot <- table(unlist(as.data.frame((subjectslist[num]))))[-which.max(table(unlist(as.data.frame((subjectslist[num])))))]
      class <- names(which.max(table(unlist(as.data.frame((subjectslist[num]))))))
      testplot <- testplot[testplot != 0]
      blah <- barplot(testplot,las = 2, col = viridis(15), main = paste("Other classes that the",countsvector$Freq[countsvector$Subjects == class],"Students enrolled in",class,"are taking"),ylab = "Number of Students Taking Class",ylim = c(0,max(testplot)+2))
      text(blah,testplot,label = testplot, pos = 3, cex = 1)
      
      #### Reactive Title for Star/Clique plot ####
      output$Title2 <- renderText({
        if(input$Clique == "Star"){
          paste("The",input$NumClasses,"Most Common Courses Taken With",class)
        }
        else {
          paste("Group of" ,input$CliqueSize,"classes that are all taken together")
        }
      })
    })
    
    #### Outputting density of network ####
    output$Density <- renderText({
      density_graph <- paste("Network Density: ",round(gden(network(as.matrix.network(network(unique(tempedge), matrix.type = "edgelist", directed = FALSE, multiple = FALSE),matrix.type = "adjacency"))),4))
    })
    
    #### Testing whether two variables are adjacent (Atleast 1 student is taking both class 1 and class 2 etc) ####
    output$Adjacency <- renderText({
      if(is.adjacent(network(as.matrix.network(network(unique(tempedge), matrix.type = "edgelist", directed = FALSE, multiple = FALSE),matrix.type = "adjacency")), input$Adj1, input$Adj2) == TRUE){
        "At least one student is taking both class 1 and class 2"
      }
      else{
        "No students are taking both class 1 and class 2"
      }
    })
    
  })
}
)

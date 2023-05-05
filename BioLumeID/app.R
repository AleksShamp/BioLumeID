library(shinydashboard)
library(shinyjs)
library(shiny)
library(dplyr)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(gridExtra)
library(ggplot2)
setwd("./") 

data<-read.csv('data.csv',blank.lines.skip=FALSE,na.strings="",sep=";")%>% select(GENRE,ORDER,SPECIES,QUALITY,SOURCE,INFO1,INFO2,INFO3)
data<-filter(data, rowSums(is.na(data)) != ncol(data))
data_biolum<-read.delim('unique_table.txt',header=TRUE,sep=" ")
uniquo<-unique(c(NA,data$GENRE,data_biolum$Group,data$ORDER,data$SPECIES,data_biolum$Concept))


data_prof<-read.delim('S1_dataset.txt',sep= " ")
NormData=read.table("S2_dataset.txt",header=T,sep="",stringsAsFactors=FALSE)
REF<-read.csv('REF.csv',blank.lines.skip=FALSE,na.strings="",sep=";")


##################################################

#####GIGA IMPORTANT, CLASSE PAR ORDRE VOULU DATA_BIOLUM
data_prof$Biolum=factor(data_prof$Biolum,levels=c("bioluminescent","likely","undefined","unlikely","non-bioluminescent"))

# Defined the bin-depth parameter
dD=100
# discretize the dataset depending on the size of bin-depth
data_prof$DepthCut=cut(data_prof$Depth,breaks=seq(0,max(data_prof$Depth,na.rm=T),by=dD))
agg_depth=aggregate(data_prof$Depth,by=list(data_prof$DepthCut),FUN=length)
# the refering depth is the middle of each class
agg_depth=cbind(agg_depth,as.numeric(agg_depth[,1])*dD-(dD/2))
names(agg_depth)=c("DepthCut","NbCountDep","BinDepth")
# normalisation of the dataset per hour
agg_depth$NbCountDep=agg_depth$NbCountDep/NormData[1:nrow(agg_depth),2]*3600
# merge data by bin-depth
data_prof=merge(data_prof,agg_depth,by="DepthCut")


# plot par groupe et par depth ! Biolum ne corresppond pas a un groupe
agg_depor=aggregate(data_prof$Biolum,by=list(data_prof$DepthCut,data_prof$Group,data_prof$Biolum,data_prof$BinDepth),length)
names(agg_depor)=c("DepthCut","Group","Biolum","BinDepth","SumDepGroup")
# NORMALIZE per hour
agg_depord=merge(agg_depor,NormData,by.x="BinDepth",by.y="Depth")
agg_depord$SumNorm=agg_depord$SumDepGroup/agg_depord$Seconds*3600
cbGPalette13<-c("#663300","#66FF66","#FF9999","#CC3366","#FF0000","#000033","#FFCC33","#FFFF66","#006600","purple4","#FF6600","#0066CC","#33CCFF")
LabelString = c("","","","3500","","","","","3000","","","","","2500","","","","","2000","","","","","1500","","","","","1000","","","","","500","","","","","0")

Palette_color<-list()
order <- c("bioluminescent","likely","undefined","unlikely","non-bioluminescent")
Palette_order<-c("#00CCFF","#A9EAFE","#FFFF99","#798081","#222244")

Taxon_list<-unique(agg_depord$Group)

################################################## POUR PT FILE
NormData1=read.table("Norm_data.txt",header=T,sep="",stringsAsFactors=FALSE)


Title<-tags$a(tags$img(src="BioLumeID.png"),"BioLumeID",style="color:#edf5fc",)

###################################################

ui <- dashboardPage(
  
 
  dashboardHeader(title=Title
),
  dashboardSidebar(
      # Custom CSS to hide the default logout panel
      tags$head(tags$style(HTML('.logo {
                              background-color: #2666cc !important;
                              }
                              .navbar {
                              background-color: #2666cc !important;
                              }
                              '))),
      
      # The dynamically-generated user panel
      uiOutput("userpanel"),
      
    sidebarMenu(id = "sidebarID",
      menuItem("Menu",tabName = "menu",icon=icon("house", style="color: #dbdbdb")),
      
      
      menuItem("bioluminescence Test",tabName="Biolu_Test", expandedName = "EXPANDED",

               selectizeInput("search",label="Entry",choices = NULL),
               
               selectInput("category", "Category:",choices=c("all","bioluminescent","likely","unlikely","non-bioluminescent","undefined")),
               
               checkboxInput("cplmt_info", "Complementary Info",value=FALSE),
               verbatimTextOutput("txt"),
               
               icon=icon("magnifying-glass", style="color: #dbdbdb")
               ),              

                  
               
               
      menuItem("Graph Plot",
               
               tabName = "plots",icon=icon("chart-simple", style="color: #dbdbdb")
  
      ),
      
      menuItem("final countdown",tabName = "file",icon=icon("folder", style="color: #dbdbdb")),
      hidden(menuItem("hidden", tabName = "hidden"))
      )),
  
  
  dashboardBody(useShinyjs(),      
    

    tabItems(
    tabItem(tabName="menu",
            
            box(width=NULL," BioLumeID est une application conçue pour permettre l'identification d'espèces bioluminescentes. Elle utilise les données d'Oba 2021 et de Martini S. and Haddock 2017.",br(),
                "Le fichier Oba 2021 recense toutes les espèces bioluminescentes connues à ce jour, tandis que le fichier Martini S. et Haddock 2017 répertorie des concepts identifiés à des profondeurs différentes, avec leur niveau de description le plus précis et leur qualité de bioluminescence correspondante (expliqué plus loin)."),
            box(width=NULL," La première section de l'application vous permettra de vérifier si une espèce, un taxon ou un genre est bioluminescent en affichant les données de deux tableaux de données issus de ces bases de données.",br(),
                " Si la recherche ne donne aucun résultat, cela signifie que l'information n'est pas présente dans les données, que votre recherche est incorrecte, que l'organisme n'est pas bioluminescent ou n'a pas encore été répertorié comme tel. ",br(),
                "Dans le premier tableau, vous pourrez filtrer les différentes catégories de bioluminescence, telles que bioluminescentes, probablement bioluminescentes, non définies, probablement pas bioluminescentes et non-bioluminescentes.",br(),
                " Dans le deuxième tableau, vous pourrez afficher des informations complémentaires contenues dans le fichier Oba 2021",br(),
                " Dans ce tableau de données, certaines informations ont été écrites en japonais et aucune traduction ne sera effectuée afin d'éviter toute erreur de traduction. Si vous souhaitez consulter ces informations supplémentaires, nous vous invitons à vous référer au fichier source."),
            box(width=NULL,"Dans la deuxième section de l'application, vous pourrez observer les graphiques effectués dans l'article de Martini S. and Haddock 2017 et modifier les paramètres sélectionnés. ",br(),
                "Toutefois, il est conseillé de ne jamais désélectionner tous les taxons, car cela entraînerait une erreur."),
            box(width=NULL,"Enfin, dans la dernière partie de l'application, vous pourrez Uploader un tableau de données contenant vos identifications d'espèces ou de concepts.",br(),
                " L'application identifiera les informations de bioluminescence dans vos données et vous fournira un graphique représentant les proportions de bioluminescence dans vos données de ce type :",
                img(src="file_plot.png" ,height="50%", width="50%", align="right")
                ,br(),
                "Il est important que le Tableau de donnée soit fournis dans ce format : ",br(),
                img(src="Supported.png", height="50%", width="50%", align="right")
                )),
    tabItem(tabName="hidden",
            
            fluidRow(
      
            column(width = 12,
              box(
              title ="Identification dans Martini S. and Haddock 2017 ",status="danger",solidHeader =TRUE,collapsible = TRUE, 
            
              dataTableOutput("fdata_biolum"),width=NULL
              
              
            ),
            box(
              title ="Identification dans Oba 2021",status="danger",solidHeader =TRUE,collapsible = TRUE, 
              
              dataTableOutput("fdata"),width=NULL
              
            ))
   
            
                        
  )),
  tabItem(tabName="plots",
          
          fluidRow(
            
            
            box(title='Distribution of the bioluminescence capability over depth'
                ,status="success",solidHeader =TRUE,width = NULL,collapsible = TRUE, 
                column(width=4,
                checkboxGroupInput("capability", "select capability",
                                   c("bioluminescent" ="bioluminescent",
                                     "likely" = "likely",
                                     "undefined"="undefined",
                                     "unlikely" = "unlikely",
                                     "non-bioluminescent"="non-bioluminescent"))),
              column(width=8 ,
              plotOutput("biolum_capacity")
              ) 
            )
            
            
            
          ),
            
            box(title='Proportion of bioluminescence capability over taxonomic categories and depth'
                ,status="success",solidHeader =TRUE,width = NULL,collapsible = TRUE,
                pickerInput(
                  inputId = "taxon_choice", label = "Taxon Choices :",
                  choices = c(Taxon_list),
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0}/{1} Taxons sélectionnés"),
                  multiple = TRUE,selected= c(Taxon_list)), #--> input$taxon_choice 
              column(width = 5, 
              plotOutput("Biolum_diversity_npb")),
              column(width = 7,
              plotOutput("Biolum_diversity_pb")       
                     )              
            )),
  
    tabItem(tabName = "file",
            fluidRow(
              box(title = "File Upload",status="primary",solidHeader =TRUE,width = NULL,
                  fileInput("data_file", "Choose CSV File",
                            accept = ".csv"),
                  tags$hr(),
                  checkboxInput("header", "Header", TRUE),

                  
                  ),
                box(title="Your graph",status="primary",solidHeader =TRUE,width = NULL,collapsible = TRUE,
                    
                          
              column(width=6,
                    plotOutput("file_plot2")),
              column(width=6,
                    plotOutput("file_plot1"))
                    
                    
                    )
              
              
            )
            
            
            
            )
  
  )
  ))
    
  
  
server<-function(input,output,session){
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  output$Title<- renderImage({
    list(src="ju.png",width="100%" )
  },deleteFile = FALSE)
  
  
  #update search input
  updateSelectizeInput(session, 'search', choices = uniquo, server = TRUE,selected = NULL)

  #affiche element caché, sidebar expand bioluminescence
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "EXPANDED"){
      updateTabItems(session, "sidebarID", selected = "hidden")
    }
  })
  
  
  output$txt <-renderText({
    if (req(input$search) %in% uniquo){
      
      paste("Saisie correcte")
      
    }else {
      paste("Votre saisie est incorrecte")}
      })
  
  
  output$fdata_biolum <- renderDataTable({
    choice<-req(input$category)
    
    if (choice== "all"){
      category<-data_biolum
    }else{
      
      category<-data_biolum[data_biolum$Biolum==choice,]
    }
    
  
    txtinput<-req(input$search)
    into_concept<-category[category$Concept==txtinput,]
    into_group<-category[category$Group==txtinput,]
    tot<-bind_rows(into_concept,into_group)
    tot<-unique(tot)
    datatable(tot,options = list(pageLength = 5, autoWidth = FALSE,scrollX=TRUE,lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),pageLength = 10),rownames= FALSE)
    })
  
  output$fdata <- renderDataTable({
    
    
    info_cplmnt<-input$cplmt_info
    
    if (info_cplmnt==FALSE){
      data<-data[,!names(data) %in% c('INFO1',"INFO2",'INFO3')]
    }
    
    txtinput<-req(input$search)
    into_spe<-data[data$SPECIES== txtinput,]
    into_genre<-data[data$GENRE== txtinput,]
    into_order<-data[data$ORDER== txtinput,]
    tot<-bind_rows(into_spe,into_genre,into_order)
    tot<-unique(tot)
    datatable(tot, 
              options = list(pageLength = 5,autoWidth = FALSE,scrollX=TRUE,lengthMenu = list(c(10, 20, -1), c('10', '20', 'All'))),rownames= FALSE)
    
  }  )
  
############# pt ploti plot plot

  output$biolum_capacity<- renderPlot({

    
    if ("bioluminescent"%in%input$capability){
      Palette_color<-append(Palette_color,"#00CCFF")
    } else if ("bioluminescent"%in%input$capability==FALSE){
     Palette_color<-Palette_color[Palette_color!= "#00CCFF"]
     
     }

    if ("likely"%in%input$capability ){
      Palette_color<-append(Palette_color,"#A9EAFE")
    }else if ("likely"%in%input$capability==FALSE){
      Palette_color<-Palette_color[Palette_color!= "#A9EAFE"]
      
    }
    
    if ("undefined"%in%input$capability){
      Palette_color<-append(Palette_color,"#FFFF99")
    }else if ("undefined"%in%input$capability==FALSE){
      Palette_color<-Palette_color[Palette_color!= "#FFFF99"]
      
    }

    if ("unlikely"%in%input$capability){
      Palette_color<-append(Palette_color,"#798081")
    }else if ("unlikely"%in%input$capability==FALSE){
      Palette_color<-Palette_color[Palette_color!= "#798081"]
      
    }
    
    if ("non-bioluminescent"%in%input$capability){
      Palette_color<-append(Palette_color,"#222244")
    }else if ("non-bioluminescent"%in%input$capability==FALSE){
      Palette_color<-Palette_color[Palette_color!= "#222244"]
      
    }
    Palette_order<-c("#00CCFF","#A9EAFE","#FFFF99","#798081","#222244")
    select_palette_order<-Palette_color[order(match(Palette_color,Palette_order))]  
    
    
    select_capability<-data_prof[data_prof$Biolum==c(input$capability),]

   
   

    h=ggplot(select_capability,aes(x=-BinDepth),width=10)
    plot(h+geom_bar(aes(weight=NbCountDep,fill=Biolum),position="fill",width=95)+theme_classic()+
            guides(fill=guide_legend(title="Bioluminescence capability"))+ 
            theme(text =element_text(size=10))+
            scale_fill_manual(values=select_palette_order)+
            ylab("Proportion")+xlab("Depth (m)")+coord_flip())
    
  })
  output$Biolum_diversity_pb<-renderPlot({
    
    
    
    select_taxon<-agg_depord[agg_depord$Group%in%c(input$taxon_choice),]
    
    plot(ggplot(select_taxon,aes(x=reorder(-BinDepth,-BinDepth)),position = "fill")+
      theme_classic()+
      geom_bar(aes(weight=SumNorm,fill=Group),subset(select_taxon,select_taxon$Biolum %in% c("bioluminescent","likely")),position = "fill")+
      scale_fill_manual(values=alpha(cbGPalette13,0.5))+ylab("Proportion")+xlab("Depth (m)")+
      guides(fill=guide_legend(title="Taxon"))+
      scale_x_discrete(labels =LabelString)+
      xlab("Depth (m)") + ylab("Proportion")+
      coord_flip()+ggtitle("Probably bioluminescent")+
      theme(plot.title=element_text(size=14),text = element_text(size=12)))
    
  })
  
  output$Biolum_diversity_npb<-renderPlot({
    select_taxon<-agg_depord[agg_depord$Group%in%c(input$taxon_choice),]
    
    plot(ggplot(select_taxon,aes(x=reorder(-BinDepth,-BinDepth)),position = "fill")+
           theme_classic()+
           geom_bar(aes(weight=SumNorm,fill=Group),subset(select_taxon,select_taxon$Biolum %in% c("non-bioluminescent","unlikely")),position = "fill")+
           scale_fill_manual(values=alpha(cbGPalette13,0.5))+xlab("Depth (m)")+ylab("Proportion")+
           guides(fill=guide_legend(title="Taxon",reverse=TRUE))+
           scale_x_discrete(labels = LabelString)+
           xlab("Depth (m)")+ylab("Proportion")+
           coord_flip()+ggtitle("Probably non-bioluminescent")+
           theme(legend.position = "none",plot.title=element_text(size=14),text = element_text(size=12)))
    
  })
  
  ##################################" pt final countdown
  


  
  mydata <- reactive({ 
    req(input$data_file) ## ?req #  require that the input is available
    
    inFile <- input$data_file 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep =";")
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
  
    df<-as.data.frame(df)
    
    for (l in 1:nrow(df)){
      for (n in 1:nrow(REF)){
        
        if (df$ConceptName[l] %in% REF$Species[n]){
          df$Biolum[l]<-REF$Biolum[n]
          
        }
      }
      
      
    } 
    df$Biolum<-replace_na(df$Biolum,"undefined")
    df$Biolum=factor(df$Biolum,levels=c("bioluminescent","likely","undefined","unlikely","non-bioluminescent"))
    
    return(df)
  })
  
  output$file_plot1<-renderPlot({
    test_file<-mydata()
    
    # aggregate by feature
    agg_data=aggregate(test_file$Biolum,by=list(test_file$Biolum,test_file$ConceptName),length)
    colnames(agg_data)=c("Biolum","Group","FGenCounts")
    # add the percentages
    agg_data$Perc=agg_data$FGenCounts/sum(agg_data$FGenCounts)*100
    # Percentages per taxa
    
    p=ggplot(agg_data,aes(weight=Perc,x=1.9))+geom_bar(aes(fill=Biolum),width=10)
    plot(p+coord_polar(theta="y")+
            theme_minimal()+ 
            theme(axis.text.x=element_blank())+
            guides(fill=guide_legend(title="Bioluminescence capability"))+ 
            theme(axis.text.y=element_blank(),text = element_text(size=12),legend.key = element_blank())+
            xlab("")+ylab("")+
            scale_fill_manual(values=alpha(Palette_order,0.5))+
            annotate("text",x=0.7,y=0,label=""))
    
    
  })
  output$file_plot2<-renderPlot({
    
    test_file<-mydata()
    print(test_file)
    
    
    test_file$Biolum=factor(test_file$Biolum,levels=c("bioluminescent","likely","undefined","unlikely","non-bioluminescent"))
    
    # Defined the bin-depth parameter
    dD=100
    # discretize the dataset depending on the size of bin-depth
    test_file$DepthCut=cut(test_file$Depth,breaks=seq(0,max(test_file$Depth,na.rm=T),by=dD))
    test_agg_depth=aggregate(test_file$Depth,by=list(test_file$DepthCut),FUN=length)
    # the refering depth is the middle of each class
    test_agg_depth=cbind(test_agg_depth,as.numeric(test_agg_depth[,1])*dD-(dD/2))
    names(test_agg_depth)=c("DepthCut","NbCountDep","BinDepth")
    # normalisation of the dataset per hour
    # merge data by bin-depth
    test_file=merge(test_file,test_agg_depth,by="DepthCut")
    
    Palette_order<-c("#00CCFF","#A9EAFE","#FFFF99","#798081","#222244")

    
    select_capability<-test_file[test_file$Biolum==c(input$file_capability),]
    
    
    
    
    h=ggplot(test_file,aes(x=-BinDepth),width=10)
    plot(h+geom_bar(aes(fill=Biolum),position="fill",width=95)+theme_classic()+
           guides(weight=NbCountDep,fill=guide_legend(title="Bioluminescence capability"))+ 
           theme(text =element_text(size=10))+
           scale_fill_manual(values=Palette_order)+
           ylab("Proportion")+xlab("Depth (m)")+coord_flip())
    
    
    
    
    
    
    
  })
  
  
  
  
}

shinyApp(ui,server)

## !!!




##################################################################################################################



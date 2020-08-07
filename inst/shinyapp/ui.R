library(shiny)
library(shinyWidgets)
library(shinydashboard)

#-------------------------------------------------------------------------
# MAIN UI ELEMENTS IN APP BODY
#-------------------------------------------------------------------------

body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(".ban2 {color:#a14c4c}")
  ),
  useSweetAlert(),
  tabItems(
    
    #-------------------
    # Intro 
    #-------------------
    tabItem(tabName = "intro", fluidPage(box(status = "primary",solidHeader = TRUE,"This is an edited version of a package originally called 'Adjutant'. Here we are doing literature mining of PubMed for the purposes of conducting systematic reviews. Given a PubMed compatible search query, the algorithm will assemble a document corpus and, using unsupervised techniques, identify clusters of documents pertaining to a specific topic. To support rapid analysis of the document corpus, we have made explicit trade-offs between speed and accuracy, which are modifiable by the user, and aim to provide a “good-enough” result to support, rather than replace, a researcher’s decision making. Thus, an analysis of several thousand documents takes only a few minutes to complete, from initial query to cluster assignment. For more details on the package, please refer to the publication", 
                                             a("PMID: 30875428", align="left", href="https://pubmed.ncbi.nlm.nih.gov/30875428/"), br(),
                                              "This package has MIT license, which allows us to edit and use the code as we wish, free of charge, without restrictions.", br(),
                                             "Copyright (c) 2018 Anamaria Crisan
    Permission is hereby granted, free of charge, to any person obtaining a copy
                                             of this software and associated documentation files (the 'Software'), to deal
                                             in the Software without restriction, including without limitation the rights
                                             to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                                             copies of the Software, and to permit persons to whom the Software is
                                             furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all
                                             copies or substantial portions of the Software."
                                            ))),
    
    

    #-------------------
    # About 
    #-------------------
    tabItem(tabName = "about", fluidPage(box(status = "primary",solidHeader = TRUE, 
                                             "Version 1.0", br(),"Update: 07.08.2020",br(),
                                             "Catenion Data Science Unit ", br()))),

  
    #-------------------
    # Search Input 
    #-------------------
    tabItem("searchIn",
      uiOutput("searchInfoStatement"),
      hr(class="style-four"), 
      tabsetPanel(id = "loadData", type="pills",
        tabPanel("NCBI API",
                 uiOutput("NCBI_key")
                 ),
        tabPanel("Enter Query",
          br(),
          actionLink("loadExample",HTML("<small>Load an example query</small>")),
          shinyWidgets::searchInput(
              inputId = "searchQuery", 
              label = NULL, 
              placeholder = "Enter a PubMed search string here...", 
              #btnSearch = icon("search"), 
              btnSearch = tagList(shiny::icon("search"), "Search"),
              btnReset = tagList(shiny::icon("remove"), "Reset"), 
              width = "100%"
          )
        ),
        tabPanel("Load Data",
                 uiOutput("loadAnalysisUI"),
                 actionButton("chooseDir","Choose Different Directory"),
                 br(),
                 HTML("<small><em> Note: All files will be saved to the analysis directory you select</em></small>")
        )
      ),
      hr(),
      fluidRow(
        column(6,
               conditionalPanel("input.loadData == 'Enter Query'",
                 h4("Search Options"),
                 textInput("retmax",label="Maximum # of articles to retrieve (leave blank to retrieve all possible articles)",value=10000),
                 checkboxInput("dateRange","Specify Date Range?",value=FALSE),
                 conditionalPanel("input.dateRange == true",
                                  dateRangeInput("dateRangeVal",label="Specify a Date Range for articles")
                 )
                
               )
        ),
        column(6,
               h4("Analysis Saving Options"),
               em("Save Analysis?"),
               switchInput(inputId = "saveAnalysis",label=NULL,onLabel = "YES",offLabel = "NO", value = TRUE,inline=TRUE,width='350px'),
               uiOutput("analysisFileName")
        )
      )
    ),

    #------------------------------
    # Search Summary & Results
    #------------------------------
    tabItem("searchOverview",
      uiOutput("searchResInfoStatement"),
      hr(class="style-four"), 
      tabsetPanel(
        id="searchResSum",
        tabPanel("Document Corpus Table",
          br(),
          uiOutput("summaryText"),
          br(),
          DT::dataTableOutput("documentTable")
        ),
        tabPanel("Overview Summary",
          br(),
          em("This overview summary is intended to give you a sense of where these articles are published, what they cover (based solely on frequency of MeSH terms), and some of the most cited articles. These visualizations are best when there are multiple years of data from multiple journals."),
          hr(),
          h4("Publications over time"),
          fluidRow(
            column(width=8,
                  plotOutput("journalPubTime",height="250px")
            ),
            column(width=4,
                   plotOutput("yearPubPlot",height="250px")
            )
          ),
          hr(),
          h4("MeSH Terms over time"),
          em("Medical Subject Heading (MeSH) terms are a controlled vocabulary used by the National Library of Medicine and assigned to articles within PubMed. MeSH terms are intended to give the reader a sense of what a PubMed article is about, but they can sometimes be much too general. For more specific topic suggestions consider initiating a topic clustering from the 'Topic Discovery' menu item to get some more specific and data-driven sense of topics within your documents."),
          br(),
          br(),
          fluidRow(
            column(width=8,
                   plotOutput("meshTimePlot",width="100%",height="350px")
            ),
            column(width=4,
                  plotOutput("meshWordCloud",height="350px")
            )
          ),
          hr(),
          h4("Most Referenced Papers"),
          em("Most referenced articles according to PubMed Central internal counts, which don't match Google Scholar but are a reasonable heuristic. Top ten articles are returned, but there may be fewer if the search is limited to more recent publications that have yet to be cited."),
          br(),
          br(),
          fluidRow( #largely to keep consistent formatting
            column(width=8,
                   uiOutput("topCorpusArticles")
            ),
            column(width=4,
                   p("")
            )
          )
        )
      )
    ),
    
    #-------------------
    # Author Analysis
    #-------------------
    tabItem("authorAnalysis"),
    
    #-------------------
    # Topic Clustering
    #-------------------
    tabItem("clusterAnalysis",
      uiOutput("topicClustInfoStatement"),
      hr(class="style-four"), 
      uiOutput("topicClustInitiateButton"),
      uiOutput("clusterOverviewStatement"),
      br(),
      shinydashboard::box(title="Topic Clusters",
                          id = "clusterButtons",
                          width="100%",
                          collapsible = TRUE,
                          collapsed = FALSE,
                          uiOutput("showAllClustNames"),
                          uiOutput("selectCluster")
      ),
      fluidRow(
        column(7,
               shinydashboard::box(title="Topic Cluster Plot",
                                   id = "clusterPlot",
                                   width="100%",
                                   uiOutput("plotOptions"),
                                   plotOutput("tsnePlot", dblclick = "plot_dbclick",brush = "plot_brush")
                                   #uiOutput("clustTopicBoxInfo")
                                 )
        ),
        column(5, 
               shinydashboard::tabBox(title="",
                                   id="exploreClust",
                                   width="100%",
                                   tabPanel("Explore Clusters",
                                            uiOutput("clusterSelect"),
                                            uiOutput("clusterDetails"),
                                            plotOutput("clusterDetailsGrowth",height="250px")
                                   ),
                                   tabPanel("Explore Articles",
                                            uiOutput("brushedPapers")
                                            )
                                  
              )
                                  
                                   
                                  
               )   
        )
              
    ),
    #-------------------
    # Document Sampling
    #-------------------
    tabItem("docSample",
      uiOutput("sampleInfoStatement"),
      hr(class="style-four"), 
      em("Important! Document sampling is only meant to create a smaller subset of the data that you can export to your computer. Topic discovery will use the full document corpus irrespective of this sampling step. Click on 'show document sampling details' for more information.  "),
      br(),
      hr(),
      h4("Sampling Approach"),
      fluidRow(
        column(width=4,
          radioButtons("sampChoices",
                       label="Choose a sampling method", 
                       selected = "random",
                       width ="100%",
                       choiceNames=c("All - select all documents meeting the filter criteria",
                                     "Ranked - select a specific number of articles by citation count (highest cited first) that match the filter criteria",
                                     "Ranked - select a specific number of articles by citation count (highest cited first) that match the filter criteria AND according to some strata (group)",
                                     "Random - randomly select articles that match filter criteria",
                                     "Random Stratified - randomly select articles that match the filter criteria AND according to some strata (group)"),
                       inline=FALSE,
                       choiceValues = c("all","ranked","rankedStratified","random","randomStratified"))
        ),
        column(width = 4,
          uiOutput("weightedSampleOptions"),
          uiOutput("stratifiedSampleOptions"),
          uiOutput("sampleSize")
        ),
        column(width = 4,
          uiOutput("filterButton"),
          uiOutput("subsetSummary"),
          br(),
          uiOutput("downloadSubsetData")
        )
      ),
      hr(),
      h4("Filter Criteria"),
      fillRow(height="2000px", #this is so all drop down menu items fit
              width="100%",
              column(width=12,
                       uiOutput("filtJournal"),
                       uiOutput("filtIsOpen"),
                       uiOutput("filtYear"),
                       uiOutput("filtArticleType"),
                       uiOutput("filtMinCitation"),
                       uiOutput("filtTopic"))
      )

    )
  )
)

#-------------------------------------------------------------------------
# SIDEBAR MENU
#-------------------------------------------------------------------------

sideDash<-dashboardSidebar(
  sidebarMenu(
    id = "sidebarTabs",
    menuItem("Introduction", tabName = "intro", icon = icon("home")),
    menuItem("Search", tabName = "searchIn", icon = icon("search")),
    menuItem("Search Results", tabName = "searchOverview", icon = icon("book")),
    menuItem("Topic Discovery", tabName = "clusterAnalysis",icon = icon("spinner")),
    menuItem("Author Analysis", tabName = "authorAnalysis",icon = icon("user")),
    #menuItem("Sample Articles", tabName = "docSample",icon=icon("clone")),
    menuItem("Clear Analysis", tabName = "clearAnalysis",icon=icon("ban", class="ban2")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)

#-------------------------------------------------------------------------
# ALL TOGETHER
#-------------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title="Bibliometrix"),
  sideDash,
  body,
  skin="black"
)

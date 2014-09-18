library(shiny)
library(ape)

tr <- read.tree("./theriot2010.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

taxList <- as.list(1:N.tip)
names(taxList) <- tips

shinyUI(fluidPage(

titlePanel(h3("Diatom Phylogeny Browser", style="color:grey")),

sidebarLayout(#position="right",
sidebarPanel(
	#p("Browse by: ", style="color:#0066FF"),
	# choose type of subsetting
	radioButtons("decision", h6("Browse by:", style="color:#0066FF"), choices=list("Lineage" = 1, "Genus" = 2, "Custom" = 3), selected = 1),
	br(),

	# choose which lineage if type is "Lineage"
	conditionalPanel(condition= "input.decision == 1",
	selectInput("lineage", label = h6("Select lineage"),
					choices = list(
					"Raphid diatoms" = "Raphid diatoms", 
					"Pennate diatoms" = "Pennate diatoms", 
					"All diatoms" = "All diatoms"), selected = "All diatoms")),

	# choose which genus if type is "Genus"
	conditionalPanel(condition= "input.decision == 2",
	textInput("genus", label = h6("Which genus"), value="Gomphonema"),
	br(),
	br(),
	print(HTML("<p>If the genus is absent, the whole tree is displayed.</p> <p>If the genus is represented by a single species, a random sub-tree that contains that genus is displayed.</p>") )
	),

	# choose which taxa if input is "Custom"
	conditionalPanel(condition= "input.decision == 3",
	selectInput("select2", label = h6("Select first taxon"),
				choices = taxList, selected = 1),
	selectInput("select3", label = h6("Select second taxon"),
				choices = taxList, selected = 16),
	# currently doesn't do anything
	actionButton("get1","Plot sub-tree")),
	br(),
	br(),
	# download plot button
	downloadButton('downloadPlot', 'Export tree as pdf'),
	br(),
	br(),
	downloadButton('downloadTreeFile', 'Export tree as newick')	
		),

mainPanel(
	tabsetPanel(
		tabPanel("Tree",
			textOutput("Error"),
			br(),
			textOutput("TreeSummary"),
			plotOutput("Tree", height="1000px")),
		tabPanel("About", htmlOutput("textAbout"))
		)
	)					
)
))

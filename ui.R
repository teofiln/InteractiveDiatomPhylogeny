library(shiny)
library(ape)

tr <- read.tree("./Nakov_etal_2014ISMEJ.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

taxList <- as.list(1:N.tip)
names(taxList) <- tips

shinyUI(fluidPage(

titlePanel(h3("Diatom Phylogeny Browser", style="color:grey")),

sidebarLayout(#position="right",
sidebarPanel(width=3,
	#p("Browse by: ", style="color:#0066FF"),
	# choose type of subsetting
	radioButtons("decision", h6("Browse by:", style="color:#0066FF"), 
               choices=list("Lineage" = 1, "Genus" = 2, "Custom" = 3), 
               selected = 1),
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
	textInput("genus", label = h6("Select genus"), value="Gomphonema"),
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
	radioButtons("Character", h6("Mapped character:", style="color:#0066FF"), 
	             choices=list("Planktonic-Benthic" = "Habitat", 
                            "Solitary-Colonial" = "Colony"), 
	             selected = "Habitat"),
	br(),
	br(),
	# download plot button
	downloadButton('downloadPlot', 'Export tree as pdf'),
	br(),
	br(),
	downloadButton('downloadTreeFile', 'Export tree as newick')
		),

mainPanel(witdth=9,
	tabsetPanel(
		tabPanel("Tree",
			plotOutput("Tree", height=2000, width=1200)),
		tabPanel("About", htmlOutput("textAbout"))
		)
	)					
)
))

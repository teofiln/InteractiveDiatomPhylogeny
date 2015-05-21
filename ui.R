library(shiny)
library(shinythemes)
library(ape)

tr <- read.tree("./Nakov_etal_2014ISMEJ.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

taxList <- as.list(1:N.tip)
names(taxList) <- tips

shinyUI(fluidPage(theme = shinytheme("flatly"),

titlePanel("Diatom Phylogeny Browser"),

sidebarLayout(#position="right",
sidebarPanel(width=3,
	# choose type of subsetting
	radioButtons("decision", "Browse by:", 
               choices=list("Lineage" = 1, "Genus" = 2, "Custom" = 3), 
               selected = 1),
	br(),

	# choose which lineage if type is "Lineage"
	conditionalPanel(condition= "input.decision == 1",
	selectInput("lineage", label = "Select lineage",
					choices = list(
					"Raphid diatoms" = "Raphid diatoms", 
					"Pennate diatoms" = "Pennate diatoms", 
					"All diatoms" = "All diatoms"), selected = "Raphid diatoms")),

	# choose which genus if type is "Genus"
	conditionalPanel(condition= "input.decision == 2",
	textInput("genus", label = "Select genus", value="Gomphonema"),
	helpText("If the genus is absent, the whole tree is displayed. If the genus is represented by a single species, a random sub-tree that contains that genus is displayed.")
	),

	# choose which taxa if input is "Custom"
	conditionalPanel(condition= "input.decision == 3",
	selectInput("select2", label = "Select first taxon",
				choices = taxList, selected = 1),
	selectInput("select3", label = "Select second taxon",
				choices = taxList, selected = 16),
	# currently doesn't do anything
	actionButton("get1","Plot sub-tree")),
  br(),
	radioButtons("Character", "Mapped character:", 
	             choices=list("Planktonic-Benthic" = "Habitat", 
                            "Solitary-Colonial" = "Colony",
                            "Marine-Freshwater" = "Salinity"), 
	             selected = "Habitat"),
	br(),
		# download plot button
	downloadButton('downloadPlot', 'Export tree as pdf'),
	br(),
	br(),
	downloadButton('downloadTreeFile', 'Export tree as newick')
		),

mainPanel(witdth=9,
	tabsetPanel(
		tabPanel("Tree", plotOutput("Tree")),
		tabPanel("About", htmlOutput("textAbout"))
		)
	)					
)
))

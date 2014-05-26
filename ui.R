library(shiny)
library(ape)

tr <- read.tree("./theriot2010.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

taxList <- as.list(1:N.tip)
names(taxList) <- tips

shinyUI(fluidPage(

	titlePanel(h3("An interactive diatom phylogeny", style="color:grey")),

	sidebarLayout(#position="right",
		sidebarPanel(
			p("Select a lineage to explore or choose 'Custom' and select two diatoms to see the sub-tree starting with their most recent common ancestor", style="color:#0066FF"),
			br(),
			selectInput("select1", label = h6("Select lineage"),
						choices = list(
							"Custom" = "Custom",
							"Raphid diatoms" = "Raphid diatoms", 
							"Pennate diatoms" = "Pennate diatoms", 
#							"Araphid clade 1" = "Araphid clade 1", 
#							"Araphid clade 2" = "Araphid clade 2", 
#							"Polars clade 1" = "Polars clade 1",
#							"Polars clade 2" = "Polars clade 2",
#							"Polars clade 3" = "Polars clade 3",
#							"Radials clade 1" = "Radials clade 1",
#							"Radials clade 2" = "Radials clade 2",
							"All diatoms" = "All diatoms"), selected = "All diatoms"),
			br(), 
			selectInput("select2", label = h6("Select first taxon"),
						choices = taxList, selected = 1),
			selectInput("select3", label = h6("Select second taxon"),
						choices = taxList, selected = 16),
			actionButton("get1","Plot custom sub-tree")
		),

		mainPanel(
			tabsetPanel(
			tabPanel("Tree",
				plotOutput("Tree", height="1000px")),
#				textOutput("Text"))
#			tabPanel("Some clades"),
#			tabPanel("Studies"),
			tabPanel("About", htmlOutput("textAbout"))
			)
		)					
)
))

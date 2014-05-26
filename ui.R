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
			p("Select two diatoms and click 'Plot tree' to see the sub-tree starting with their most recent common ancestor"),
			br(),
			selectInput("select1", label = h6("Select first taxon"),
						choices = taxList, selected = 1),

			selectInput("select2", label = h6("Select second taxon"),
						choices = taxList, selected = 16),
			br(),
			actionButton("get1","Plot tree"),
#			br(),
			actionButton("get2","Restore default")
		),

		mainPanel(
			tabsetPanel(
			tabPanel("Big tree",
				plotOutput("bigTree", height="1000px")),
#				textOutput("Text"))
			tabPanel("Some clades"),
			tabPanel("Studies"),
			tabPanel("About", htmlOutput("textAbout"))
			)
		)					
)
))

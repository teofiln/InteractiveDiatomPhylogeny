library(shiny)
library(ape)
library(phangorn)
#source("prepTree.R")

tr <- read.tree("./theriot2010.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

subtr <- list()
subtr[(N.tip+1):(N.tip+N.node)] <- subtrees(tr)

taxList <- as.list(1:N.tip)
names(taxList) <- tips

shinyServer(function(input, output) {

whichTaxa <- reactive({
 	if(input$get1 == 0 ) {
 		xx <- subtr[[N.tip+1]] }
 	else {
	isolate({
		xx <- subtr[[mrca.phylo(tr, c(input$select1:input$select2))]]
	})
	}
	return(xx)
})

output$bigTree <- renderPlot({
		plot(ladderize(whichTaxa()),
			label.offset=0.001,
			cex=0.8,
			edge.width=2,
			edge.color="darkblue",
			tip.color="darkred")
	})

output$textAbout <- renderUI({
			HTML(
			"<p>This Shiny R code is meant as a tool to explore the diatom phylogeny. The main functionality so far is plotting sub-trees based on two user-selected terminal nodes. It depends on 'ape' and 'phangorn'. It is rather clunky at present time, but it does serve its basic purpose.</p> <p>The topology is that of Theriot et al. 2010 (Plant Ecology and Evolution). I plan to have a tree constructed from all available nSSU sequences here soon. This is because the user might be interested in taxa beyond the sampling of Theriot et al. 2010 and nSSU is still by far the most sequenced marker in diatoms.</p> <p>The 'Some clades' tab will have sub-trees of some recognized clades (e.g. raphid diatoms). The 'Studies' tab is meant as a repository of phylogenies from TreeBase or Dryad. Both are in develompent and I might abandon them. Also, the 'Restore default' button doesn't quite work. I haven't figured out how to do this yet.</p> <p>For an excellent implementation of ape::plot.phylo in Shiny with the functionality to load your own tree see https://github.com/KlausVigo/shinyTreeViewer/blob/master/ui.R.</p>"
			)
	})
})






















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

shinyServer(function(input, output, session) {

whichLineage <- reactive({	
			if (input$select1 == "Custom") {
			 		if (input$get1 == 0 ) {	finalTree <- subtr[[N.tip+1]] }
	 				else {	isolate({
							finalTree <- subtr[[mrca.phylo(tr, c(input$select2:input$select3))]]
					})	}	
			}	
			else {
				finalTree <- switch(input$select1,
					"Raphid diatoms" = subtr[[mrca.phylo(tr, c(101:136))]],
					"Pennate diatoms" = subtr[[mrca.phylo(tr, c(75:136))]],
#					"3" = c(101:136),
#					"4" = c(101:136),
#					"5" = c(101:136),
#					"6" = c(101:136),
#					"7" = c(101:136),
#					"8" = c(101:136),
#					"9" = c(101:136),
					"All diatoms" = subtr[[mrca.phylo(tr, c(1:136))]])
			}
			return(finalTree)
	})
		
output$Tree <- renderPlot({
	plot(ladderize(whichLineage()),
		label.offset=0.001,
		cex=0.8,
		no.margin=TRUE,
		edge.width=2,
		edge.color="darkblue",
		tip.color="darkred")
})

output$TreeSummary <- renderText({
	input$get1
	str1 <- as.character(Ntip(whichLineage()))
	str2 <- as.character(Nnode(whichLineage()))
	whichTaxa <- reactive({
	input$get1
		if (input$select1 != "Custom") {
			str3 <- gsub("_"," ", whichLineage()$tip.label[1])
			str4 <- gsub("_"," ", whichLineage()$tip.label[Ntip(whichLineage())])
			}
		else {
			isolate({
				str3 <- gsub("_"," ", tips[as.numeric(input$select2)])
				str4 <- gsub("_"," ", tips[as.numeric(input$select3)])
				})
			}
			return(c(str3, str4))
		})
	paste("Sub-tree composed of ", str1, "species and ", str2, 
	"internal nodes. Clade defined as the most recent common ancestor of ", 
	whichTaxa()[1], "and ", whichTaxa()[2], "and all of its descendants")

})
 	
output$textAbout <- renderUI({
			HTML(
			"<p>This Shiny R code is meant as a tool to explore the diatom phylogeny. The main functionality so far is plotting pre-defined sub-trees or sub-trees based on two user-selected terminal nodes. The code depends on the R packages <a href=http://ape-package.ird.fr>ape</a> and <a href=http://cran.r-project.org/web/packages/phangorn/index.html>phangorn</a>. The code is rather clunky, but it does serve its basic purpose.</p> <p>To run the Interactive Diatom Phylogeny use R and call it from its GitHub repository:</p> <p> <code>shiny::runGitHub('InteractiveDiatomPhylogeny', 'teofiln')</code></p> <p>The topology is that of Theriot et al. 2010 (Plant Ecology and Evolution). This will be replaced by a much larger tree constructed from all available diatom nSSU sequences soon.</p> <p>For an excellent treeviewer in Shiny with the functionality to load your own tree see <a href=https://github.com/KlausVigo/shinyTreeViewer/blob/master/ui.R>shinyTreeViewer</a>.</p>"
			)
	})
})






















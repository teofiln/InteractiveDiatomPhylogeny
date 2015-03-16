library(shiny)
library(ape)
library(phangorn)
#source("prepTree.R")

tr <- read.tree("./Nakov_etal_2014ISMEJ.newick")

N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

subtr <- list()
subtr[(N.tip+1):(N.tip+N.node)] <- subtrees(tr)

taxList <- as.list(1:N.tip)
names(taxList) <- tips

# list of genera
unique.genera <- unique(sort(unlist(lapply(strsplit(tips,"_"), function(x) x[1]))))
all.genera <- unlist(lapply(strsplit(tips,"_"), function(x) x[1]))

shinyServer(function(input, output, session) {

whichLineage <- reactive({	
	finalTree <- switch(input$lineage,
		
		"Raphid diatoms" = subtr[[mrca.phylo(tr, 
			c(min(which(all.genera=="Eunotia")),
			min(which(all.genera=="Surirella")) ) ) ]],
		
		"Pennate diatoms" = subtr[[mrca.phylo(tr, 
			c(min(which(all.genera=="Plagiogramma")),
			min(which(all.genera=="Surirella")) ) ) ]],
		
		"All diatoms" = subtr[[mrca.phylo(tr, c(1:N.tip))]] )
	
	return(finalTree)
})
		
whichGenus <- reactive({
	gen <- input$genus
	if (!gen %in% all.genera) {
		finalTree <- whichLineage()
		}
	else {
		g1 <- min(which(all.genera==gen))
		g2 <- max(which(all.genera==gen))
	
		if (g1!=g2) {
			finalTree <- subtr[[mrca.phylo(tr, c(g1:g2))]]
		}
	
		else {
			g2 <- sample(c(1:N.tip)[-g1],1)
			finalTree <- subtr[[mrca.phylo(tr, c(g1:g2))]]
		}
	}
	return(finalTree)
})

whichCustom <- reactive({

if (input$get1 == 0) {
	finalTree <- subtr[[mrca.phylo(tr, c(1:N.tip))]] 
	}

else { isolate({	
	finalTree <- subtr[[mrca.phylo(tr, c(input$select2:input$select3))]]
		})
	}

return(finalTree)
})

# deside which function to use to subset the tree
whatPlot <- reactive({
	
	if (input$decision == 2) {
		finalTree <- whichGenus() 
		}
	else if (input$decision == 3) {
		finalTree <- whichCustom() 
		}
	else {
		finalTree <- whichLineage() 
		}

	return(finalTree)
})

# function to plot the tree
treeplot <- function(x) {
	plot(ladderize(x),
		label.offset=0.001,
		cex=0.8,
		no.margin=TRUE,
		edge.width=2,
		edge.color="darkblue",
		tip.color="darkred",
		use.edge.length=FALSE)	}

#output$Tree <- reactive({

#if (is.null(whatPlot())) {
#	renderText({
#		print("There was a problem with the selection. Either the genus/species is not in the phylogeny, or the genus is represented by a single species.")
#		})
#	}  
#else {
#}
#})

output$Tree <- renderPlot({
#	input$get1
	treeplot(whatPlot())
})


# export plot 

output$downloadPlot <- downloadHandler(
	filename="Tree.pdf",
	
	content= function(file) {
		pdf(file, height=10, width=7)
		treeplot(whatPlot())
		dev.off()
	},
	
	contentType='application/pdf'

)

# export tree file

output$downloadTreeFile <- downloadHandler(
	filename="Tree_newick.txt",
	
	content= function(file) {
		write.tree(whatPlot(), file)
	},
	
	contentType='text/txt'

)

output$TreeSummary <- renderText({
	input$get1
	
	str1 <- as.character(Ntip(whatPlot()))
	str2 <- as.character(Nnode(whatPlot()))
	whichTaxa <- reactive({
	
	input$get1
		if (input$decision == 1) {
			str3 <- gsub("_"," ", whatPlot()$tip.label[1])
			str4 <- gsub("_"," ", whatPlot()$tip.label[Ntip(whatPlot())])
			}
		else if (input$decision == 2) {
			str3 <- gsub("_"," ", tr$tip.label[min(which(all.genera==input$genus))])
			str4 <- gsub("_"," ", tr$tip.label[max(which(all.genera==input$genus))])
			}	
		else {
	
	isolate({
			str3 <- gsub("_"," ", tr$tip.label[as.numeric(input$select2)] )
			str4 <- gsub("_"," ", tr$tip.label[as.numeric(input$select3)] )
		})
	}
	
	return(c(str3, str4))
})

paste(as.character(unlist(taxList)))
paste("Sub-tree composed of ", str1, "species and ", str2, 
"internal nodes. Clade defined as the most recent common ancestor of ", 
whichTaxa()[1], "and ", whichTaxa()[2], "and all of its descendants")

})

output$textAbout <- renderUI({
			HTML(
			"<p>This R Shiny code is meant as a tool to browse the diatom phylogeny. The main functionality so far is plotting pre-defined sub-trees, sub-trees based on genera, or sub-trees based on two user-selected terminal nodes. The code depends on the R packages <a href=http://ape-package.ird.fr>ape</a> and <a href=http://cran.r-project.org/web/packages/phangorn/index.html>phangorn</a>. The code is rather clunky, but it does serve its basic purpose.</p> <p>To run the Interactive Diatom Phylogeny use R and call it from its GitHub repository:</p> <p> <code>shiny::runGitHub('InteractiveDiatomPhylogeny', 'teofiln')</code></p> <p>The topology is that of Theriot et al. 2010 (Plant Ecology and Evolution). This will soon be replaced by a much larger tree constructed from diatom nSSU sequences.</p> <p>For an excellent treeviewer in Shiny with the functionality to load your own tree see <a href=https://github.com/KlausVigo/shinyTreeViewer/blob/master/ui.R>shinyTreeViewer</a>.</p>"
			)
	})
})


library(shiny)
library(ape)
library(phangorn)
library(geiger)
library(corHMM)

Traits <- read.csv("./Jan28_RECODED_282taxa_3traits.data_.csv", header=TRUE)
DAT <- data.frame(Traits[,3:5])
rownames(DAT) <- Traits[,2]

tr <- read.tree("./Nakov_etal_2014ISMEJ.newick")
N.tip <- Ntip(tr)
N.node <- Nnode(tr)
tips <- tr$tip.label

# make a list of subtrees
subtr <- list()
subtr[(N.tip+1):(N.tip+N.node)] <- subtrees(tr)

taxList <- as.list(1:N.tip)
names(taxList) <- tips

Cols <- c("steelblue", "firebrick")

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
whatTree <- reactive({
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

whatData <- reactive({
  Tree <- whatTree()
  Data <- treedata(phy = Tree, data = DAT, sort = TRUE, warnings = FALSE)$data
  Char <- switch(input$Character,
                 "Habitat" = Data[,1],
                 "Colony" = Data[,2],
                 "Salinity" = Data[,3])
  return(data.frame(Names=names(Char), Char))
})

getAncestors <- reactive({
  out <- rayDISC(phy = whatTree(), data = whatData(), model="ARD", node.states="marginal")
  return(out)
})

makeLabels <- reactive({
  Char <- whatData()[,2]
  names(Char) <- whatData()[,1]
  Char[which(Char==0)] <- "steelblue"
  Char[which(Char==1)] <- "firebrick"
  return(Char)
})

makeLegend <- reactive({
  switch(input$Character,
         "Habitat" = c("Planktonic", "Benthic"),
         "Colony" = c("Solitary", "Colonial"),
         "Salinity" = c("Marine", "Freshwater"))
})

# function to plot the tree
treePlot <- function(x) {
  plot(x,
       label.offset=Ntip(x)/200,
       cex=0.7,
       no.margin=TRUE,
       edge.width=0.75,
       edge.color="black",
       tip.color="black",
       use.edge.length=TRUE)
}

fPlotSize <- function() {
  Height <- Ntip(whatTree())*15
  return(Height)
}

output$Tree <- renderPlot({
  treePlot(whatTree())
  ancestors <- getAncestors()$states
  nodelabels(pie=ancestors, cex=0.3, piecol = Cols)
  tiplabels(pch=21, bg=makeLabels(), cex=2)
  legend(x=0, y=Ntip(whatTree())/2, makeLegend(), pch=21, pt.bg=Cols, bty="n", cex=2)
}, height = fPlotSize)

# export plot 
output$downloadPlot <- downloadHandler(
	filename="Tree.pdf",
	content= function(file) {
		pdf(file, height=10, width=7)
		treePlot(whatTree())
		ancestors <- getAncestors()$states
		nodelabels(pie=ancestors, cex=0.4, piecol = Cols)
		tiplabels(pch=21, bg=makeLabels(), cex=2)
		legend(x=0, y=Ntip(whatTree())/2, makeLegend(), pch=21, pt.bg=Cols, bty="n", cex=2)
		dev.off()
	},
	contentType='application/pdf'
)

# export tree file
output$downloadTreeFile <- downloadHandler(
	filename="Tree_newick.txt",
	content= function(file) {
		write.tree(what(), file)
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
			"<p>This R Shiny code is meant as a tool to browse the diatom phylogeny. The main functionality so far is plotting pre-defined sub-trees, sub-trees based on genera, or sub-trees based on two user-selected terminal nodes. Some trait data are also incorporated. The code depends on the R packages ape, phangorn, geiger and corHMM.</p> <p>To run the Interactive Diatom Phylogeny use R and call it from its GitHub repository:</p> <p> <code>shiny::runGitHub('InteractiveDiatomPhylogeny', 'teofiln')</code></p> <p>The topology is that of Nakov et al. 2014 (ISME-J, 9:246–255).</p> <p>For an excellent treeviewer in Shiny with the functionality to load your own tree see <a href=https://github.com/KlausVigo/shinyTreeViewer/blob/master/ui.R>shinyTreeViewer</a>.</p>"
			)
	})
})


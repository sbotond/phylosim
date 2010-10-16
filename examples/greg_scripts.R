plot.all.in.dir <- function(
  aln.dir,
  ...
) {

  files <- dir(aln.dir)  
  files <- files[grep("\\.(fa|fasta)$",files)]
  for (file in files) {
    file <- paste(aln.dir,file,sep="/")
    file.base <- gsub("\\.[^\\.]+$","",file)
    tree.f <- paste(file.base,".nh",sep="")
    if(!file.exists(tree.f)) {
      tree.f=NULL
    }
    out.f <- paste(file.base,".pdf",sep="")    
    print(paste("tree:",tree.f))
    print(paste("aln:",file))
    print(paste("out:",out.f))

    pdf(file=out.f)
    ggplot.aln(aln=file,tree=tree.f,plot.tree=T)
    dev.off()
  }
}

ggplot.aln <- function(
  aln,
  tree,
  plot.tree,
  ...
) {
  
  if (missing(plot.tree)) {
    plot.tree <- TRUE
  }
  if (missing(tree) || is.null(tree) || (is.character(tree) && !file.exists(tree))) {
    tree <- NULL
    plot.tree <- FALSE
    warn("[plot.aln] Tree not found!")
  } else if (is.character(tree)) {
    print("Reading tree...")
    tree <- read.tree(tree)
  }

  if (is.character(aln)) {
    print("Reading aln...")
    aln <- read.dna(file=aln,format='fasta',as.character=TRUE)
    aln <- toupper(aln)
  }

  sim <- PhyloSim()
  sim$.phylo <- tree
  sim$.alignment <- aln

  plot(sim,plot.tree=plot.tree,...)
}


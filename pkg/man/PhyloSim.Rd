%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{PhyloSim}
\docType{class}
\alias{PhyloSim}

 \alias{phylosim}

 \title{The PhyloSim class}

 \description{

	PhyloSim is an extensible object-oriented framework for the Monte Carlo simulation
	of sequence evolution written in 100 percent \code{R}.
	It is built on the top of the \code{\link{R.oo}} and \code{\link{ape}} packages and uses
	Gillespie's direct method to simulate substitutions, insertions and deletions.

	Key features offered by the framework:
	\itemize{
	\item  Simulation of the evolution of a set of discrete characters with arbitrary states evolving
		by a continuous-time Markov process with an arbitrary rate matrix.
	\item Explicit implementations of the most popular substitution models (for nucleotides, amino acids and codons).
 	\item Simulation under the popular models of among-sites rate variation, like the gamma (+G) and invariants plus gamma (+I+G) models.
	\item The possibility to simulate with arbitrarily complex patterns of among-sites rate variation by setting the site specific rates according to any \code{R} expression.
	\item Simulation with one or more separate insertion and/or deletion processes acting on the sequences, which sample the insertion/deletion length from an arbitrary discrete distribution or an \code{R} expression (so all the probability distributions implemented in \code{R} are readily available for this purpose).
	\item Simulation of the effects of variable functional constraints over the sites by site-process-specific insertion and deletion tolerance parameters, which determine the rejection probability of a proposed insertion/deletion.
	\item The possibility of having a different set of processes and site-process-specific parameters for every site, which allow for an arbitrary number of partitions in the simulated data.
	\item Simulation of heterotachy and other cases of non-homogeneous evolution by allowing the user to set "node hook" functions altering the site properties at internal nodes of the phylogeny.
	\item The possibility to export the counts of various events ("branch statistics") as phylo objects (see \code{\link{exportStatTree.PhyloSim}}).
	}

	General notes:
	\itemize{
	\item The \code{Sequence} objects have no "immortal links". The simulation
	is aborted if the sequence length shrinks to zero. It is up to the user
	to choose sensible indel rates and sequence lengths to prevent that.
	\item The sites near the beginning and end of the sequences have less sites proposing
	insertion and deletion events around the so the insertion and deletion processes
	have an "edge effect". The user can simulate
	realistic flanking sequences to alleviate the edge effect in the simulation settings where
	it may be an issue.
 }

	Notes on performance:
	\itemize{
	\item The pure \code{R} implementation offers felxibility, but also comes
	with a slower simulation speed. If the \code{PSIM_FAST} object is present in the environment, a "fast & careless"
	mode is enabled. In this mode most of the error checking is skipped, increasing the speed.
	It is recomended that simulations are only run in fast mode if you are sure that the simulation
	settings are free from errors. It is probably a good practice to set up the simulations in normal mode
	with short sequences and enable fast mode when running the actual simulation with long sequences.
	\item Please note, that no "branch statistics" are saved in fast mode.
	\item Logging also has a negative impact on performance, so it's not a good idea to run
	large simulations with the logging enabled.
	\item The time needed to run a simulation depends not only on the number of the sites,
	but also on the length of the tree.
	\item Constructing \code{Sequence} objects with large number of sites is expensive. Avoid doing
	that inside a cycle.
	\item In the case of \code{Sequence} objects with a large number of sites (more than 10 000) the
	amount of available memory can be limiting as well.
  }

	The examples below demonstrate only some more common simulation settings,
	the framework offers much more flexibility. See the package
	vignette (\code{vignette("PhyloSim",package="phylosim")}) and the
	examples directory (\url{http://github.com/sbotond/phylosim/tree/master/examples/})
	for additional examples.

	Package:   \cr
\bold{Class PhyloSim}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{PhyloSim}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{PhyloSim}\cr
extends \link{PSRoot}\cr


 }


 \references{
	Gillespie, DT (1977) Exact stochastic simulation of coupled chemical reactions -
	J. Phys. Chem. 81 (25):2340-2361 \url{http://dx.doi.org/10.1021/j100540a008}
 }

 \usage{
PhyloSim(phylo=NA, root.seq=NA, name=NA, log.file=NA, log.level=-1, ...)
}

 \arguments{
 	\item{phylo}{A rooted phylo object, constructed by the APE package.}
 	\item{root.seq}{A valid Sequence object with Process objects attached. Used as the starting sequence during simulation.}
 	\item{name}{The name of the object (a character vector of length one).}
 	\item{log.file}{Name of the file used for logging.}
 	\item{log.level}{An integer specifying the verbosity of logging (see \code{\link{setLogLevel.PhyloSim}}).}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{attachHookToNode} \tab  -\cr
 \tab \code{attachSeqToNode} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{Debug} \tab  -\cr
 \tab \code{detachHookFromNode} \tab  -\cr
 \tab \code{detachSeqFromNode} \tab  -\cr
 \tab \code{exportStatTree} \tab  -\cr
 \tab \code{getAlignment} \tab  -\cr
 \tab \code{getAlignmentLength} \tab  -\cr
 \tab \code{getBranchEvents} \tab  -\cr
 \tab \code{getEdge} \tab  -\cr
 \tab \code{getEdges} \tab  -\cr
 \tab \code{getId} \tab  -\cr
 \tab \code{getLogFile} \tab  -\cr
 \tab \code{getLogLevel} \tab  -\cr
 \tab \code{getName} \tab  -\cr
 \tab \code{getNedges} \tab  -\cr
 \tab \code{getNodes} \tab  -\cr
 \tab \code{getNtips} \tab  -\cr
 \tab \code{getPhylo} \tab  -\cr
 \tab \code{getRootNode} \tab  -\cr
 \tab \code{getRootSeq} \tab  -\cr
 \tab \code{getSeqFromNode} \tab  -\cr
 \tab \code{getSequences} \tab  -\cr
 \tab \code{getTipLabels} \tab  -\cr
 \tab \code{getTips} \tab  -\cr
 \tab \code{getTreeLength} \tab  -\cr
 \tab \code{is.tip} \tab  -\cr
 \tab \code{Log} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{readAlignment} \tab  -\cr
 \tab \code{readTree} \tab  -\cr
 \tab \code{saveAlignment} \tab  -\cr
 \tab \code{scaleTree} \tab  -\cr
 \tab \code{setAlignment} \tab  -\cr
 \tab \code{setBranchEvents} \tab  -\cr
 \tab \code{setEdges} \tab  -\cr
 \tab \code{setId} \tab  -\cr
 \tab \code{setLogFile} \tab  -\cr
 \tab \code{setLogLevel} \tab  -\cr
 \tab \code{setName} \tab  -\cr
 \tab \code{setNedges} \tab  -\cr
 \tab \code{setNodes} \tab  -\cr
 \tab \code{setNtips} \tab  -\cr
 \tab \code{setPhylo} \tab  -\cr
 \tab \code{setRootNode} \tab  -\cr
 \tab \code{setRootSeq} \tab  -\cr
 \tab \code{setSequences} \tab  -\cr
 \tab \code{setTipLabels} \tab  -\cr
 \tab \code{setTips} \tab  -\cr
 \tab \code{setTreeLength} \tab  -\cr
 \tab \code{Simulate} \tab  -\cr
 \tab \code{summary} \tab  -\cr
 \tab \code{Undocumented} \tab  -\cr
}


 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
   set.seed(1)
	## The following examples demonstrate
	## the typical use of the framework.
	## See the package vignette and
	## \url{http://github.com/sbotond/phylosim/tree/master/examples/}

	## The ll() method gives information about the methods defined
	## in the immediate class of an object.
	## Useful when exploring the framework.

	s<-Sequence()
	ll(s)
	ll(PhyloSim())
	ll(GTR())

	## Example 1 - A short simulation:
	## simulate nucleotide seqeunces and display
	## the resulting alignment matrix.

	Simulate(
		PhyloSim(phy=rcoal(3),
       root=NucleotideSequence(string="ATGC", proc=list(list(JC69())) ) )
	)$alignment

	# Construct a phylo object for the following
	# simulations, scale total tree length to 1:

	tmp<-PhyloSim(phylo=rcoal(3))
	scaleTree(tmp,1/tmp$treeLength)
	tmp$treeLength
	t<-tmp$phylo

	## Example 3 - simulating rate variation,
	## insertions and deletions.
	## See the examples/example_3_clean.R file
	## in the phylosim GitHub repository.

	# construct a GTR process object
       gtr<-GTR(
		name="MyGTR",
               rate.params=list(
                       "a"=1, "b"=2, "c"=3,
                       "d"=1, "e"=2, "f"=3
               ),
               base.freqs=c(2,2,1,1)/6
       )
	# get object summary
	summary(gtr)
	# display a bubble plot
	plot(gtr)

	# construct root sequence object
	s<-NucleotideSequence(length=40)
	# attach process via virtual field
	s$processes<-list(list(gtr))
	# sample states from the equilibrium
	# distribution of the attached processes
	sampleStates(s)
	# create among-site rate variation by sampling
	# the "rate.multiplier" site-process-specific parameter
	# from a discrete gamma distribution (GTR+G).
	plusGamma(s,gtr,shape=0.1)
	# make the range 11:20 invariable
	setRateMultipliers(s,gtr,0,11:20)
	# get the rate multipliers for s and gtr
	getRateMultipliers(s,gtr)

	# proposing lengths in the range 1:3
	d<-DiscreteDeletor(
		rate=0.1,
		name="MyDel",
		sizes=c(1:3),
		probs=c(3/6,2/6,1/6)
	)
	# get object
	summary(d)
	# plot deletion length distribution
	plot(d)
	# attach deletion process d to sequence s
	attachProcess(s,d)
 	# create a region rejecting all deletions
       setDeletionTolerance(s,d,0,11:20)

	# construct an insertion process object
	# proposing lengths in the range 1:3
	i<-DiscreteInsertor(
		rate=0.1,
		name="MyDel",
		sizes=c(1:2),
		probs=c(1/2,1/2),
		template.seq=NucleotideSequence(length=1,processes=list(list(JC69())))
	)
 	# states will be sampled from the JC69 equilibrium distribution
	# get object
	summary(i)
	# plot insertion length distribution
	plot(i)
	# attach insertion process i to sequence s
	attachProcess(s,i)
 	# create a region rejecting all insertions
       setInsertionTolerance(s,i,0,11:20)

	# plot total site rates
	plot(s)
	# construct simulation object
	sim<-PhyloSim(root.seq=s, phylo=t)
	# get object summary
	summary(sim)
	# plot tree
	plot(sim)
	# run simulation
	Simulate(sim)
 	# get the list of recorded per-branch event counts
 	getBranchEvents(sim)
	# export the number of substitutions as a phylo object
	subst<-exportStatTree(sim,"substitution")
	# plot the exported phylo object
	plot(subst)
	# plot tree and alignment
	plot(sim)
	# save and display alingment
	file<-paste("PhyloSim_dummy_fasta_",Sys.getpid(),".fas",sep="");
	saveAlignment(sim,file=file,paranoid=TRUE);
	# print out the Fasta file
	cat(paste(scan(file=file,what=character(),sep="\n"),collapse="\n"));cat("\n");
	# delete Fasta file
	unlink(file);

   # See \url{http://github.com/sbotond/phylosim/tree/master/examples/examples_class.R}
   # for the full list of PhyloSim constructor examples.
 
	## See the package vignette and
	## the GitHub repository for even more examples.
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
	\code{\link{PSRoot} \link{Alphabet} \link{AminoAcidAlphabet}
	\link{AminoAcidSequence} \link{AminoAcidSubst}
	\link{AnyAlphabet} \link{BinaryAlphabet} \link{BinarySequence}
	\link{BinarySubst} \link{BrownianInsertor} \link{CodonAlphabet}
	\link{CodonSequence} \link{CodonUNREST} \link{ContinuousDeletor}
	\link{ContinuousInsertor} \link{cpREV} \link{DiscreteDeletor}
	\link{DiscreteInsertor} \link{Event} \link{F81} \link{F84}
	\link{FastFieldDeletor} \link{GeneralDeletor}
	\link{GeneralInDel} \link{GeneralInsertor} \link{GeneralSubstitution}
	\link{GTR} \link{GY94} \link{HKY} \link{JC69} \link{JTT} \link{JTT.dcmut}
	\link{K80} \link{K81} \link{LG} \link{mtArt} \link{mtMam} \link{mtREV24}
	\link{MtZoa} \link{NucleotideAlphabet} \link{NucleotideSequence} \link{PAM}
	\link{PAM.dcmut} \link{PhyloSim} \link{Process} \link{QMatrix} \link{Sequence}
	\link{Site} \link{T92} \link{TN93} \link{UNREST} \link{WAG}}
 }

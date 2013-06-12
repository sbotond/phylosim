%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{sampleStates.Sequence}
\alias{sampleStates.Sequence}
\alias{Sequence.sampleStates}
\alias{sampleStates,Sequence-method}

 \title{Sample the states for a collection of Site objects aggregated by a Sequence object}

 \description{
	Sample the states for a collection of Site objects aggregated by a Sequence object.
	This method samples new states from the equlibrium distribution of the attched process(es) for sites
	having undefined states (NA).
	If a site has more than one substitution process attached, then the method samples the new state from the
	mixture of equlibrium distributions. The weight of each equlibrium distribution is proportional to the
	site-process specific rate multiplier of the corresponding process at the given site.

	Sites having defined states are not touched. All sites with undefined states must have at least one
	substitution process (object inheriting from GeneralSubstitution) attached.
 }

 \usage{
\method{sampleStates}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a nucleotide sequence
	s<-NucleotideSequence(length=80)
	# create some processes
	jc69<-JC69(); 				# Jukes-Cantor
	hky<-HKY(base.freqs=c(0.1,0.4,0.1,0.4)) # "GC-rich" HKY
	# attach the processes
	s$processes<-list(list(jc69)) # jc is attached to all sites
	attachProcess(s,hky,60:80)  # hky is attached to range 60:80
	# tweak rate multiplier for hky
	setRateMultipliers(s,hky,10,60:80)
	# set states in range 1:20
	setStates(s,"A",1:20)
	# sample remaining states
	sampleStates(s)
	# print sequence
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	sampleState.GeneralSubstitution GeneralSubstitution
 }

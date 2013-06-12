%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.PhyloSim}
\alias{plot.PhyloSim}
\alias{PhyloSim.plot}
\alias{plot,PhyloSim-method}

 \title{Plot a PhyloSim object}

 \description{
	Plot a PhyloSim object.

       This method plots the aggregated alignment alongside the tree used for simulation. Various options
       allow for control over the plot style.

 }

 \usage{
\method{plot}{PhyloSim}(
    x, plot.tree, plot.ancestors, plot.chars, plot.legend, 
    plot.labels, aspect.ratio, num.pages, char.text.size, 
    axis.text.size, color.scheme, color.branches, tree.xlim,
    aln.xlim, tracks, aln.length.tolerance, plot.nongap.bl, ...
    )
}

 \arguments{
 	\item{x}{A PhyloSim object.}
 	\item{plot.tree}{Whether to plot the tree alongside the alignment. TRUE or FALSE; defaults to TRUE.}
 	\item{plot.ancestors}{Whether to plot the ancestral sequences. TRUE or FALSE; defaults to TRUE.}
 	\item{plot.chars}{Whether to plot the actual text of the characters.}
 	\item{plot.legend}{Whether to plot the legend showing the character-to-color mapping.}
 	\item{plot.labels}{Whether to plot the sequence labels along the y-axis}
 	\item{aspect.ratio}{(Experimental; when set, this option forces the num.pages value to 1) Constrains the alignment residues to have a certain aspect ratio; values above 1 cause vertically-stretched blocks. FALSE disables aspect ratio control, numerical values set the aspect ratio; defaults to FALSE.}
 	\item{num.pages}{Optionally split the alignment over a number of vertically-stacked pages. This is useful for long alignments. 'auto' chooses a sensible number of pages, numerical values specify a number; defaults to 'auto'.}
 	\item{char.text.size}{Text size for the aligned characters. This may require tweaking depending on the DPI and output format. Defaults to 'auto'.}
 	\item{axis.text.size}{Text size for the sequence labels along the y-axis. This may require tweaking depending on the DPI and output format. Defaults to 'auto'.}
       \item{color.scheme}{Color scheme to use ("auto", "binary", "dna", "protein", "codon", "combined", "combined_codon"). Defaults to 'auto'. When set to 'auto', the function will choose an appropriate coloring scheme based on the alignment content.}
       \item{color.branches}{The event count used to color the branches ("substitutions" by default). See \code{\link{getBranchEvents.PhyloSim}}.}
       \item{tree.xlim}{The x-axis limits of the tree panel.}
       \item{aln.xlim}{The x-axis limits of the alignment panel (in alignment column coordinates).}
       \item{tracks}{Tracks to display above or below the alignment as colored blocks.

 The input format for tracks is a list of data frames with the following possible fields, all of which are optional and can be omitted:
 \itemize{
 \item pos - the sequence position (starting with 1) of the feature. Defaults to NULL.
 \item score - the score (between 0 and 1) of the feature. Scores above 1 or below zero will be truncated.
 Defaults to 1.
 \item y_lo - the lower Y offset (between 0 and 1) of the feature. Defaults to 0. Use a y_lo and y_hi
 value for each row in the track data frame to create a wiggle plot like effect.
 \item y_hi - the upper Y offset (between 0 and 1) of the feature. Defaults to 1. Use just a y_hi value
 for each row in the track data frame to create a bar plot like effect.
 \item [the fields below are considered unique per track; the values from the first row in the track
 data frame are used.]
 \item id - the display ID for the track. Defaults to 'Track'.
 \item layout - set to 'above' to put the track above the alignment, 'below' for below.
 \item height - the number of alignment rows for the track to span in height. Defaults to 3.
 \item color.gradient - a comma-separated list of colors to interpolate between when coloring
 the blocks. Examples: 'white,red' 'blue,gray,red' '#FF00FF,#FFFFFF'. Defaults to 'white,black'.
 \item color - a single color to use when coloring the blocks. Mutually exclusive with color.gradient,
 and if a color.gradient value exists then this value will be ignored. Defaults to black.
 \item background - a color for the background of the track. Defaults to white.
 }}
       \item{aln.length.tolerance}{The desired alignment/sequence length ratio (A/S ratio) to trim the alignment to.
       The A/S ratio is defined as the ratio between the alignment length and the mean ungapped sequence length, and
       the alignment trimming procedure will remove blocks of indel-containing columns (in a sensible order) until
       either (a) the desired indel tolerance is reached, or (b) no more columns can be removed without yielding an empty
       alignment. A track is added below the alignment to indicate how many indels each resulting alignment column used
       used to harbor, and black squares are overlaid onto the alignment where extant sequence data has been trimmed.
       Defaults to NULL (no trimming); values in the range of 0.9 to 1.3 tend to work well at improving the
       legibility of very gappy alignments.}
       \item{plot.nongap.bl}{If set to TRUE, plots the non-gap branch length (defined as the branch length of the subtree of non-gapped sequences) as a track below the alignment. Defaults to FALSE.}
 	\item{...}{Not used.}
 }

 \value{
 	The PhyloSim object (invisible).
 }

 \examples{
	# Create a PhyloSim object.
	# Provide the phylo object
	# and the root sequence.
	sim<-PhyloSim(
		name="TinySim",
		phylo=rcoal(3),
		root.seq=NucleotideSequence(string="ATGCTAGCTAGG",processes=list(list(JC69())))
	);
       # plot the aggregated phylo object
       plot(sim)
	# run simulation
	Simulate(sim)
       # Plot the alignment without the tree or ancestral sequences.
       plot(sim, plot.ancestors=FALSE, plot.tree=FALSE)
       # Force a DNA-based color scheme
       # (default is 'auto' to auto-detect based on the sequence composition)
       plot(sim, color.scheme='dna', plot.legend=TRUE)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{rescaleQMatrix.ToleranceSubstitution}
\alias{rescaleQMatrix.ToleranceSubstitution}
\alias{ToleranceSubstitution.rescaleQMatrix}
\alias{rescaleQMatrix,ToleranceSubstitution-method}

 \title{Rescale the scaled rate matrix of a QMatrix object aggregated by a ToleranceSubstitution object}

 \description{
	Rescale the scaled rate matrix of a QMatrix object aggregated by a ToleranceSubstitution object.

	The QMatrix objects aggregated by the ToleranceSubstitution objects store two rate matrices: one containes
	the rates provided by the user (unscaled rate matrix), the other matrix (scaled rate matrix) is rescaled to have the
	expected number of subsitutions per unit time equal to one when the process is at equlibrium.
	This method performes the re-scaling of the scaled rate matrix in the associated QMatrix object based on
	the equlibrium distribution and the unscaled rate matrix.

	This method is mainly used internally as the scaled matrix is rescaled every time the unscaled matrix
	is modifed.

 }

 \usage{
\method{rescaleQMatrix}{ToleranceSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE.
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
	# rescale rate matrix
	rescaleQMatrix(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }

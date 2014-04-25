%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{clone.ToleranceSubstitution}
\alias{clone.ToleranceSubstitution}
\alias{ToleranceSubstitution.clone}
\alias{clone,ToleranceSubstitution-method}

 \title{Clone a ToleranceSubstitution object}

 \description{
	Clone a ToleranceSubstitution object.

	This method also clones the aggregated QMatrix object, but not the aggregated Alphabet
	object, as that is a good target for recycling.
 }

 \usage{
\method{clone}{ToleranceSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A ToleranceSubstitution object.
 }

 \examples{
	# create a ToleranceSubstitution object
	p<-ToleranceSubstitution(
                           alphabet=BinaryAlphabet(),
                           rate.list=list("0->1"=1,"1->0"=2),
                           name="MyBinary"
                           )
	# clone p
	pp<-clone(p)
	# do some checks
	p;pp
	p == p
	p == pp
	equals(p$qMatrix, pp$qMatrix)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }

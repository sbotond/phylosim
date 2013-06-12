%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{summary.BrownianInsertor}
\alias{summary.BrownianInsertor}
\alias{BrownianInsertor.summary}
\alias{summary,BrownianInsertor-method}

 \title{Summarize the properties of an object}

 \description{
       Summarize the properties of an object.
 }

 \usage{
\method{summary}{BrownianInsertor}(object, ...)
}

 \arguments{
       \item{object}{An object}
       \item{...}{Not used.}
 }

 \value{
  Returns a PSRootSummary object.
 }

 \examples{

       # create an object
	p<-BrownianInsertor(
                       type="discrete",
                       scale=0.05,
                       sizes=1:4,
                       probs=c(3/6,1/6,1/6,1/6),
                       rate=0.05
                       )
       # get a summary
       summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{BrownianInsertor}}.
 }

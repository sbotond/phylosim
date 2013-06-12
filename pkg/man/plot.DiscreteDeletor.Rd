%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.DiscreteDeletor}
\alias{plot.DiscreteDeletor}
\alias{DiscreteDeletor.plot}
\alias{plot,DiscreteDeletor-method}

 \title{Plot the deletion length distribution}

 \description{
	Plot the deletion length distribution.
 }

 \usage{
\method{plot}{DiscreteDeletor}(x, ...)
}

 \arguments{
 	\item{x}{A DiscreteDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	The DiscreteDeletor object (invisible).
 }

 \examples{
	d<-DiscreteDeletor(
		name="MyDiscDel",
		sizes=1:6,
		probs=c(0.25000000, 0.16666667, 0.08333333, 0.08333333, 0.16666667, 0.25000000)
	)
	# plot the deletion length distribution
	plot(d)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{DiscreteDeletor}}.
 }

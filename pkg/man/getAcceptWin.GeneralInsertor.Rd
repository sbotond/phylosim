%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAcceptWin.GeneralInsertor}
\alias{getAcceptWin.GeneralInsertor}
\alias{GeneralInsertor.getAcceptWin}
\alias{getAcceptWin,GeneralInsertor-method}

 \title{Get the size of the acceptance window}

 \description{
	Get the size of the acceptance window

	This parameter determines the number of sites neighbouring the position (from left and right) of the insertion considered when accepting/rejecting
	a proposed insertion. The "insertion.tolerance" parameter is retrived from sites falling in the window specified by this parameter.
	The default value is 1, so the two neighbouring sites are considered by default.

 }

 \usage{
\method{getAcceptWin}{GeneralInsertor}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a GeneralInsertor object
	i<-GeneralInsertor(rate=0.5);
	# get acceptance window size
	getAcceptWin(i)
	# get acceptance window size via virtual field
	i$acceptWin
	# set acceptance window size
	setAcceptWin(i,2)
	# set acceptance window size via virtual field
	i$acceptWin<-3
	i$acceptWin
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInsertor}}.
 }

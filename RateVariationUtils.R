##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	

##	
## Method: plusGamma
##	
setMethodS3(
	"plusGamma", 
	class="Sequence", 
	function(
		this,
		process,
		shape,	
		index,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		if(!is.GeneralSubstitution(process)){
			throw("The sepcified process is not a substitution process!\n");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		else {
		
			if(missing(index)){
				index<-seq(along=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}

			setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(length(index),shape=shape,rate=shape),index=index);
	
		}
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plusInvGamma
##	
setMethodS3(
	"plusInvGamma", 
	class="Sequence", 
	function(
		this,
		process,
		pinv,
		shape,	
		index,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		if(!is.GeneralSubstitution(process)){
			throw("The sepcified process is not a substitution process!\n");
		}
		else if(missing(pinv)){
			throw("No proportion of invariant sites given!\n");
		}
		else if(!all(is.numeric(pinv)) | length(pinv) != 1){
			throw("The pinv parameter must be a numeric vector of lenght 1!\n");	
		}
		else if(pinv > 1){
			throw("Tpe proportion of invariant sites cannot be larger than 1.!");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		else {
		
			if(missing(index)){
				index<-seq(along=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}

			# Iterating over the sites specified by the index vector:
			
			for(site in index){

				# Choose between invariant and gamma:
				type<-sample(c("INV","GAMMA"),size=1, replace=FALSE, prob=c( pinv, (1-pinv) ) );

				if(type == "INV"){
					setParameterAtSites(this, process=process, id="rate.multiplier",value=0,index=c(site));
				}
				else {
					setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(1,shape=shape,rate=shape),index=c(site));
				}
		
			}

	
		}
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

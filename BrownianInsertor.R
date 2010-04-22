##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
  "BrownianInsertor",
  function( 
		name="Anonymous",
		type="discrete",
		... 
		)	{

		if(type == "continous"){
			this<-ContinousInsertor(
			 ...
			);
		}
		else if (type == "discrete") {
			this<-DiscreteInsertor(
			 ...
			);
		}
		else {
			throw("Invalid insertor process type!\n");
		}
    
		this<-extend(
      			this,
      			"BrownianInsertor",
			.scale = 0.001,
			.type  = type
    		);
		
		# Using virtual field to clear Id cache:
		this$name<-name;

		this$generateBy<-function(process=NA,length=NA,target.seq=NA,event.pos=NA,insert.pos=NA){
	 
			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	
			
			if(is.na(target.seq)){
				return(NA);
			}
	
			# The start and end of the Brownian path:

			start;
			end;
			proc<-list();
			
			if( (event.pos == 1) || (event.pos == target.seq$.length) ){

				start<-clone(target.seq$.sites[[event.pos]]);
				start$.state=NA;
				end<-clone(start);
				proc<-getProcesses(start);
				
			} else {

				start<-clone(target.seq$.sites[[insert.pos]]);
				start$.state=NA;
				end<-clone(target.seq$.sites[[insert.pos + 1]]);
				end$.state=NA;
			
				proc.start<-getProcesses(start);
				proc.end<-getProcesses(end);

				# Calculate the intersection of process list:
			
				proc<-PSRoot$intersect.list(proc.start,proc.end);

				start$processes<-proc;
				start$processes<-proc;
				
				# No common processes:

				if(length(proc) == 0){
					start<-sample(c(start,end),1);
					end<-clone(start);
					proc<-getProcesses();
				}

			}
		

			# Create the insert sequence:			

			class.seq<-class(target.seq)[[1]];
			insert<-do.call(class.seq,list(length=length));
			setProcesses(this=insert,value=list(proc),sloppy=TRUE);

			# For every process...
			
			for (p in proc){
				
				# ... and site specific parameter:	
				for(param in getSiteSpecificParamIds(p)){
					
					start.value<-getParameterAtSite(p,site=start,id=param)$value;
					end.value<-getParameterAtSite(p,site=end,id=param)$value;
					
					path<-seq(from=start.value,to=end.value,length.out=(insert$.length + 2));
					path<-path[2:(length(path)-1)];
					brownian.path<-abs(PSRoot$BrownianPath(p=path, a=this$.scale));
		
					# Tolerance values are probabilities, do not alow them to wander beyond 1:	
					if(param == "insertion.tolerance" || param == "deletion.tolerance"){
						brownian.path[which(brownian.path > 1)]<-1;	
					}

					setParameterAtSites(
						insert,
						process	= p,
						id	= param,
						value	= brownian.path 
					);	
					
				}
			}

			return(insert);

				
	 	}

		return(this);
  },
  enforceRCC=TRUE
);

##
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="BrownianInsertor",
  function(
    this,
    length,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

			# FIXME

      }
      tryCatch(may.fail(this),finally=this$writeProtected<-wp);
      NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: BrownianPath.PSRoot
##
setMethodS3(
  "BrownianPath",
  class="PSRoot",
  function(
	this,
	p=NA,
	a=NA,
    	...
  ){

	generate_brownian<-function(length, a){
		cumsum(rnorm(length,0,sd=a));
	}

	generate_bridge <- function (length,a){
		b <- generate_brownian(length,a)
		b - (1:length)/(length) * b[length]
	}

	generate_path <- function (p,a){
		n <- length(p);
		b <- generate_bridge (n+1,a);
		p + b[1:n];
	}

	return(generate_path(p,a));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
        static=TRUE,
  conflict="warning"
);


##	
## Method: getType
##	
setMethodS3(
	"getType", 
	class="BrownianInsertor", 
	function(
		this,
		...
	){

		this$.type;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setType
##	
setMethodS3(
	"setType", 
	class="BrownianInsertor", 
	function(
		this,
		new_type,
		...
	){

		throw("The type of the BrownianInsertor objects can be set only from the constructor!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getScale
##	
setMethodS3(
	"getScale", 
	class="BrownianInsertor", 
	function(
		this,
		...
	){

		this$.scale;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setScale
##	
setMethodS3(
	"setScale", 
	class="BrownianInsertor", 
	function(
		this,
		value,
		...
	){


		.checkWriteProtection(this);	
		if(!is.numeric(value) || (length(value) != 1)){
			throw("The value of the scale paramter must be a numeric vector of length 1!\n");
		}
		this$.scale<-value;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);



##
## Method: summary
##
setMethodS3(
  "summary",
  class="BrownianInsertor",
  function(
    object,
    ...
  ){
	this<-object;
    	.addSummaryNameId(this);
	this$.summary$"Type"<-this$.type;
	this$.summary$"Brownian path scale parameter"<-this$.scale;
	
    	NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


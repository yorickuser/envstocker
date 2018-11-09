#' stock and load execution environments of functions for debugging
#' @aliases envstocker envstocker-package
#' @keywords internal
"_PACKAGE"


       
##' This function converts a list of environments to a normal list 
##' @title convert list of environments to a normal list
##' @param elist list of environment
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' .ee.e2list(.ee)
#' @export
.ee.e2list <- function(elist){
    el=elist;
    for(i in 1:length(el))el[[i]]=as.list(el[[i]]);
    return(el);
}


##' This function shows the stracture of ".ee.get()" by "ls.str" function instead of "ls". 
##' @title show structure of stocked environments by "ls.str"
##' @param n integer : if not specified, then only the last environment is returned. If 0, all environments are returned. If positive, a list from [length(.ee)-n +1] to length(.ee) is returned. If negative, a list from 1 to -n is returned.
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' .ee.ls.str()
##' .ee.ls.str(0)
#' @export
.ee.ls.str <- function(n=1){
    return(ls.str(.ee.get(n=n)));
}

##' This function is a short-name varsion of ".ee.ls.str()". 
##' @title a short-name varsion of ".ee.ls.str()". 
#' @export
.ee. = .ee.ls.str



##' This function shows the stracture of ".ee.get()" 
##' @title show structure of stocked environments
##' @param n integer : if not specified, then only the last environment is returned. If 0, all environments are returned. If positive, a list from [length(.ee)-n +1] to length(.ee) is returned. If negative, a list from 1 to -n is returned.
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' 
##' .ee.str()
##' .ee.str(0)
#' @export
.ee.str <- function(n=1){
    return(str(.ee.get(n=n)));
}
    
##' This function converts from ".ee" to a normal list 
##' @title converts to list
##' @param n integer : if not specified, then only the last environment is returned. If 0, all environments are returned. If positive, a list from [length(.ee)-n +1] to length(.ee) is returned. If negative, a list from 1 to -n is returned.
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' 
##' .ee.get()
##' .ee.get(0)
##' .ee.get(2)
##' .ee.get(-2)
#' @export
.ee.get <- function(n=1){
    e=.ee;
    if(n>0){
        st=max(1,(length(e)-n+1));
        ed=length(e);
    }
    if(n<0){
        st=1;
        ed=min(-1*n,length(e));
    }
    if(n==0){
        st=1;
        ed=length(e);
        }
    return((.ee.e2list(e[st:ed])));
}
    
##' This function registers the execution environment of the focal funtion to stock list ".ee".
##' @title register excecution environment of focal function
##' @param fname character : function name.
##' @param env environment : usually "environment()".
##' @param i integer : used to additional information to environment names.
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' .ee.str(0)
#' @export
.ee.append <- function(fname,env,i){
                   
    env1=env;
    attr(env1,"name")=fname;
    eb=list(env1);
    count=0;
    .ee=get(".ee",envir=.GlobalEnv);
    
    for(j in 1:length(.ee)){
        if(attr(.ee[[j]],"name")==fname)count=count+1;
    }
    
    if(missing(i)){
        if(count==0)names(eb)=fname
        else names(eb)=paste(fname,(count+1),sep=".")
    }else {
        names(eb)=paste(paste(fname,i,sep=".."),count,sep=".")
    }


    assign(".ee", c(.ee,eb), .GlobalEnv) 
}


##' This function is a short-name varsion of ".ee.append()". 
##' @title a short-name varsion of ".ee.append()". 
#' @export
.ee.a = .ee.append;

##' This function load object from a stocked environment, without calling ".ee.backup()". 
##' @title load objects from environment without taking backup
##' @param ee environment from which objects are loaded.
##' @param vartop integer(0) or character : if not specified, then only the arguments of the function are loaded. If 0 then all objects are loaded. If "varname", objects upto "varname" are loaded. When "varname" is the first argument of the function, all arguments are loaded.  
##' @author Hiroshi C. Ito
##' @examples
##' testfunc <- function(x){
##' .ee.append("testfunc",environment())
##' return(x+1)}
##' .ee.set()
##' testfunc(3)
##' 
##' .ee.look()
#' @export
.ee.look <- function(ee=.ee[[length(.ee)]],vartop="") {

    fname=attr(ee,"name");  
    if(vartop==""){

        varname=names(formals(args(fname)));
        cat(fname," <- ");
       cat(str(args(fname)));
        }else{           
            if(vartop==0){
                varname=names(ee);
            }else{
                varname=names(ee);
                start=which(varname==vartop);
                varname=varname[start:length(varname)];
                                
                }
                
            }
        
    cat("loading objects:", paste(varname,collapse=", "),"\n");
    for (i in 1:length(varname)){
        tryCatch(assign(varname[i],get(varname[i], envir = ee),envir=.GlobalEnv))
    }
      
}


##' This function loads objects from a stocked environment specified by "ee". If "ee" is not specified, then the last stocked environment is used.
##'
##' By adding ".ee.append("function_name",environment())" at the first line of functions called in your R script, and add ".ee.set()" in your R script, the execution environments of those functions are stocked in a list named ".ee". Each environmend can be loaded afterward in .GlobalEnv by ".ee.l" function, where objects in .Globalenv are copied to an environment named ".ee.b", and ".ee.b" becomes the parent environment of .GlobalEnv. The original environment is recovered by ".ee.recover()".
##' @title load objencts from a stocked environment
##' @param ee environment from which objects are loaded.
##' @param vartop integer(0) or character : if not specified, then only the arguments of the function are loaded. If 0 then all objects are loaded. If "varname", objects upto "varname" are loaded. When "varname" is the first argument of the function, all arguments are loaded.  
##' @author Hiroshi C. Ito
##' @examples
##' 
##' fact <- function(nn,i,flag_ppp=FALSE){     
##'     .ee.append("fact",environment());
##'     cat("i =",i,"  nn =",nn,"\n");
##'      if(flag_ppp){
##'          if(nn==2)print(ppp);
##'          }
##'      y=nn;     
##'     if(y<=1){
##'         x=1;        
##'     }else{
##'         x=y*fact(y-1,i+1,flag_ppp=flag_ppp);
##'     }
##'     return(x);
##' }
##'
##' .ee.set();
##' fact(3,1,flag_ppp=FALSE);
##' .ee.subt_time();
##'
##' ls.str()
##' ls.str(all.names=TRUE)
##'
##' .ee.(0)
##' 
##' .ee.l()
##' ls.str()
##' 
##' .ee.l(vartop=0)
##' ls.str()
##'
##' .ee.l(.ee$fact.2)
##' ls.str()
##' 
##' .ee.recover()
##' ls.str()
##'
##' as.list(.ee$fact.2)
##' 
##' .ee.set()
##' fact(3,1,flag_ppp=TRUE) ##This causes error because ppp does not exist
##' .ee.subt_time()
##'
##' .ee.(0)
##' 
##' .ee.l()
##' ls.str()
##' 
##' .ee.l(vartop=0)
##' ls.str()
##' .ee.recover()
#' @export
.ee.l <- function(ee=.ee[[length(.ee)]],vartop="") {
    eer=get(".ee.recovered",.GlobalEnv);
    if(eer==TRUE){
        .ee.backup();
    }else{
        .ee.clear();
    }
    .ee.look(ee=ee,vartop=vartop);
}

##' This function copies objects in the original .GlobalEnv to the environment named ".ee.b", and remove all objects in .GlobalEnv except ".ee*".
##' @title set temporal .GlobalEnv
##' @author Hiroshi C. Ito
##' @examples
##' .ee.set()
##' .ee.backup()
#' @export
.ee.backup <- function(){
    if(!get(".ee.recovered",.GlobalEnv)){
        cat("already backuped!\n")
    }else{
  
    if(!exists(".ee.b",envir=.GlobalEnv)){
        .ee.b<<-new.env();
    }

    while(sum(search()==".ee.b")>0)detach(.ee.b)
    
    
    oname=ls(all.names=TRUE,envir=.GlobalEnv);
    oname=oname[substr(oname,1,3)!=".ee"];
    if(length(oname)>0){
        mapply(assign, oname, mget(oname, .GlobalEnv), list(.ee.b),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
   
    rm(list=oname,envir=.GlobalEnv);

    attach(.ee.b);

 
        assign(".ee.recovered",FALSE,.GlobalEnv);
        }
 }

##' This function recovers the original .GlobalEnv by copying objects from ".ee.b".  When the current .GlobalEnv is the original one, ".ee.recovered" is "TRUE".
##' @title recover original .GlobalEnv
##' @author Hiroshi C. Ito
##' @examples
##' .ee.set()
##' .ee.backup()
##' .ee.recover()
#' @export
.ee.recover  <- function(){
    if(get(".ee.recovered",.GlobalEnv)){
        cat("already recovered!\n")
    }else{
        
    .ee.clear();
    .ee.copy(.ee.b,.GlobalEnv);
    while(sum(search()==".ee.b")>0)detach(.ee.b);
    rm(list=ls(all.names=TRUE,envir=.ee.b),envir=.ee.b);

    assign(".ee.recovered",TRUE,.GlobalEnv);

    }
}

##' This function is a short-name varsion of ".ee.recover()". 
##' @title a short-name varsion of ".ee.recover()". 
#' @export
.ee.r = .ee.recover;

##' This function removes all objects in .GlobalEnv except ".ee*" 
##' @title removes objects in .GlobalEnv.
##' @author Hiroshi C. Ito
##' @examples
##' .ee.set()
##' .ee.clear()
#' @export
.ee.clear <- function(){
    oname=ls(all.names=TRUE,envir=.GlobalEnv);
    oname=oname[substr(oname,1,3)!=".ee"];
    if(length(oname)>0){
        rm(list=oname,envir=.GlobalEnv);    
    }
}


.ee.copy <- function(env0, env1){
    oname=ls(env0, all.names=TRUE);
    oname=oname[substr(oname,1,3)!=".ee"];
    if(length(oname)>0){
        mapply(assign, oname, mget(oname, env0), list(env1),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
}

##' This function generates a list of environment named ".ee".
##' @title initialize a environment stock list
##' @author Hiroshi C. Ito
##' @examples
##' .ee.set()
#' @export
.ee.set <- function(){
    if(exists(".ee.recovered")){
        if(!.ee.recovered){
            .ee.recover();
            }
    }else{
            assign(".ee.recovered", TRUE, .GlobalEnv);
    }
    
    if(exists(".ee",envir=.GlobalEnv))rm(".ee",envir=.GlobalEnv);
    assign(".ee",list(time=proc.time()[3]), .GlobalEnv) 
}

##' This function calculate elapsed time and put it to ".ee$time" 
##' @title calculate ellapse time
##' @author Hiroshi C. Ito
##' @examples
##' .ee.set()
##' .ee.subt_time()
#' @export
.ee.subt_time <- function(){    
    .ee$time<<-proc.time()[3]-.ee$time;
}




## ###########
## # rebuild
## ###########
## rebuild <- function(x, upTo){
##     ## misc checks
##     if(!is.gGraph(x)) stop("x is not a valid gGraph object")


##     ## misc variables
##     myHistory <- getHistory(x)
##     L <- length(myHistory@cmd)
##     myDates <- getDates(x)


##     ## handle upTo if character
##     if(is.character(upTo)){
##          upTo <- as.POSIXct(upTo)
##          toKeep <- (myDates < upTo)
##     } else if(is.numeric(upTo)){
##         upTo <- round(upTo)
##         if(upTo > L) stop("upTo cannot be greater than the number of items in @history")
##         if(upTo == L) {
##             warning("Pointless operation.")
##             return(x)
##         }

##         toKeep <- 1:upTo
##     }


##     ## handle history
##     myHistory <- myHistory[toKeep]
##     myCmd <- myHistory@cmd
##     objName <- deparse(substitute(x))

##     for(i in 1:upTo){
##         assign(objName, eval(myCmd[[i]]) )
##     }

##     res <- get(objName)
##     return(res)

## } # end rebuild

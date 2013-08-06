## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedDisp/seedDisp.org::*waterDispGRASS][waterDispGRASS:1]]
##' Water disperse seeds from a seed layer using GRASS
##'
##' The principle in this module is as follow:
##' 1) calculate seeds which are deposited in cell and add these to the dispersedSeeds layer
##' 2) disperse in each direction separately to avoid overlap
##' 3) add up all dispersed seeds layers
##' 4) repeat until all seeds are in the deposit layer
##' 
##' @usage waterDispGRASS(input, output="waterDispSeeds", slope="SLOPE", flowdir="FLOWDIR", overwrite=FALSE)
##' @name waterDispGRASS
##' @title Dispersal of seeds by water
##' 
##' @param input \code{character} name of GRASS raster layer specifying number of seeds to be dispersed
##' @param output \code{character} name of GRASS raster layer generated, containing the dispersed seeds
##' @param flowdir \code{character} name of GRASS raster containing flow direction (in GRASS agnps format)
##' @param depRates \code{character} name of GRASS raster layer cotaining the dsposit rates for each cell.
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
waterDispGRASS <- function(
    input,
    output,
    flowdir,
    depRates, 
    overwrite  = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 

    ## does one dispersal step and returns
    ## TRUE if executed
    ## FALSE if sum of stepInput is 0, i.e. no seeds to disperse
    oneStep <- function(stepInput, stepDep, stepToDisp, stepFlowdir, stepDepRates) {
        ## calculation of sum of seeds left to be dispersed
        univ <- execGRASS("r.univar", map=stepInput, intern=TRUE)
        sm <- grep("sum", univ, value=TRUE)
        s <- as.numeric(strsplit( sm, split=": " )[[1]][2])
        if ( s <= 0 ) {
            return(FALSE)
        } else {
            ## Calculate seeds to be deposited in cell and set null values to 0
            execGRASS(
                "r.mapcalc",
                expression = paste0(
                    stepDep,
                    " = ",
                    "round(", stepInput, " * ", stepDepRates, ", 1)"
                    )
                )
            execGRASS(
                "r.null",
                map = stepDep,
                null = 0L
                )
            ##
            
            ## Calculate seeds to be dispersed and set null values to 0
            execGRASS(
                "r.mapcalc",
                expression = paste0(
                    "_tmp.wd.disp = ",
                    "max( ", stepInput, " - ", stepDep, ", 0 )"
                    )
                )
            execGRASS(
                "r.null",
                map = "_tmp.wd.disp",
                null = 0L
                )

            ## combine expressions for r.mapcalc
            mce <- paste0(
                "_tmp.wd.into.", 1:8,
                " = ",
                "if( ", stepFlowdir, "[",
                c(1,  1,  0, -1, -1, -1,  0,  1),
                ", ",
                c(0, -1, -1, -1,  0,  1,  1,  1),
                " ] == ", 1:8,
                ", _tmp.wd.disp[ ",
                c(1,  1,  0, -1, -1, -1,  0,  1),
                ", ",
                c(0, -1, -1, -1,  0,  1,  1,  1),
                " ], null() )"
                ) 

            ## calculate all and set null vaues to 0 
            for (i in 1:length(mce)) {
                execGRASS(
                    "r.mapcalc",
                    expression = mce[i]
                    )
                execGRASS(
                    "r.null",
                    map = paste0("_tmp.wd.into.", i),
                    null = 0L
                    )
            }

            ## and finally sum them up
            execGRASS(
                "r.mapcalc",
                expression = paste0(
                    stepToDisp,
                    " = ",
                    paste0("_tmp.wd.into.", c(1:8), collapse = " + ")
                    ),
                flags = "overwrite"
                )
            
            ## and finally delete all temporary layers
            execGRASS(
                "g.mremove",
                rast = "_tmp.wd.*",
                flags = "f"
                )
            return(TRUE)
        }
    }

    ## copy input in temporary input layer
    execGRASS(
        "g.copy",
        rast = paste0(input, ",_tmp.wdout.input")
        )
    ## create empty deposit layer
    execGRASS(
        "r.mapcalc",
        expression = "_tmp.wdout.dep.final = 0"
        )
    while (oneStep("_tmp.wdout.input", "_tmp.wdout.dep", "_tmp.wdout.disp", flowdir, depRates)) {
        univ <- execGRASS("r.univar", map="_tmp.wdout.input", intern=TRUE)
        sm <- grep("sum", univ, value=TRUE)
        paste("############", as.numeric(strsplit( sm, split=": " )[[1]][2]), "############")
        ## copy still to be dispersed seeds into temporary input layer
        execGRASS(
            "g.copy",
            rast = "_tmp.wdout.disp,_tmp.wdout.input",
            flags = "overwrite"
            )
        ## add the deposited seeds to the final deposit layer
        execGRASS(
            "r.mapcalc",
            expression = "_tmp.wdout.dep.final = _tmp.wdout.dep.final + _tmp.wdout.dep",
            flags = "overwrite"
            )
        ## remove _tmp.wdout.dep
        execGRASS(
            "g.remove",
            rast = "_tmp.wdout.dep",
            flags = "f"
            )
        ## and continue, i.e. execute oneStep() and repeat until oneStep returns FALSE
        ## Then nothing needs to be done anymore
    }  
   
    ## set 0 values to null and write temporary layer to output layer
    execGRASS(
        "r.null",
        map = "_tmp.wdout.dep.final",
        setnull = "0"
        )
    if (overwrite) {
        fl <- "overwrite"
    } else {
        fl <- NULL
    }
    execGRASS(
        "g.copy",
        rast = paste0("_tmp.wdout.dep.final", ",", output),
        flags = fl
        )
    ## and delete temporary layers
    execGRASS(
        "g.mremove",
        rast = "_tmp.wdout.*",
        flags = "f"
        )
    invisible(output)
}
## waterDispGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:

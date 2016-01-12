## [[file:../seedDisp.org::*waterDispGRASS][waterDispGRASS:1]]
##' Water disperse seeds from a seed layer using GRASS
##'
##' This function disperses seeds using water dispersal using the raster \code{flowdir} in GRASS agnps format
##' and a raster containing the deposit rates of the seeds for each cell (values rangingfrom 0 to 1).
##' 
##' The principle in this module is as follow:
##' \enumerate{
##' \item create empty output layer
##' \item copy input layer into seedsToBeDispersed
##' \item \bold{repeat}
##' \item calculate seeds which are deposited in each cell based on depRates and add these to the output layer
##' \item subtract the deposited seeds from the seedsToBeDispersed layer
##' \item disperse remaining seeds in each direction separately for each cell
##' \item add up dispersed seeds and store in seedsToBeDispersed
##' \item \bold{until seedsToBeDispersed is empty}
##' \item \bold{end}
##' }
##' 
##' @usage waterDispGRASS(input, output="waterDispSeeds", flowdir, depRates, zeroToNULL = TRUE, overwrite = FALSE)
##' @name waterDispGRASS
##' @title Dispersal of seeds by water
##' 
##' @param input name of GRASS raster layer specifying number of seeds to be dispersed - \code{character} 
##' @param output name of GRASS raster layer generated, containing the dispersed seeds - \code{character} 
##' @param flowdir \code{character} name of GRASS raster containing flow direction (in GRASS agnps format)
##' @param depRates \code{character} name of GRASS raster layer cotaining the deposit rates for each cell.
##' @param zeroToNULL \code{boolean} if TRUE replace 0 with NA in the returned \code{matrix},
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' 
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
waterDispGRASS <- function(
    input,
    output = "waterDispSeeds",
    flowdir,
    depRates,
    zeroToNULL = TRUE,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.list", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
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
    execGRASS(
        "g.copy",
        rast = paste0("_tmp.wdout.dep.final", ",", output),
        flags = "overwrite"
        )
    ## and delete temporary layers
    execGRASS(
        "g.mremove",
        rast = "_tmp.wdout.*",
        flags = "f"
        )
    ## if zeroToNULL
    if (zeroToNULL) {
        execGRASS(
            "r.null",
            map=output,
            setnull="0"
            )
    } else {
        execGRASS(
            "r.null",
            map=output,
            null=0
            )    
    }
    invisible(output)
}
## waterDispGRASS:1 ends here

## Local Variables:
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:

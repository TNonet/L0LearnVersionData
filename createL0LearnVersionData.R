library("L0Learn")

createL0LearnVersionData <- function(){
    currentVersion = paste(packageVersion("L0Learn"))
    
    cwd <- getwd();
    
    # Expecting cwd to be "../L0LearnVersionData
    
    # Will create and fill directory "../L0LearnVersionData/<currentVersion>
    
    if (basename(cwd) !="L0LearnVersionData"){
        stop("Must be in 'L0LearnVersionData' base directory (.../L0LearnVersionData/")
    }
    
    cdirs <- dir()
    
    if (currentVersion %in% cdirs){
        stop("currentVersion is already saved. To replace delete the folder and rerun the script")
    }
    
    data <- L0Learn::GenSynthetic(n=500, p=2000, k=25, seed=1)
    smalldata <- L0Learn::GenSynthetic(n=100, p=500, k=25, seed=1)
    
    L0_grid = list()
    L0_grid[[1]] <- c(10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001)
    L0_nGamma = 1
    
    L012_grid = list()
    L012_grid[[1]] <- c(10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001)
    L012_grid[[1]] <- .8*c(10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001)
    L012_grid[[1]] <- .6*c(10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001)
    L012_grid[[1]] <- .4*c(10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001)
    L012_nGamma <- length(L012_grid)
    

    tests <- list()
    i = 0
    for (intercept in c(TRUE, FALSE)){
        for (penalty in c("\"L0\"", "\"L0L1\"", "\"L0L2\"")){
            for (algorithm in c("\"CD\"", "\"CDPSI\"")){
                data_str = ""
                if (algorithm == "\"CD\""){
                    data_X = "data$X"
                    data_y = "data$y"
                } else {
                    data_X = "smalldata$X"
                    data_y = "smalldata$y"
                }
                
                if (penalty == "\"L0\""){
                    grid = "L0_grid"
                    nGamma = "L0_nGamma"
                } else {
                    grid = "L012_grid"
                    nGamma = "L012_nGamma"
                }
                
                if (currentVersion == "1.2.0"){
                    grid_string = paste(", autoLambda = FALSE, lambdaGrid=", grid, 
                                        ", nGamma=", nGamma, sep = "")
                } else {
                    grid_string = paste(", lambdaGrid=", grid, 
                                        ", nGamma=", nGamma, sep = "")
                }
                
                
                if (currentVersion != "1.2.0" || intercept){
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(", data_X, ", ", data_y,
                                       ", loss=\"SquaredError\", penalty="
                                       , penalty, ", intercept=",intercept,
                                       ", algorithm=", algorithm,  grid_string, ")", sep='')
                    
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(", data_X, ", sign(", data_y,
                                       "), loss='Logistic', penalty=",
                                       penalty, ", intercept=", intercept,
                                       ", algorithm=", algorithm, grid_string, ")", sep='')
                    
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(", data_X, ", sign(", data_y,
                                       "), loss='SquaredHinge', penalty=",
                                       penalty, ", intercept=", intercept,
                                       ", algorithm=", algorithm, grid_string, ")", sep='')
                }
            }
        }
    }
    
    # creates "../L0LearnVersionData/<currentVersion>/
    dir.create(currentVersion)
    
    # Saves tests to "../L0LearnVersionData/<currentVersion>/tests.txt"
    lapply(tests, write, paste(currentVersion, "/tests.txt", sep=''),
           append=TRUE, ncolumns=1)
    
    saveRDS(data, file.path(currentVersion, "data.rData"))
    saveRDS(smalldata, file.path(currentVersion, "smalldata.rData"))
    saveRDS(L0_grid, file.path(currentVersion, "L0_grid.rData"))
    saveRDS(L012_grid, file.path(currentVersion, "L012_grid.rData"))
    
    # Run all tests
    for (i in 1:length(tests)){
        test = tests[i]
        fit <- eval(parse(text=test))
        saveRDS(list(text=test, fit=fit), file.path(currentVersion, paste(i, ".rData", sep='')))
    }

    
}

createL0LearnVersionData()

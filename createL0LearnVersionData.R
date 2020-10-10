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
    
    data <-  L0Learn::GenSynthetic(n=500, p=2000, k=25, seed=1)
    X <- data[[1]]
    y <- data[[2]]
    
    tol = 1e-4
    
    if (sum(apply(X, 2, sd) == 0)) {
        stop("X needs to have non-zero std for each column")
    }
    
    tests <- list()
    i = 0
    for (intercept in c(TRUE, FALSE)){
        for (penalty in c("\"L0\"", "\"L0L1\"", "\"L0L2\"")){
            for (algorithm in c("\"CD\"", "\"CDPSI\"")){
                
                if (currentVersion != "1.2.0" || intercept){
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(data$X, data$y, loss=\"SquaredError\", penalty="
                                       , penalty, ", intercept=",intercept,
                                       ", algorithm=", algorithm, ")", sep='')
                    
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(data$X, sign(data$y), loss='Logistic', penalty=",
                                       penalty, ", intercept=", intercept,
                                       ", algorithm=", algorithm, ")", sep='')
                    
                    i = i+1
                    tests[[i]] = paste("set.seed(1); L0Learn.fit(data$X, sign(data$y), loss='SquaredHinge', penalty=",
                                       penalty, ", intercept=", intercept,
                                       ", algorithm=", algorithm, ")", sep='')
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
    
    #Run all tests
    for (i in 1:length(tests)){
        test = tests[i]
        fit <- eval(parse(text=test))
        saveRDS(list(text=test, fit=fit), file.path(currentVersion, paste(i, ".rData", sep='')))
    }
    
    
}

createL0LearnVersionData()

###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "openxlsx",
            "microbenchmark",
            "data.table",
            "parallel",
            "snow",
            "lme4",
            "future",
            "Rcpp"
        ),
        function(my_package){
            
            if(!(my_package %in% rownames(installed.packages()))){
                
                install.packages(
                    my_package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
                
            }
            
            library(my_package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"_script_.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){
        
        dir.create(file.path(
            
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

# adresace typu $var vs. adresace typu [, var] --------------------------------

for(my_variable in colnames(mtcars)){
    cat(
        mean(mtcars$my_variable)
    )
    cat("\n")
}   # nebude fungovat

for(my_variable in colnames(mtcars)){
    cat(
        mean(mtcars[, my_variable])
    )
    cat("\n")
}   # funguje


## ----------------------------------------------------------------------------

###############################################################################

## prealokace -----------------------------------------------------------------

bezAlokace <- function(n){    
    my_output <- NULL
    
    for(i in 1:n){my_output <- c(my_output, i)}
    
    return(my_output)        
}

sAlokaci <- function(n){    
    my_output <- rep(0, n)
    
    for(i in 1:n){my_output[i] <- i}
    
    return(my_output)        
}


## porovnání obou přístupů

n <- 5000
    
microbenchmark::microbenchmark(
    times = 100,
    unit = "ns",
    bezAlokace(n),
    sAlokaci(n)
)


## ----------------------------------------------------------------------------

###############################################################################

## vektoriraze ----------------------------------------------------------------

bezVektorizace <- function(n){
    x <- NULL    
    for(i in 1:n){x <- c(x, runif(1) + 1)}
    return(x)
}

sVektorizaci <- function(n){
    y <- runif(n) + 1
    return(y)
}


## porovnání obou přístupů

n <- 5000
    
microbenchmark::microbenchmark(
    times = 100,
    unit = "ns",
    bezVektorizace(n),
    sVektorizaci(n)
)


## ----------------------------------------------------------------------------

###############################################################################

## prodlužování vektorů -------------------------------------------------------

n <- 1000

# prodlužování vektoru
x <- NULL    
for(i in 1:n){x <- c(x, i ^ 3)}

# prealokace
x <- rep(0, n)
for(i in 1:n){x[i] <- i ^ 3}

# vektorizace
x <- c(1:n) ^ 3

my_start <- Sys.time()
x <- NULL          # prodlužování vektoru
for(i in 1:n){x <- c(x, i ^ 3)}
my_stop <- Sys.time(); my_stop - my_start # 0.050s
    
my_start <- Sys.time()
x <- rep(0, n)  # prealokace
for(i in 1:n){x[i] <- i ^ 3}
my_stop <- Sys.time(); my_stop - my_start # 0.037s
    
my_start <- Sys.time()
x <- c(1:n) ^ 3 # vektorizace
my_stop <- Sys.time(); my_stop - my_start # 0.015s


## ----------------------------------------------------------------------------

###############################################################################

## caching proměnných ---------------------------------------------------------

#### chceme každý člen matice vydělit součtem celé matice

# méně vhodně
set.seet(1)
my_matrix <- matrix(runif(10000), nrow = 100)

apply(
    my_matrix,
    2,
    function(i){i / sum(my_matrix)}
)

# lepší řešení
set.seet(1)
my_matrix <- matrix(runif(10000), nrow = 100)
sum_of_my_matrix <- sum(my_matrix)

apply(
    my_matrix,
    2,
    function(i){i / sum_of_my_matrix}
)


## ----------------------------------------------------------------------------

###############################################################################

## funkce rodiny apply() ------------------------------------------------------

apply(mtcars, 2, mean)

my_start <- Sys.time()
x <- apply(mtcars, 2, mean)
my_stop <- Sys.time(); my_stop - my_start # 0.019s

my_start <- Sys.time()
x <- NULL
for(i in 1:dim(mtcars)[2]){
  x <- c(x, mean(mtcars[, i]))
  names(x)[length(x)] <- colnames(mtcars)[i]
}
my_stop <- Sys.time(); my_stop - my_start # 0.039s


## funkce lapply() jako nástroj adresace --------------------------------------

set.seed(1)
my_long_list <- lapply(
    sample(c(80:120), 100, TRUE),
    function(x) sample(
        c(50:150), x, replace = TRUE
    )
)   # list vektorů náhodné délky
    # generovaných z náhodných čísel

lapply(my_long_list, "[[", 14)
    # z každého prvku listu (vektoru)
    # vybírám jen jeho 14. prvek


## for cyklus vs. lapply ------------------------------------------------------

# for cyklus
my_start <- Sys.time()

for_x <- NULL
for(i in 1:100000){for_x <- c(for_x, i ^ 5)}

my_stop <- Sys.time(); my_stop - my_start # 18.45s

# lapply
my_start <- Sys.time()

lapply_x <- unlist(lapply(
    1:100000,
    function(i) i ^ 5   # koncept anonymní funkce
))

my_stop <- Sys.time(); my_stop - my_start # 0.10s


## ----------------------------------------------------------------------------

###############################################################################

## real-time výpis do konzole u iterativních procesů --------------------------

x <- NULL
    
flush.console()
for(i in 1:100000){
   x <- c(x, i ^ 5)
   cat(
       paste(
           "Proces hotov z ",
           i / 100000 * 100,
           " %.\n", sep = ""
       )
   )
}


# lapply bez progress baru
x <- lapply(
    1:1000000,
    function(i) i ^ 5
)

# progress bar u lapply
library(pbapply)

x <- pblapply(
    1:1000000,
    function(i) i ^ 5
)

## dynamické iterování --------------------------------------------------------

#### vytvořit $26$ vektorů $a$, $b$, \dots, $z$
#### tak, že $j$-tý vektor je tvořen právě $j$ jedničkami,
#### kde $j$ odpovídá indexu názvu vektoru v~rámci anglické abecedy,
#### tedy např. pro $a$ je $j = 1$, pro $c$ je $j = 3$ apod.

for(my_letter in letters){
    
    assign(
        my_letter,
        rep(1, which(letters == my_letter))
    )
    
}

#### nyní chceme všechny vektory $a$, $b$, \ldots, $z$
#### vynásobit číslem $2$ a přičíst ke každé jeho složce vždy náhodnou hodnotu
#### bílého šumu $\mathcal{N}(0, 1^2)$

for(my_letter in letters){
    
    my_vector <- get(my_letter)
    
    assign(
        my_letter,
        2 * my_vector + rnorm(1)
    )
    
}
    

## ----------------------------------------------------------------------------

###############################################################################

## funkce do.call() -----------------------------------------------------------

my_list <- list(1:10, trim = 0, na.rm = TRUE)
    
do.call(what = "mean", args = my_list) # 5.5

# odpovídá příkazu
mean(1:10, trim = 0, na.rm = TRUE)

#### předpokládejme, že chceme pro vektory hodnot $\bm{x}$ a $\bm(y)$ zjistit
#### postupně průměr, minimum, maximum, medián, směrodatnou odchylku, rozptyl
#### a vždy poslední cifru čísla

set.seed(1)
x <- floor(runif(100) * 100)
set.seed(2)
y <- floor(runif(100) * 100)

for(my_vector_name in c("x", "y")){
    my_vector <- get(my_vector_name)
    for(my_function in c(
        "mean", "min",
        "max", "median",
        "sd", "var",
        function(i) i %% 10
    )){
        cat(do.call(my_function, list(my_vector)))
        cat("\n")
    }
}


## ----------------------------------------------------------------------------

###############################################################################

## pokročilý caching funkcí a proměnných --------------------------------------

# původní funkce
getFibonacci <- function(n){
    if(n == 1){return(1)}
    if(n == 2){return(1)}
    if(n >= 3){
        return(
            getFibonacci(n - 1) +
            getFibonacci(n - 2)
        )
    }
}

# cachovaná funkce
memoisedFibonacci <- memoise::memoise(getFibonacci)


# benchmarking
n <- 25

microbenchmark::microbenchmark(
    times = 10,
    unit = "ms",
    getFibonacci(n),
    memoisedFibonacci(n)
)


## ----------------------------------------------------------------------------

###############################################################################

## prekompilace ---------------------------------------------------------------

library(compiler)

getFunction("mean")
# function (x, ...) 
# UseMethod("mean")
# <bytecode: 0x033be6b8>
# <environment: namespace:base>

my_mean <- function(x){
    my_output <- 0
    for(i in 1:length(x)){
        my_output <- my_output + x[i] / length(x)
    }
    return(my_output)
}

# kompilace pomocí funkce cmpfun() balíčku compiler
compiled_my_mean <- compiler::cmpfun(my_mean)

x <- rnorm(1000)
microbenchmark::microbenchmark(
    times = 100,
    unit = "ms",
    mean(x), my_mean(x), compiled_my_mean(x)
)
# Unit: milliseconds
#                 expr      min        lq       mean    median       uq      max
#              mean(x) 0.005133 0.0059870 0.00674457 0.0062015 0.006843 0.033357
#           my_mean(x) 0.163788 0.1669955 0.21073893 0.1702020 0.175762 4.044222
#  compiled_my_mean(x) 0.163361 0.1676370 0.17462025 0.1697750 0.175762 0.234349
#  neval cld
#    100  a 
#    100   b
#    100   b


## ----------------------------------------------------------------------------

###############################################################################

## načítání velkých souborů ---------------------------------------------------

my_data <- data.frame(matrix(1:1e7, nrow = 1e6))

library(data.table)
fwrite(x = my_data, file = "moje_data.txt")
fread("moje_data.txt")

library(vroom)
vroom_write(x = my_data, path = "moje_data.txt")
vroom("moje_data.txt")

## ----------------------------------------------------------------------------

###############################################################################

## paralelizace ---------------------------------------------------------------

library(parallel)
library(MASS)


# zkusme
clusterEvalQ(my_cluster, 2 + 2)

x <- 1
clusterEvalQ(my_cluster, x)

rm(y)
clusterEvalQ(my_cluster, y <- 1)
clusterEvalQ(my_cluster, y)
y

clusterEvalQ(
    my_cluster,
    {
        library(ggplot2)
        library(stringr)
    }
)

parSapply(my_cluster, Orange, mean, na.rm = TRUE)

stopCluster(my_cluster)


#### komplexní případ

###### lapply verze na jednom jádru

library(lme4)

my_function <- function(i){
    lmer(
        Petal.Width ~ . - Species + (1 | Species),
        data = iris
    )
}
 
system.time(my_first_save <- lapply(1:100, my_function))


###### spoužitím paralelizované verze lapply() (na jednom jádru)

system.time(my_second_save <- mclapply(
    1:100,
    my_function,
    mc.cores = detectCores() # ne na Windows !
))


###### na více jádrech

my_cluster <- makeCluster(detectCores())
clusterEvalQ(my_cluster, library(lme4))

system.time(my_third_save <- parLapply(my_cluster, 1:100, my_function))

stopCluster(my_cluster)


## ----------------------------------------------------------------------------

###############################################################################

## C++ v R --------------------------------------------------------------------

library(Rcpp)

cppFunction(
    'int signCpp(double x) {
        if (x > 0) {
            return 1;
        } else if (x == 0) {
            return 0;
        } else {
            return -1;
        }
    }'
)

signCpp(-4)
signCpp(0)
signCpp(10)


cppFunction(
    'double mean_cpp(NumericVector x) {
        int i;
        int n = x.size();
        double mean = 0;
        for(i=0; i<n; i++) {
            mean = mean + x[i] / n;
        }
        return mean;
    }'
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






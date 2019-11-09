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
###############################################################################
###############################################################################






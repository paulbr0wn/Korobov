# Korobov Lattices
You can use this code to generate a shifted Korobov lattice pointset in R. You will need to specify the number of points, number of dimensions, a generating constant, and the option to generate a shifted Korobov lattice.

shift.koro = function(n.pts, dimension, gen_constant, shift) {
    z.korob = vector(length = s)
    z.korob[1] = 1
    for (j in 1:(s-1)){
        z.korob[j+1] = (z.korob[j] * a) %% n
    }
    korob = array(dim = c(n,s))
    if(shift == TRUE){
        for (k in 0:(n-1)){
            korob[k+1,] = (((k/n)*z.korob) + v) %% 1
        }
    } else {
        for (k in 0:(n-1)){
            korob[k+1,] = ((k/n)*z.korob) %% 1
        }
    }
    return(korob)
}

## Example 1 - Shifted Korobov lattice with 32 points in 2 dimensions

xx = shift.koro(32,2,19,TRUE)
plot(xx, pch=16)

## Example 2 - Korobov lattice with 256 points in 5 dimensions

xxx = shift.koro(128,5,19,FALSE)
pairs(xxx, pch=16)

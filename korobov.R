shift.koro = function(n.pts, dimension, gen_constant, shift)
{
    ## Arguments:
    ## n.pts - number of points. Keep it to 2^x 
    ## gen_constant - generating constant.
    ## Relatively prime to n.pts. For optimal, see below where function is used..
    ## shift - TRUE or FALSE. Random shift (rather than start at 0)

    n = n.pts; s = dimension; a = gen_constant; v = runif(s,0,1)
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


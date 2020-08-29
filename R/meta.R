meta <-
function (means, ses) 
{
    ws <- 1/ses^2
    beta <- sum(ws * means)/sum(ws)
    se <- sqrt(1/sum(ws))
    return(list(beta = beta, se = se, p = pchisq((beta/se)^2, 
        1, low = FALSE)))
}

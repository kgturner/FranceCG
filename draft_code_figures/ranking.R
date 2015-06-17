#ranking and tests of rank

#france
#ranking, single trait, increasing trait value
sort(rank(tapply(d$LfCount1, d$Pop, mean), ties.method = "first"))
sort(rank(tapply(d$MaxLfLgth1, d$Pop, mean), ties.method = "first"))
sort(rank(tapply(d$MaxLfWdth1, d$Pop, mean), ties.method = "first"))
library(data.table); library(ez);

load("./PilotData/IndividualData.RData")
load("./PilotData/ExcludedSubjectList.RData")

# Remove excluded subjects
outside.hit.rate.item.curiosity.median <- outside.hit.rate.item.curiosity.median[! SubjectNo %in% excluded.ps]
outside.hit.rate.item.interest.median  <- outside.hit.rate.item.interest.median[! SubjectNo %in% excluded.ps]
outside.hit.rate.item.surprise.median  <- outside.hit.rate.item.surprise.median[! SubjectNo %in% excluded.ps]

outside.hit.rate.item.order.curiosity.median <- outside.hit.rate.item.order.curiosity.median[]

# Compare the *curiosity* groups
outside.hit.rate.item.curiosity.median[,. (Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(CurGrpMd)]
#   CurGrpMd      Mean        SD
# 1:     High 0.2195288 0.1728828
# 2:      Low 0.2263433 0.1791499

t.test(SAcc ~ CurGrpMd, outside.hit.rate.item.curiosity.median, paired = T)
#         Paired t-test
# 
# data:  SAcc by CurGrpMd
# t = -0.33085, df = 28, p-value = 0.7432
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.04900503  0.03537613
# sample estimates:
#   mean of the differences 
#               -0.00681445 

## Include the item order
outside.hit.rate.item.order.curiosity.median[, .(Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(CurGrpMd, ObjOrdGrp)]
#    CurGrpMd ObjOrdGrp      Mean        SD
# 1:     High     Early 0.2235929 0.1856678
# 2:     High     Later 0.2154648 0.1989185
# 3:      Low     Early 0.2352376 0.1839472
# 4:      Low     Later 0.2174489 0.1954880

ezANOVA(outside.hit.rate.item.order.curiosity.median, dv = .(SAcc), wid = .(SubjectNo), within = .(CurGrpMd, ObjOrdGrp), type = 3)
# $ANOVA
#               Effect DFn DFd          F         p p<.05          ges
# 2           CurGrpMd   1  28 0.10946193 0.7432208       0.0003291005
# 3          ObjOrdGrp   1  28 0.50689360 0.4823772       0.0011890390
# 4 CurGrpMd:ObjOrdGrp   1  28 0.05404384 0.8178613       0.0001653828

# Compare the *interestingness* groups
outside.hit.rate.item.interest.median[,. (Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(IntGrpMd)]
#    IntGrpMd      Mean        SD
# 1:     High 0.2291553 0.1844852
# 2:      Low 0.2126460 0.1760553

t.test(SAcc ~ IntGrpMd, outside.hit.rate.item.interest.median, paired = T)
#         Paired t-test
# 
# data:  SAcc by IntGrpMd
# t = 0.88494, df = 28, p-value = 0.3837
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.02170544  0.05472405
# sample estimates:
#   mean of the differences 
#                 0.0165093 

## Include the item order
outside.hit.rate.item.order.interest.median[, .(Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(IntGrpMd, ObjOrdGrp)]
#    IntGrpMd ObjOrdGrp      Mean        SD
# 1:     High     Early 0.2205688 0.1988840
# 2:     High     Later 0.2377417 0.2050686
# 3:      Low     Early 0.2161900 0.1762285
# 4:      Low     Later 0.2091019 0.2001880

ezANOVA(outside.hit.rate.item.order.interest.median, dv = .(SAcc), wid = .(SubjectNo), within = .(IntGrpMd, ObjOrdGrp), type = 3)
# $ANOVA
#               Effect DFn DFd          F         p p<.05          ges
# 2           IntGrpMd   1  28 0.78311983 0.3837253       0.0018447767
# 3          ObjOrdGrp   1  28 0.07115291 0.7916202       0.0001723817
# 4 IntGrpMd:ObjOrdGrp   1  28 0.34648238 0.5608300       0.0009968169

# Compare the *surprise* groups
outside.hit.rate.item.surprise.median[,. (Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(SurGrpMd)]
#    SurGrpMd      Mean        SD
# 1:     High 0.2586298 0.2109511
# 2:      Low 0.1906358 0.1659977

t.test(SAcc ~ SurGrpMd, outside.hit.rate.item.surprise.median, paired = T)
#         Paired t-test
# 
# data:  SAcc by SurGrpMd
# t = 2.6004, df = 28, p-value = 0.0147
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01443337 0.12155459
# sample estimates:
#   mean of the differences 
#                0.06799398 

## Include the item order
outside.hit.rate.item.order.surprise.median[, .(Mean = mean(SAcc, na.rm = T), SD = sd(SAcc, na.rm = T)), by = .(SurGrpMd, ObjOrdGrp)]
#    SurGrpMd ObjOrdGrp      Mean        SD
# 1:     High     Early 0.2576104 0.2141326
# 2:     High     Later 0.2596492 0.2275826
# 3:      Low     Early 0.1932973 0.1766066
# 4:      Low     Later 0.1879744 0.1927549

ezANOVA(outside.hit.rate.item.order.surprise.median, dv = .(SAcc), wid = .(SubjectNo), within = .(SurGrpMd, ObjOrdGrp), type = 3)
# $ANOVA
#               Effect DFn DFd           F          p p<.05          ges
# 2           SurGrpMd   1  28 6.762112843 0.01470203     * 2.803792e-02
# 3          ObjOrdGrp   1  28 0.006862425 0.93456835       1.682340e-05
# 4 SurGrpMd:ObjOrdGrp   1  28 0.037472868 0.84790293       8.453301e-05



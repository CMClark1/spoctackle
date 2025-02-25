
#Install package
devtools::install_github('CMClark1/spoctackle')
library(spoctackle)

#Pull some survey data
channel<-ROracle::dbConnect(DBI::dbDriver("Oracle"), username="clarkc", password="dfq2twhr", "PTRAN")

detail<-unique(ROracle::dbGetQuery(channel, "select b.year, b.season, EXTRACT(MONTH FROM c.sdate) as month,c.strat,a.setno,a.SPEC,a.FLEN,a.FSEX
                                  from groundfish.gsdet a,
                                  groundfish.gsmissions b,
                                  groundfish.gsinf c,
                                  groundfish.gscat d
                                  where a.spec in ('10','14')
                                  and a.fsex in ('0','1','2')
                                  and b.season='SUMMER'
                                  and b.year = 2024
                                  and c.type=1
                                  and a.mission=b.mission
                                  and a.mission=c.mission
                                  and a.setno=c.setno
                                  and a.mission=d.mission
                                  and a.setno=d.setno"))

#Apply AB function from spoctackle package

#survey As and Bs
with_weights_survey <- spoctackle::calc_weight_ab(data=detail,ab_source="survey")

library(ggplot2)
ggplot(with_weights_survey, aes(x=flen, y=weight_ab))+geom_point()+facet_wrap(~spec)

#quarterly As and Bs for cod only
with_weights_catch <- spoctackle::calc_weight_ab(data=detail,ab_source="catch")

library(ggplot2)
ggplot(with_weights_catch, aes(x=flen, y=weight_ab))+geom_point()+facet_wrap(~spec)

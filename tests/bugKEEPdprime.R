require(SensMixed)

tools::assertError(print(res2 <- sensmixed(c("Noise", "Elasticeffect"),
                  Prod_effects = c("TVset"), replication="Repeat", 
                  individual="Assessor", data=TVbo, parallel = FALSE)))
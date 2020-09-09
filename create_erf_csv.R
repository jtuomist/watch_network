## This code fetches all standard exposure-response functions (ERFs) and produces a CSV
## file of the median values.

library(OpasnetUtils)

objects.latest("Op_en2031", code_name="ERF2")

openv.setN(0)

exposure <- 1
ERFchoice <- 1
ERF <- EvalOutput(ERF)@output
colnames(ERF)[colnames(ERF)=="ERFResult"] <- "result"
colnames(ERF) <- tolower(colnames(ERF))
ERF$erfsource <- NULL

write.csv(ERF, "~/devel/ghg-notebooks/exposure_response_functions.csv", row.names = FALSE)

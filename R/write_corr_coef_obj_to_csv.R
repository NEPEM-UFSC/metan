# R/write_corr_coef_obj_to_csv.R
# --------------------------------

#' Write corr_coef object to CSV
#'
#' Exports both the correlation matrix and the corresponding p-value matrix
#' into a single comma-separated file, exactly reproducing the manual six-block
#' layout.
#'
#' @param corrCoefObj  A list returned by \code{\link[metan]{corr_coef}}.
#' @param fileNameWithoutExtension  Output file name **without extension**.
#'                     The string \code{".csv"} is automatically appended.
#'
#' @return
#' Called for its side effect (writing a file). Nothing is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(metan)
#' cc <- corr_coef(df, c(v1, v2, v3))
#' write_corr_coef_obj_to_csv(cc, "my_corr")
#' }

write_corr_coef_obj_to_csv<-function(corrCoefObj, fileNameWithoutExtension)
{
	utils::write.csv("Correlations", sep=",", row.names=FALSE, col.names=FALSE, file =  paste0(fileNameWithoutExtension,".csv"))
	write.table(data.frame(t(c("",colnames(corrCoefObj$cor)))),append= TRUE, sep=",", col.names=FALSE, row.names=FALSE, file=paste0(fileNameWithoutExtension,".csv"))   
	write.table(corrCoefObj$cor, append= TRUE, sep=",", col.names=FALSE, file=paste0(fileNameWithoutExtension,".csv")) 
	write(" ",file = paste0(fileNameWithoutExtension,".csv"), append = TRUE)
	write("P-Values For Correlations",file = paste0(fileNameWithoutExtension,".csv"), append = TRUE)
	write.table(data.frame(t(c("",colnames(corrCoefObj$pval)))), append= TRUE, sep="," ,row.names=FALSE ,col.names=FALSE, file=paste0(fileNameWithoutExtension,".csv")) 
	write.table(corrCoefObj$pval, append= TRUE, sep=",", col.names=FALSE, file=paste0(fileNameWithoutExtension,".csv"))
}

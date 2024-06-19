#' MethylSig-UM Binary Classification Function
#'
#' Get binary classification using MethylSig-UM model
#' @param test.data A matrix or data-frame with CpG probe names as named rows and samples as columns.
#' @keywords binary
#' @export
#' @examples
#' methylsig_um_predict_binary()


methylsig_um_predict_binary <- function(test.data){
	
	if(all(rownames(final.sig) %in% rownames(test.data))){
		print("All probes present, can continue")
	}else{
		print("Missing probes from MethylSig-UM signature. Current model does not handle missing data.")
		q()
	}

	### Subset data
	test.data <- test.data[rownames(final.sig) , ] 

	### Apply signature
	test.binary <- predict(rf.model, newdata=t(test.data))
	warning("An optimal score threshold has not been established for MethylSig-UM. Use these binary classifications with caution. Consider using methylsig_um_predict function and evaluating various cutoffs for your dataset.")
	
	return(test.binary)
}
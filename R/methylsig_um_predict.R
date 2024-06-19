#' MethylSig-UM Classification Function
#'
#' Get MethylSig-UM scores (random forest "votes") using MethylSig-UM model
#' @param test.data A matrix or data-frame with CpG probe names as named rows and samples as columns.
#' @keywords score
#' @export
#' @examples
#' methylsig_um_predict()

methylsig_um_predict <- function(test.data){
	
	if(all(rownames(final.sig) %in% rownames(test.data))){
		print("All probes present, can continue")
	}else{
		print("Missing probes from MethylSig-UM signature. Current model does not handle missing data.")
		q()
	}

	### Subset data
	test.data <- test.data[rownames(final.sig) , ] 

	### Apply signature - return votes for each class
	test.binary <- predict(rf.model, newdata=t(test.data), type='vote')

	return(test.binary[,2])
}
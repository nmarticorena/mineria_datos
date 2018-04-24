#'Lift Curve
#'
#'Get the Lift curve(s) from a(some) score(s) dataframe(s)
#'@usage liftCurve(score, quantiles=100, cumulative=T, groupCol=NULL) 
#'@param score List of scores as dataframes or a single dataframe with al scores binded.
#'@param quantiles integer, number of buckets to make the lift curve.
#'@param cumulative boolean, Is cumulative lift?
#'@param groupCol character indicating the name of the column to group-by the differents 
#'score groups if \code{score} is a single dataframe.
#'@author Martin Vicencio
#'@return It returns a three column dataframe \code{quantile}, \code{lift}, and another one to indentify each group of scoring.
#'@export


liftCurve = function(score,quantiles=100,cumulative=T,groupCol=NULL) {
  safeLibrary(plyr)
  if(is.data.frame(score)&is.null(groupCol)){score=list(score=score)}
  ifelse(is.data.frame(score),
         {
            lift = ddply(score, formula(paste0("~", groupCol)) , function(x) {
             if (!is.integer(x$real_value)) {
               x$real_value <- as.integer(as.character(x$real_value))
             }
             lift <- x[order(-x[, "score"]),]
             buckets <-
               ceiling(seq_along(lift[, "real_value"]) / floor(length(lift[,
                                                                           "real_value"]) / quantiles))
             cap <-
               floor(length(lift[, "real_value"]) / quantiles) * quantiles
             lift <-
               aggregate(lift[1:cap, "real_value"], by = list(buckets[1:cap]),
                         mean)
             if (cumulative) {
               lift[, 2] <- cumsum(lift[, 2]) / seq_along(lift[, 2])
             }
             names(lift) = c("quantile", "lift")
             responseRate = mean(x$real_value)
             lift[, "lift"] = lift[, "lift"] / responseRate
             return(lift)
           })},
         {
           lift = ldply(score, function(x) {
             if (!is.integer(x$real_value)) {
               x$real_value <- as.integer(as.character(x$real_value))
             }
             lift <- x[order(-x[, "score"]),]
             buckets <-
               ceiling(seq_along(lift[, "real_value"]) / floor(length(lift[,
                                                                           "real_value"]) / quantiles))
             cap <-
               floor(length(lift[, "real_value"]) / quantiles) * quantiles
             lift <-
               aggregate(lift[1:cap, "real_value"], by = list(buckets[1:cap]),
                         mean)
             if (cumulative) {
               lift[, 2] <- cumsum(lift[, 2]) / seq_along(lift[, 2])
             }
             names(lift) = c("quantile", "lift")
             responseRate = mean(x$real_value)
             lift[, "lift"] = lift[, "lift"] / responseRate
             lift[,"quantile"]=as.numeric(lift[,"quantile"])
             return(lift)
           })
         })
           return(lift)
}
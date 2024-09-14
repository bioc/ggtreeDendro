##' plot wgcna result
##'
##' Display WGCNA modules using ggtree. This function is similar to WGCNA::plotDendroAndColors()
##' @title plot_wgcna
##' @param x `WGCNA::blockwiseModules()`` output
##' @return ggtree object
##' @importFrom yulab.utils get_fun_from_pkg
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 scale_fill_identity
##' @importFrom ggtree gheatmap
##' @export
##' @author Fengbei Li and Guangchuang Yu
plot_wgcna <- function(x) {
  colors <- x$colors
  labels2colors <- yulab.utils::get_fun_from_pkg(fun="labels2colors", pkg="WGCNA")
  if(is.numeric(colors)) {
    colors <- labels2colors(colors)
  }      

  d <- data.frame(Module=as.character(colors)[x$blockGenes[[1]]])
  rownames(d) <- seq_along(d[,1])

  p <- ggtree(x$dendrograms[[1]], layout = "dendrogram", ladderize = FALSE, size=.2)

  gheatmap(p, d, color=NA, width = .1) + scale_fill_identity() + 
    labs(caption = as.character(as.expression(x$dendrograms[[1]]$call)))
   
}



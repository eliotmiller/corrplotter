#' Function to produce correlation panel plots
#'
#' Fairly versatile function to produce paneled correlation plots, where the lower
#' triangle of panels shows the x,y relationship, the diagonal shows a histogram of each
#' column of data, and the upper triangle summarizes the correlation.
#'
#' @param dataframe The object you want to pull data from to plot.
#' @param ids Optional vector of column IDs or names you want to plot from dataframe.
#' @param hist.breaks The number of breaks desired in the diagonal histograms. Default is
#' 10.
#' @param hist.col The color of the histograms. Default is black.
#' @param hist.names The titles to be printed above histograms, if desired. Will be left
#' blank if not provided.
#' @param hist.names.cex The size of the hist.names titles. Defaults to 1.
#' @param hist.names.spacing The space of the title below the top of the histogram plot.
#' Default is -2. Increasing this number will move the title closer to the top of the
#' panel.
#' @param hist.y The y-axis label in the diagonal histograms. Defaults to blank, but can
#' be set to, e.g., "Frequency".
#' @param summary.cex The size of the summary stat text in the upper half of the plots.
#' Default is 2.
#' @param neg.color The background color of the panel in the upper half of the plots for
#' correlations with negative coefficients. It is very important that this be formatted as
#' a vector of length four, minimum 0, maximum 1, corresponding to rgb + alpha channel.
#' All other formats will fail. Default is red, with alpha 0.5.
#' @param mid.color The background color of the panel in the upper half of the plots for
#' non-existent correlations. Format the same as neg.color. Default is white.
#' @param pos.color The background color of the panel in the upper half of the plots for
#' correlations with positive coefficients. Format the same as neg.color. Default is blue,
#' with alpha 0.5.
#' @param par.oma Used to format the space around the entire plot (the area outside all
#' panels). The format is c(bottom, left, top, right). Default is c(4, 4, 2, 2).
#' @param par.tcl Used to control the length of the tick marks inside the panels. Default
#' is -0.25.
#' @param par.mgp Used to control the distance of the labels from the tick marks inside
#' the panels. Default is c(2, 0.2, 0).
#' @param par.lab Used to control the number of tick marks inside the panels. Format is
#' c(x, y, len). Although len is not implemented in R, it is necessary to pass along.
#' Default is c(5, 5, 7).
#' @param left.text The labels to be printed along each panel on the left margin of the
#' figure. Must be the same length as the number of columns to be printed. If you do not
#' want to print text along the last panel (the histogram), simply make the last element
#' of the vector be equal to "".
#' @param bottom.text Like left.text, but for the bottom axis labels.
#' @param left.text.cex The size of the left.text labels. Defaults to 1.
#' @param bottom.text.cex The size of the bottom.text labels. Defaults to 1.
#' @param expansion Controls how much the histogram is shrunk down to facilitate plotting
#' of histogram names. Also controls how zoomed out the scatterplots are, which might be
#' useful for ensuring dots do not interfere with axis labels.
#' @param ... Other arguments that can be passed to plot, including cex and color for the
#' dots in the scatterplot.
#'
#' @details There are a large variety of options that can be customized on this function,
#' but fundamentally it places scatterplots in the lower triangle of the plot, histograms
#' along the diagram, and a summary of the correlation in the upper triangle.
#'
#' @return A paneled correlation plot.
#'
#' @export
#'
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom graphics hist mtext par plot rect text title axis
#' @importFrom stats cor.test
#'
#' @examples
#' exData <- data.frame(
#'	col1=jitter(sort(rnorm(n=200, mean=0, sd=1), decreasing=TRUE), amount=1),
#'	col2=jitter(sort(rnorm(n=200, mean=10, sd=1)), amount=4),
#'	col3=jitter(sort(rnorm(n=200, mean=-3, sd=1)), amount=3))
#'
#' corrplotter(dataframe=exData, hist.breaks=20, hist.col="purple",
#'	hist.names=c("one","two","three"), left.text=c("units1", "units2", ""),
#'	bottom.text=c("other_units1", "other_units2", "other_units3"),
#'	summary.cex=2, neg.color=c(1,0,0,1), mid.color=c(1,1,0,1), pos.color=c(0,1,0,1),
#'	expansion=0.5, cex=0.5, col="gray")

corrplotter <- function(dataframe, ids, hist.breaks, hist.col, hist.names,
	hist.names.cex, hist.names.spacing, hist.y, summary.cex, neg.color, mid.color,
	pos.color, par.oma, par.tcl, par.mgp, par.lab, left.text, bottom.text, left.text.cex,
	bottom.text.cex, expansion, ...)
{
	if(missing(ids))
	{
		dataframe <- dataframe
	}
	else
	{
		dataframe <- dataframe[,ids]
	}

	#fill in all the missing graphical parameters with the defaults
	if(missing(par.oma))
	{
		par.oma <- c(4, 4, 2, 2)
	}
	else
	{
		par.oma <- par.oma
	}
	
	if(missing(par.tcl))
	{
		par.tcl <- -0.25
	}
	else
	{
		par.tcl <- par.tcl
	}
	
	if(missing(par.mgp))
	{
		par.mgp <- c(2, 0.2, 0)
	}
	else
	{
		par.mgp <- par.mgp
	}
	
	if(missing(par.lab))
	{
		par.lab <- c(5, 5, 7)
	}
	else
	{
		par.lab <- par.lab
	}

	if(missing(hist.breaks))
	{
		hist.breaks <- 10
	}
	else
	{
		hist.breaks <- hist.breaks
	}
	
	if(missing(hist.col))
	{
		hist.col <- "black"
	}
	else
	{
		hist.col <- hist.col
	}
	
	if(missing(hist.names))
	{
		hist.names <- rep("", dim(dataframe)[2])
	}
	else
	{
		hist.names <- hist.names
	}

	if(missing(hist.names.cex))
	{
		hist.names.cex <- 1
	}
	else
	{
		hist.names.cex <- hist.names.cex
	}

	if(missing(hist.names.spacing))
	{
		hist.names.spacing <- -2
	}
	else
	{
		hist.names.spacing <- hist.names.spacing
	}

	if(missing(hist.y))
	{
		hist.y <- ""
	}
	else
	{
		hist.y <- hist.y
	}

	if(missing(left.text))
	{
		left.text <- rep("", dim(dataframe)[2])
	}
	else
	{
		left.text <- left.text
	}

	if(missing(bottom.text))
	{
		bottom.text <- rep("", dim(dataframe)[2])
	}
	else
	{
		bottom.text <- bottom.text
	}

	if(missing(left.text.cex))
	{
		left.text.cex <- 1
	}
	else
	{
		left.text.cex <- left.text.cex
	}

	if(missing(bottom.text.cex))
	{
		bottom.text.cex <- 1
	}
	else
	{
		bottom.text.cex <- bottom.text.cex
	}

	if(missing(neg.color))
	{
		neg.color <- c(1,0,0,0.5)
	}
	else
	{
		neg.color <- neg.color
	}
	
	if(missing(mid.color))
	{
		mid.color <- c(1,1,1,1)
	}
	else
	{
		mid.color <- mid.color
	}

	if(missing(pos.color))
	{
		pos.color <- c(0,0,1,0.5)
	}
	else
	{
		pos.color <- pos.color
	}
	
	if(missing(summary.cex))
	{
		summary.cex <- 2
	}
	else
	{
		summary.cex <- summary.cex
	}
	
	if(missing(expansion))
	{
		expansion <- 0
	}
	else
	{
		expansion <- expansion
	}
	
	#check here to ensure you have equal number of names, columns and things you
	#want to plot
	if(	length(hist.names) != length(left.text) &
		length(hist.names) != length(bottom.text) &
		length(hist.names) != dim(dataframe)[2])
	{
		stop("Your text names and desired panel dimensions are not compatible")
	}
	
	#set up the gridded plot to have as many panels as the square of the number of columns
	par(mfrow = c(dim(dataframe)[2], dim(dataframe)[2]), mar=c(0,0,0,0),
		oma=par.oma, tcl=par.tcl, mgp=par.mgp, lab=par.lab)

	#outrageously complicated step to deal with pasting a vector into rgb
	neg.color <- as.matrix(neg.color)
	mid.color <- as.matrix(mid.color)
	pos.color <- as.matrix(pos.color)

	#create a vector 201 long from -1 to 1 for the possible correlation coefficients
	corColors <- seq(from=-1, to=1, length=201)
	
	#assign names to the corColors. these names are actually a color ramp from neg.color
	#(negative corrs) to mid.color to pos.color
	names(corColors) <- colorRampPalette(c(
		rgb(neg.color[1,], neg.color[2,], neg.color[3,], neg.color[4,]),
		rgb(mid.color[1,], mid.color[2,], mid.color[3,], mid.color[4,]),
		rgb(pos.color[1,], pos.color[2,], pos.color[3,], pos.color[4,])),
		alpha = TRUE)(201)
	
	#loop through each column
	for(i in 1:dim(dataframe)[2])
	{
		#plot against each other column
		for(j in 1:dim(dataframe)[2])
		{
			#plot the upper triangle
			if(j > i)
			{
				#create a temporary correlation of column i as a function of j
				temp <- cor.test(x=dataframe[,j], y=dataframe[,i])
				
				#find the interval the corr coefficient p-value belongs to
				rectColor <- names(corColors[findInterval(temp$estimate, corColors)])
				
				#plot a fake plot that does have borders but no axes or labels
				plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
				
				rect(xleft=0, xright=2, ybottom=0, ytop=2, col=rectColor)
				
				#add the correlation coefficient, rounded to 2 digits, and the
				#significance on the next line. note the "\n" to start a newline
				text(x=1, y=1, paste(expression("  r ="), round(temp$estimate, 2), "\n",
					paste(expression("p ="), round(temp$p.value, 2))), cex=summary.cex)
			}        
			#plot the diagonals
			if(j == i)
			{
				#create a temporary histogram so you can determine what the maximum is
				#and expand y-axis accordingly
				tempHist <- hist(dataframe[,j], breaks=hist.breaks, plot=FALSE)
				hist(dataframe[,j], main="", breaks=hist.breaks, col=hist.col,
					ylim=c(0, max(tempHist$counts) * (1 + expansion)))
				axis(side=2, labels=hist.y, tick=FALSE,
					at=max(tempHist$counts) * (1 + expansion)/2, line=-1.5)
				title(hist.names[i], line=hist.names.spacing, cex.main=hist.names.cex)
			}
			#plot the lower triangle
			if(j < i)
			{
				#first calculate the range between the min and max for x and for y
				xRange <- diff(range(dataframe[,j]))
				yRange <- diff(range(dataframe[,i]))
				plot(dataframe[,i]~dataframe[,j],
					xlim=c(	min(dataframe[,j]) - (xRange * expansion),
							max(dataframe[,j]) + (xRange * expansion)),
					ylim=c(	min(dataframe[,i]) - (yRange * expansion),
							max(dataframe[,i]) + (yRange * expansion)), ...)
			}
		}
	}
	#add unit labels to left and bottom axes. the outer margin of the figure isn't changed
	#by messing with par mar above, and runs from zero to 1. determine where labels should
	#go by making a sequence from 0 to 1 of length equal to twice the number of panels
	#plus 1, then only selecting the even numbered (excluding zero)
	places <- seq(from=0, to=1, length=2*dim(dataframe)[2]+1)
	places <- places[1:dim(dataframe)[2]*2]
	
	#add the left side labels
	mtext(text=left.text, at=places, line=2, side=2, outer=TRUE, cex=left.text.cex)
	
	#add the bottom side labels if provided
	mtext(text=bottom.text, at=places, line=2, side=1, outer=TRUE, cex=bottom.text.cex)
}

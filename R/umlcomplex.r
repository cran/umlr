umlclass = function (name, attributes=NULL, methods=NULL, abstract=FALSE, ...)
{	text = list (name)
	if (!is.null (methods) )
	{	text [[2]] = ""
		text [[3]] = methods
	}
	if (!is.null (attributes) ) text [[2]] = attributes
	extend (umlcomplex (text=text, italics=abstract, ...), "umlclass")
}

#expand doesn't work...
#move roundness to umbox????
umlcomplex = function (x=0, y=0, text=list (""), markup=FALSE, italics=FALSE,
	expand=FALSE, roundness=0.18, ...)
{	v = extend (umlbox (x, y, text=text, ...), "umlcomplex")
	v$nspaces = length (text)
	v$nstrs = v$ydivs = 0
	#do we want both text and strs attributes????
	v$strs = character ()
	v$markup = markup
	v$italics = italics
	v$roundness = roundness
	h = getOption ("umlr.hstr")
	for (i in 1:v$nspaces)
	{	#validate w measurement now, after or never????
		v$strs = c (v$strs, text [[i]])
		v$nstrs = length (v$strs)
		if (i < v$nspaces) v$ydivs [i] = v$nstrs * h
	}
	v$h = v$nstrs * h
	v$ydivs = v$ydivs - v$h / 2
	v$ystrs = ( (1:v$nstrs) - 0.5) * h - v$h / 2
	if (expand) v$w = NA
	v
}

print.umlcomplex = function (v, ...)
	cat (class (v) [1], ": ", v$strs [1], "\n", sep="")

plot.umlcomplex = function (v, ...)
{	if (is.na (v$w) ) .umlcomplex.validate (v)
	.rbox (v$x, v$y, v$w, v$h, s=v$roundness, col=v$col, fill=v$fill)
	str = v$strs [1]
	if (v$markup)
	{	str = paste ("expression(", str, ")", sep="")
		str = eval (parse (text=str) )
	}
	text (v$x, v$y + v$ystrs [1], str, font=ifelse (v$italics, 3, 1),
		cex=0.8, adj= c (0.5, 0.35) )
	if (v$nspaces > 1)
	{	segments (v$x - v$w / 2, v$y + v$ydivs, v$x + v$w / 2, v$y + v$ydivs)
		text (v$x - v$w / 2 + 0.15, v$y + v$ystrs [-1], v$strs [-1],
			cex=0.8, adj= c (0, 0.35) )
	}
}

#doesn't work...
.umlcomplex.validate = function (v)
{	v$w = getOption ("umlr.wmin")
	wstr = max (strwidth (v$strs) )
	if (v$w < wstr) v$w = wstr
}










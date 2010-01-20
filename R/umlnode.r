umlgraph = function (vs, cs) extend (compenv (vs, cs, n=length (vs) ), "umlgraph")
plot.umlgraph = function (g, ...)
{	#umlcanvas ()
	for (i in 1:g$n) plot (g$vs [[i]])
	for (i in 1:length (g$cs) ) plot (g$cs [[i]])
}

umlnode = function (x, y) extend (compenv (x, y), "umlnode")
plot.umlnode = function (v, ...) points (v$x, v$y)

umlbounds = function (v, ...) UseMethod ("umlbounds")
umlbounds.umlnode = function (v, ...) .umlnode.bounds (v)

umlcanvas = function (xlim=c (-10, 10), ylim=c (10, -10), gridlines=TRUE)
{	par (mai=c (0.25, 0.25, 0.25, 0.25) )
	plot.new ()
	plot.window (xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
	if (gridlines)
	{	abline (h=ylim [1]:ylim [2], v=xlim [1]:xlim [2], col="grey90")
		abline (h=c (-5, 0, 5), v=c (-5, 0, 5), lty=3, col="grey50")
	}
	box ()
}

.umlgraph.bounds = function (g)
{	k = NULL
	for (i in 1:g$n) k = rbind (k, umlbounds (g$vs [[i]]) )
	c (min (k [,1]), min (k [,2]), max (k [,3]), max (k [,4]) )
}
.umlgraph.plotbounds = function (g)
{	k = .umlgraph.bounds (g)
	rect (k [1], k [2], k [3], k [4], lty=3, border="orange")
}

umlgraph.preview = function (g)
{	k = .umlgraph.bounds (g)
	xlim = c (k [1] - 0.25, k [3] + 0.25)
	ylim = c (k [4] + 0.25, k [2] - 0.25)
	sc = 6.5 / 20
	dev.new (width=sc * diff (xlim), height=-sc * diff (ylim) )
	par (mar=c (0, 0, 0, 0) )
	plot.new ()
	plot.window (xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
	plot (g)
}

umlgraph.pdf = function (g)
{	k = .umlgraph.bounds (g)
	xlim = c (k [1] - 0.25, k [3] + 0.25)
	ylim = c (k [4] + 0.25, k [2] - 0.25)
	sc = 6.5 / 20
	pdf ("uml.pdf", width=sc * diff (xlim), height=-sc * diff (ylim) )
	par (mar=c (0, 0, 0, 0) )
	plot.new ()
	plot.window (xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
	plot (g)
	dev.off ()
}


.umlnode.bounds = function (v) c (v$x, v$y, v$x, v$y)





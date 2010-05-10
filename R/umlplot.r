plot.uml = function (m, fresh=TRUE, crop=FALSE, grid=FALSE, ...)
{	d = umldims (m)
	kst = .imperial.size (m$k)
	if (crop)
	{	if (fresh)
		{	x = 0.3937
			dev.new (width=x * d [5] + kst$m, height=x * d [6] + kst$m)
			par (mai=rep (0, 4) )
		}
		plot.new ()
		plot.window (xlim=c (d [1] - kst$m, d [3] + kst$m),
			ylim=c (d [4] + kst$m, d [2] - kst$m), xaxs="i", yaxs="i")
	}
	else
	{	if (fresh)
		{	dev.new (width=kst$w, height=kst$h)
			par (mai=rep (kst$m, 4) )
		}
		plot (m$k, grid=grid)
	}
	plot (m$g)
}

#some code duplication...
umlpdf = function (m, image.path=paste (m$image.path, "pdf", sep=".") )
{	d = umldims (m)
	kst = .imperial.size (m$k)
	x = 0.3937
	pdf (image.path, width=x * d [5] + kst$m, height=x * d [6] + kst$m)
	par (mai=rep (0, 4) )
	plot.new ()
	plot.window (xlim=c (d [1] - kst$m, d [3] + kst$m),
		ylim=c (d [4] + kst$m, d [2] - kst$m), xaxs="i", yaxs="i")
	plot (m$g)
	dev.off ()
}

#scaling untested, cm available?
umlpng = function (m, image.path=paste (m$image.path, "png", sep=".") )
{	d = umldims (m)
	kst = .imperial.size (m$k)
	x = 0.3937
	png (image.path, width=x * d [5] + kst$m, height=x * d [6] + kst$m,
		units="in", res=200)
	par (mai=rep (0, 4) )
	plot.new ()
	plot.window (xlim=c (d [1] - kst$m, d [3] + kst$m),
		ylim=c (d [4] + kst$m, d [2] - kst$m), xaxs="i", yaxs="i")
	plot (m$g)
	dev.off ()
}

plot.umlcanvas = function (k, grid=FALSE, ...)
{	plot.new ()
	plot.window (xlim=k$xlim, ylim=rev (k$ylim), xaxs="i", yaxs="i")
	if (grid)
	{	abline (h=.umlseq (k$ylim), v=.umlseq (k$xlim), col="grey92")
		abline (h=.umlseq (k$ylim, 5), v=.umlseq (k$xlim, 5), lty=3, col="grey40")
	}
}

.imperial.size = function (k)
{	x = 0.3937
	list (w=x * k$total.size [1], h=x * k$total.size [2], m=x * k$margin.size)
}

.umlseq = function (rng, freq=1)
{	x = ceiling (rng [1]):floor (rng [2])
	x [x %% freq == 0]
}



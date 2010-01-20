umlclass = function (name, attributes=NULL, methods=NULL, ...)
{	str = list (name)
	if (is.null (attributes) )
	{	if (!is.null (methods) )
		str [[2]] = ""
	}
	else str [[2]] = attributes
	if (!is.null (methods) ) str [[3]] = methods
	umlbox (str, ...)
}

umlbox = function (str=list (""), col=NA, border="black", x=0, y=0, wmin=5, vincr=0.25)
{	nsb = length (str)
	nstr = numeric (nsb)
	superstr = character ()
	for (i in 1:nsb)
	{	nstr [i] = length (str [[i]])
		superstr = c (superstr, str [[i]])
	}
	nt = sum (nstr)
	strh = -1 * max (strheight (superstr, cex=1.45) )
	th = nt * strh + 2 * vincr
	wst = wmin / 2
	hst = th / 2
	obj = extend (umlnode (x, y), "umlbox")
	obj$nsb = nsb
	obj$nstr = nstr
	obj$superstr = superstr
	obj$nt = nt
	obj$wst = wst
	obj$hst = hst
	obj$strh = strh
	obj$vincr = vincr
	obj$col=col
	obj$border=border
	setbox (obj, x, y)
}

plot.umlbox = function (k, ...)
{	#rect (k$px [1], k$py [1], k$px [2], k$py [2])
	.rbox (k$x, k$y, "", 2 * k$wst, 2 * k$hst, col=k$col, border=k$border)
	if (k$nsb > 1) for (i in 1:(k$nsb - 1) )
	{	z = k$py [1] + k$vincr + sum (k$nstr [1:i]) * k$strh
		lines (k$px, c (z, z), col=k$border)
	}
	text (k$x, k$ystr [1] + 0.1, k$superstr [1], adj=c (0.5, 1) )
	if (k$nsb > 1) text (k$px [1] + 0.1, k$ystr [-1] + 0.1, k$superstr [-1], cex=0.95, adj=c (0, 1) )
}

umlbounds.umlbox = function (v, ...) c (v$px [1], v$py [1], v$px [2], v$py [2])

setbox = function (k, x, y)
{	k$x = x
	k$y = y
	k$px = c (x - k$wst, x + k$wst)
	k$py = c (y - k$hst, y + k$hst)
	k$ystr = k$py [1] + k$vincr + 0:(k$nt - 1) * k$strh
	invisible (k)
}

.rbox = function (x, y, str="", w=3.2, h=1.4, s=0.25, col, border)
{	x1 = x - w / 2
	y1 = y - h / 2
	x2 = x + w / 2
	y2 = y + h / 2

	xs1 = x1 + s
	xs2 = x2 - s
	ys1 = y1 + s
	ys2 = y2 - s

	a = 1:7
	a = c (a, a + 8, a + 16, a + 24)
	a = a * pi / 16
	xc = 1 * s * cos (a)
	yc = s * sin (a)

	xv = c (xs2 + xc [1:7], xs2, xs1, xs1 + xc [8:14], x1, x1,
		xs1 + xc [15:21], xs1, xs2, xs2 + xc [22:28], x2, x2)
	yv = c (ys2 + yc [1:7], y2, y2, ys2 + yc [8:14], ys2, ys1,
		ys1 + yc [15:21], y1, y1, ys1 + yc [22:28], ys1, ys2)
	polygon (xv, yv, lwd=1.2, col=col, border=border)
	if (str == "term" || str == "termenv") text (x, y+0.01, str, font=3)
	else text (x, y+0.01, str)
}





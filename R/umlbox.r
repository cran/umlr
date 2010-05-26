umlbox = function (x=0, y=0, w=getOption ("umlr.wmin"), h=1.2,
	text="", col="black", fill=getOption ("umlr.fill") )
{	v = extend (umlnode (x, y), "umlbox")
	v$w = w
	v$h = h
	v$text = text
	v$col=col
	v$fill=fill
	v
}

umldims.umlbox = function (v, ...)
{	#this line should be in a umlcomplex method
	if (is.na (v$w) ) .umlcomplex.validate (v)
	c (v$x - v$w / 2, v$y - v$h / 2, v$x + v$w / 2, v$y + v$h / 2)
}

plot.umlbox = function (v, ...)
{	.rbox (v$x, v$y, v$w, v$h, col=v$col, fill=v$fill)
	text (v$x, v$y, v$text, col=v$col)
}

#todo: rewrite + use res arg
.rbox = function (x, y, w, h, res, s=0, col="black", fill=NA)
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

	polygon (xv, yv, lwd=1.2, col=fill, border=col)
}






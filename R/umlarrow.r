umlarrow = function (v1, v2, solid=FALSE, col="black", fill=getOption ("umlr.fill") )
{	con = extend (umlconnection (v1, v2), "umlarrow")
	con$solid = solid
	con$col = col
	con$fill = fill
	con
}

umlextends = function (v1, v2, ...) extend (umlarrow (v1, v2, ...), "umlextends")

#rewrite...
plot.umlarrow = function (con, ...)
{	source = target = 0
	d1 = umldims (con$v1)
	d2 = umldims (con$v2)
	dir = atan (-1 * (con$v2$y - con$v1$y) / (con$v2$x - con$v1$x) )
	if (con$v1$x > con$v2$x)
	{	if (con$v1$y < con$v2$y) dir = dir - pi
		else dir = dir + pi
	}
	#east
	if (dir > -0.25 * pi && dir <= 0.25 * pi)
	{	source = c (d1 [3], con$v1$y)
		target = c (d2 [1], con$v2$y)
	}
	#north
	else if (dir > 0.25 * pi && dir <= 0.75 * pi)
	{	source = c (con$v1$x, d1 [2])
		target = c (con$v2$x, d2 [4])
	}
	#west
	else if (dir > 0.75 * pi || dir <= -0.75 * pi)
	{	source = c (d1 [1], con$v1$y)
		target = c (d2 [3], con$v2$y)
	}
	#south
	else
	{	source = c (con$v1$x, d1 [4])
		target = c (con$v2$x, d2 [2])
	}
	fill = if (con$solid) con$col else con$fill
	lines (c (source [1], target [1]), c (source [2], target [2]), col=con$col)
	.arrowhead (source [1], source [2], target [1], target [2], con$col, fill)
}

#rewrite...
.arrowhead = function (x1, y1, x2, y2, col=NULL, fill=NA, s=0.45)
{	dx = x2 - x1
	dy = y2 - y1
	slope = dy / dx
	a = atan (slope)
	xa = ya = xb = yb = xc = yc = 0
	if (dx >= 0)
	{	xc = x2 + s * cos (a + pi)
		yc = y2 + s * sin (a + pi)
		xa = x2 + s * cos (a + pi - pi / 8)
		ya = y2 + s * sin (a + pi - pi / 8)
		xb = x2 + s * cos (a + pi + pi / 8)
		yb = y2 + s * sin (a + pi + pi / 8)
}
	else
	{	xc = x2 - s * cos (a + pi)
		yc = y2 - s * sin (a + pi)
		xa = x2 - s * cos (a + pi - pi / 8)
		ya = y2 - s * sin (a + pi - pi / 8)
		xb = x2 - s * cos (a + pi + pi / 8)
		yb = y2 - s * sin (a + pi + pi / 8)
	}
	polygon (c (xa, xb, x2), c (ya, yb, y2), border=col, col=fill)
}





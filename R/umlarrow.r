umlarrow = function (v1, v2)
{	extend (compenv (v1, v2), "umlarrow")	
}

plot.umlarrow = function (con, ...)
{	cc = list (x=con$v1$x, y=con$v1$y)
	cp = list (x=con$v2$x, y=con$v2$y)
	.connector (cc, cp)
}

.connector = function (cc, cp)
{	lines (c (cc$x, cp$x), c (cc$y, cp$y) )
	.arrowhead (cc$x, cc$y, cp$x, cp$y)
}

.arrowhead = function (x1, y1, x2, y2, s=0.4)
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
	polygon (c (xa, xb, x2), c (ya, yb, y2), col="grey95")
}




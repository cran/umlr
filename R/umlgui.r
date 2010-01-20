umlgui = function (g)
{	rpt = first = TRUE
	while (rpt)
	{	if (!first) umlcanvas ()
		plot (g)
		.umlgraph.plotbounds (g)
		p = locator (1)
		if (p$x < -10 || p$x > 10 || p$y < -10 || p$x > 10) rpt = FALSE
		else
		{	x = round (4 * p$x) / 4
			y = round (4 * p$y) / 4
			k = dist = NULL
			for (i in 1:g$n)
			{	v = g$vs [[i]]
				distst = sqrt ( (x - v$x)^ 2 + (y - v$y)^2)
				if (inherits (v, "umlbox") )
				{	if (is.null (k) || distst < dist)
					{	k = v
						dist = distst
					}
				}
			}
			if (!is.null (k) ) setbox (k, x, y)
		}
		first = FALSE
	}

}

umlgui = function (m, grid=TRUE)
{	plot (m, grid=grid)
	p = try (locator (1), TRUE)
	while (is.list (p) && p$x > m$k$xlim [1] && p$x < m$k$xlim [2] &&
		p$y > m$k$ylim [1] && p$y < m$k$ylim [2])
	{	x = round (4 * p$x) / 4
		y = round (4 * p$y) / 4
		u = dist = NULL
		for (i in 1:m$g$nv)
		{	v = m$g$vs [[i]]
			distst = sqrt ( (x - v$x)^ 2 + (y - v$y)^2)
			if (is.null (u) || distst < dist)
			{	u = v
				dist = distst
			}
		}
		if (!is.null (u) ) recenter (u, x, y)
		plot (m, FALSE, grid=grid)
		p = try (locator (1), TRUE)
	}
	invisible (m)
}



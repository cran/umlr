#need more attributes, col etc
umlmerge = function (...)
{	m = extend (list (...), "umlca")
	for (con in m) con$m = 0
	m$n = length (m)
	m$col = m [[1]]$col
	m$fill = m [[1]]$fill
	m [[1]]$m = m
	invisible (m)
}

plot.umlca = function (ca, ...)
{	#note, no validation
	#plus for now, assume all composite arrows are north facing
	sources = list ()
	target = NULL
	for (i in 1:ca$n)
	{	sources [[i]] = ca [[i]]$v1
		if (is.null (target) ) target = ca [[i]]$v2
	}
	.umlca.north (sources, target, fill=ca$fill)
}

#note, assuming h attribute exists
.umlca.north = function (sources, target, ...)
{	sxs = sys = numeric ()
	for (v in sources)
	{	sxs = c (sxs, v$x)
		sys = c (sys, v$y - v$h / 2)
	}
	z1 = target$y + target$h / 2
	z2 = (z1 + min (sys) ) / 2
	segments (min (sxs), z2, max (sxs), z2)
	segments (target$x, z2, target$x, z1)
	segments (sxs, sys, sxs, z2)
	.arrowhead (target$x, z2, target$x, z1, ...)
}






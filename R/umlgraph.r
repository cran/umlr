#change lists to environments
umlgraph = function (vs=list (), cs=list () )
	extend (ENVIRONMENT (vs, cs, nv=length (vs), nc=length (cs) ), "umlgraph")

umlnode = function (x=0, y=0) extend (ENVIRONMENT (x, y, cons=list () ), "umlnode")
umlconnection = function (v1, v2)
{	con = extend (ENVIRONMENT (v1, v2, m=NULL), "umlconnection")
	.umladj (v1, con)
	.umladj (v2, con)
	con
}
.umladj = function (v, con) {n = length (v$cons); v$cons [[n + 1]] = con}

plot.umlgraph = function (g, ...)
{	for (con in g$cs)
	{	if (is.null (con$m) ) plot (con)
		else if (inherits (con$m, "umlca") ) plot (con$m)
	}
	for (v in g$vs) plot (v)
}

plot.umlnode = function (v, ...) points (v$x, v$y, pch=21, col="black", bg="white", cex=1.5)
plot.umlconnection = function (con, ...)
	lines (c (con$v1$x, con$v2$x), c (con$v1$y, con$v2$y) )

umldims.umlgraph = function (g, ...)
{	k = NULL
	for (v in g$vs) k = rbind (k, umldims (v) )
	d = c (min (k [,1]), min (k [,2]), max (k [,3]), max (k [,4]) )
	d [5] = d [3] - d [1]
	d [6] = d [4] - d [2]
	d
}

umldims.umlnode = function (v, ...) c (v$x, v$y, v$x, v$y)

recenter.umlnode = function (v, x, y, ...)
{	v$x = x
	v$y = y
}



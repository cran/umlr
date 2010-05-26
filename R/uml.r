uml = function (..., g=umlgraph (...), k=umlcanvas (),
	model.path="model.umlr", image.path="model")
	extend (ENVIRONMENT (g, k, model.path, image.path), "uml")


umlcanvas = function (model.size=c (22, 15), margin.size=0.2, xlim, ylim)
{	if (missing (xlim) ) {x = model.size [1] / 2; xlim = c (-x, x)}
	if (missing (ylim) ) {y = model.size [2] / 2; ylim = c (-y, y)}
	k = list (model.size=model.size, margin.size=margin.size, xlim=xlim, ylim=ylim)
	k$total.size = model.size + margin.size
	k$scaling.factor = c (abs (diff (xlim) ) / model.size [1],
		abs (diff (ylim) ) / model.size [2])
	extend (k, "umlcanvas")
}

#new save generic in oosp instead?
umlsave = function (m, path=m$model.path) save (m, file=path)
umlload = function (path="model.umlr") load (path)

umldims = function (...) UseMethod ("umldims")
umldims.uml = function (m, ...) umldims (m$g)

print.uml = function (m, ...) print (m$g)

recenter = function (...) UseMethod ("recenter")






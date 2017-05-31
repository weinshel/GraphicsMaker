build:
	elm-make src/GraphicsMaker.elm --output docs/GraphicsMaker.js
	cp src/style.css docs/style.css
	cp src/range.css docs/range.css
	cp src/index.html docs/index.html
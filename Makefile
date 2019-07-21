watch: _site/.git
	stack run -- watch

build: _site/.git
	stack run -- build

clean:
	stack run -- clean

_site/.git: clean
	git-new-workdir . _site master

publish:
	cd _site && ((git add -A && git commit -m "Update") || true)
	git push origin master
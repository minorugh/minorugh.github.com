a.out:copy hugo git

copy:
	rsync -auv --delete ~/Dropbox/minorugh/snap/content/post/ ~/src/github.com/minorugh/minorugh.github.io/content/posts/

hugo:
	hugo

git:
	git add . && git diff --cached --exit-code --quiet && echo "\nnothing to commit, working tree clean!"|| \
	git commit -a -m "Updated: `date +'%Y-%m-%d %H:%M:%S'`" && \
	git push origin main

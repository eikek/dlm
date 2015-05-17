SRC_FILES=$(shell find ../src -type f -print0 | xargs -0)
TEST_FILES=$(shell find ../tests -type f -print0 | xargs -0)
LISP=sbcl
GIT=git
EMACS=emacs
VERSION=$(shell sbcl --load build.lisp --quit --eval \
   '(format t "~&~a" (asdf:component-version (asdf:find-system :dlm)))' \
   2> /dev/null | tail -n 1)

ORG_NO_HTMLIZE=(setq org-html-htmlize-output-type nil)

dlm: $(SRC_FILES)
	$(LISP) --load build.lisp --eval '(make-image)'

clean:
	rm -rf dlm-*
	rm -rf README.html

test: $(TEST_FILES)
	$(LISP) --load build.lisp --eval '(test-dlm-and-exit)'

release: clean test dlm
	@echo "Making release for ${VERSION}"
	$(GIT) tag -sm "Release ${VERSION}" release/${VERSION}

readme:
	@echo "Generating HTML Readme file"
	$(EMACS) --file ../README.org --eval '${ORG_NO_HTMLIZE}'  --batch -f org-html-export-to-html
	mv ../README.html .

push-release: release readme
	@echo "Pushing release ${VERSION}"
	$(GIT) push origin release/${VERSION}
	scp dlm-${VERSION} eknet.org:/var/data/www/eknet.org/main/projects/dlm/
	scp README.html  eknet.org:/var/data/www/eknet.org/main/projects/dlm/

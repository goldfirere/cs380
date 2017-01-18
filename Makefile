TEXS := $(wildcard [0-9][0-9]_*/*.tex)
PDFS := $(TEXS:%.tex=%.pdf)

default:
	@echo "Choose what to make."

test:
	echo $(TEXS)
	echo $(PDFS)

web: hakyll/site $(PDFS)
	hakyll/site build

rebuild: hakyll/site
	hakyll/site rebuild

hakyll/site: hakyll/*.hs
	rm -rf _cache _site
	ghc --make hakyll/site.hs

%.pdf: %.tex
	cd $(dir $*); latexmk -pdf $(notdir $*)

clean:
	rm -rf _cache _site
	rm -rf hakyll/*.{o,hi,dyn_o,dyn_hi}
	rm -rf [0-9][0-9]_*/*.{aux,log,out,bbl,blg,ptb,fls,fdb_latexmk,synctex.gz}
	rm -rf $(PDFS)

deploy:
	[[ -z `git status -s` ]]  # deploy only when committed
	[[ `git status -sb` == '## master...origin/master' ]]  # deploy when pushed
	rsync -crptz --delete -e ssh --progress _site/ bmc:~/public_html/courses/17spring380
          # c == use checksums
          # r == recursive
          # p == keep permissions
          # t == keep times
          # z == compress
          # --delete == remove extra files on server
          # --progress == talk to me
          # -e ssh == use SSH

.PHONY: web rebuild deploy default

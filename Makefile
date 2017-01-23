TEXS := $(wildcard [0-9][0-9]_*/*.tex)
LHSS := $(wildcard [0-9][0-9]_*/*.lhs)
PDFS := $(TEXS:%.tex=%.pdf) $(LHSS:%.lhs=%.pdf)

HWS = 01/Intro.hs
STUBS = $(HWS:%=hw/%)

FMTS = etc/rae.fmt

LHS2TEX_PATH = ../etc:/Users/rae/.cabal/share/x86_64-osx-ghc-8.0.1/lhs2tex-1.19

default:
	@echo "Choose what to make."

web: hakyll/site $(PDFS) $(STUBS)
	hakyll/site build

rebuild: hakyll/site
	hakyll/site rebuild

hakyll/site: hakyll/*.hs
	rm -rf _cache _site
	ghc --make hakyll/site.hs

%.pdf: %.tex
	cd $(dir $*); latexmk -pdf $(notdir $*)

%.tex: %.lhs $(FMTS)
	cd $(dir $*); lhs2TeX --path=$(LHS2TEX_PATH) -o $(notdir $@) $(notdir $<)

hw/%.hs: private/hw/%.hs
	hpp -DSTUB $< $@

clean:
	rm -rf _cache _site
	rm -rf hakyll/*.{o,hi,dyn_o,dyn_hi}
	rm -rf [0-9][0-9]_*/*.{aux,log,out,bbl,blg,ptb,fls,fdb_latexmk,synctex.gz}
	rm -rf $(PDFS)
	rm -rf $(LHSS:%.lhs=%.tex)

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

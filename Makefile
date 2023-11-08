.PHONY: all
all: ml 3am

.PHONY: ml
ml:
	mkdir -p vendor/ml
	cp ~/Workspace/mlutils/mlsyntax.lisp              vendor/ml/
	cp ~/Workspace/mlutils/mlutils-package.lisp       vendor/ml/
	cp ~/Workspace/mlutils/mlutils.lisp               vendor/ml/
	cp ~/Workspace/mlutils/net.matteolandi.utils.asd  vendor/ml/

.PHONY: 3am
3am:
	mkdir -p vendor/3am
	cp ~/Workspace/3am/3am.asd          vendor/3am/
	cp ~/Workspace/3am/3am.lisp         vendor/3am/

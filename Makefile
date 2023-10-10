######################
# 
# Created: 2018-06-12
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav
######################

############### FLAGS ###############

VMAJOR 						 = 0
VMINOR 						 = 0
VPATCH  					 = 0
VDEV 							 = .0006
PKG_NAME 					:= HappyCampR

RPKG_USES_RCPP 		:= 0

include ./rpkg_make/Makefile

data-raw/%.csv : campdata/intermediate/%.csv
	cp $< $@

data/%.rda : data-raw/%.csv
	r -l devtools,readr -e '$* <- readr::read_csv("$<");devtools::use_data($*)'

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

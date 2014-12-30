#!/bin/bash
##############################################################################
#
# Generate SVF file from configuration .bit file
#
# $Id: gen_svf.sh,v 1.3 2007/02/05 22:20:55 arnim Exp $
#
# Mandatory environment variables:
#   $MODULE : Name of toplevel project
#   $BLD    : Build directory where project and all temporary files are
#             stored
#
##############################################################################

module=${MODULE:?Environment variable is not defined.}
bld=${BLD:?Environment variable is not defined.}

cd $bld
cat > _gen_svf.cmd <<EOF
setPreference -pref AutoSignature:FALSE
setPreference -pref KeepSVF:FALSE
setPreference -pref ConcurrentMode:FALSE
setPreference -pref UseHighz:FALSE
setPreference -pref svfUseTime:FALSE
setMode -bs
setMode -bs
setCable -port svf -file "$module-tmp.svf"
addDevice -p 1 -file "$module.bit"
Program -p 1 -defaultVersion 0 
quit
EOF

impact -batch _gen_svf.cmd
egrep -v FREQUENCY $module-tmp.svf > $module.svf
\rm -f $module-tmp.svf
\rm -f _impact*

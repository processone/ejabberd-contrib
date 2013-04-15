
MODULES=`pwd`

# Put here the path to ejabberd source
EJADIR=$MODULES/../git/ejabberd/

# TODO: Support extraction of multiple modules 
#MODULE=mod_webpresence
MODULE=mod_register_web

RUNDIR=$MODULES/$MODULE/trunk/
PREPARESCRIPT=$EJADIR/contrib/extract_translations/prepare-translation.sh

# 1. Create the directory $MODULE/msgs/ 

# 2. Create the $MODULE.pot
#$PREPARESCRIPT -rundir $RUNDIR -ejadir $EJADIR -project $MODULE -src2pot

# 3. Create a language
# cp $MODULE.pot $LANG.$MODULE.po
# echo "" > $LANG.$MODULE.msg

# 3.b Convert msg to po. But it doesn't work! :(
#$PREPARESCRIPT -rundir $RUNDIR -ejadir $EJADIR -project $MODULE -srcmsg2po we

# 4. Update strings
$PREPARESCRIPT -rundir $RUNDIR -ejadir $EJADIR -project $MODULE -updateall

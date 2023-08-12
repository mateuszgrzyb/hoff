


HOFF_LIB_DIR="/lib/hoff"
LIBFFI="libffi.so.7.1.0"

# check expected commands

CMDS=("gcc" "ldconfig")
MISSING_CMDS=()

for CMD in ${CMDS[@]}
do
    if ! command -v $CMD &> /dev/null
    then
        MISSING_CMDS+=($CMD)
    fi
done

if (( ${#MISSING_CMDS[@]} ))
then
    echo "there are missing files: ${MISSING_CMDS[@]}"
    exit 1
fi

cd lib

# configure libraries

mkdir $HOFF_LIB_DIR
mv $LIBFFI $HOFF_LIB_DIR
ln -s "$HOFF_LIB_DIR/$LIBFFI" "$HOFF_LIB_DIR/libffi.so"

cat <<EOT >> /etc/ld.so.conf.d/hoff.conf
$HOFF_LIB_DIR
EOT

ldconfig

cd ../

# move compiler

mv hoff /usr/bin

# cleanup
rm -rf lib

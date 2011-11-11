
APP_NAME=billy_client

VSN=$(cat ./src/${APP_NAME}.app.src  | grep vsn | perl -pe 's|.*\"(.*)\".*|\1|')
TAR_GZ="${APP_NAME}-rel-v${VSN}_$(uname -s)-$(uname -m).tar.gz"

cd ./rel
if [ -f "$TAR_GZ" ]; then
	echo "$TAR_GZ - release archive already exists"
	exit 1
fi
tar cz ./${APP_NAME} > $TAR_GZ



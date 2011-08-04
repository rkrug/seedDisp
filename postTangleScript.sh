#!/bin/bash
MAINVERSION=$(cat <<'BABEL_TABLE'
0
BABEL_TABLE
)
SVNVERSION=$(cat <<'BABEL_TABLE'
3
BABEL_TABLE
)
SVNSTATE=$(cat <<'BABEL_TABLE'
0
BABEL_TABLE
)
VER=$(cat <<'BABEL_TABLE'
3
BABEL_TABLE
)
STATE=$(cat <<'BABEL_TABLE'
up-to-date
BABEL_TABLE
)
sed -i s/MAINVERSION/$MAINVERSION/ ./pkg/DESCRIPTION
sed -i s/SVNVERSION/$SVNVERSION/ ./pkg/DESCRIPTION
sed -i s/SVNSTATE/$SVNSTATE/ ./pkg/DESCRIPTION
sed -i s/TODAYSDATE/`date +%Y-%m-%d_%H-%M`/ ./pkg/DESCRIPTION

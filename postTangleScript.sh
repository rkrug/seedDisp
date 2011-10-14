#!/bin/bash
MAINVERSION=$(cat <<'BABEL_TABLE'
0
BABEL_TABLE
)
SVNVERSION=$(cat <<'BABEL_TABLE'
0
BABEL_TABLE
)
SVNSTATE=$(cat <<'BABEL_TABLE'
13
BABEL_TABLE
)
VER=$(cat <<'BABEL_TABLE'
0
BABEL_TABLE
)
STATE=$(cat <<'BABEL_TABLE'
added
BABEL_TABLE
)
sed -i s/MAINVERSION/$MAINVERSION/ ./pkg/DESCRIPTION
sed -i s/SVNVERSION/$SVNVERSION/ ./pkg/DESCRIPTION
sed -i s/SVNSTATE/$SVNSTATE/ ./pkg/DESCRIPTION
sed -i s/TODAYSDATE/`date +%Y-%m-%d_%H-%M`/ ./pkg/DESCRIPTION

sed -i s/MAINVERSION/$MAINVERSION/ ./pkg/seedDisp-package.R
sed -i s/SVNVERSION/$SVNVERSION/ ./pkg/seedDisp-package.R
sed -i s/SVNSTATE/$SVNSTATE/ ./pkg/seedDisp-package.R
sed -i s/TODAYSDATE/`date +%Y-%m-%d_%H-%M`/ ./pkg/seedDisp-package.R

Rscript -e "library(roxygen2);roxygenize('pkg', roxygen.dir='pkg', copy.package=FALSE, unlink.target=FALSE)"

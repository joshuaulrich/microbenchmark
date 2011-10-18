R	:= R
RSCRIPT	:= Rscript

.SILENT:
.PHONEY: clean check build install package usage help

usage:
	echo "Available targets:"
	echo ""
	echo " install  - install the package, writing the output into install.log"
	echo " check    - run R CMD check on the package"
	echo " help     - shows all available targets"

help: usage
	echo " clean    - clean up package cruft"
	echo " package  - build source package of last commit"
	echo " pkg      - roxygenize skel/ into pkg/"

unexport MAKEFLAGS
install: clean pkg
	echo "Installing package..."
	${R} CMD INSTALL pkg > install.log 2>&1

unexport MAKEFLAGS
check: clean pkg
	echo "Running ${R} CMD check..."
	${R} CMD check pkg && rm -fR pkg.Rcheck

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel.Rcheck
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log

package: clean pkg
	echo "Building package..."
	-git stash save -q
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso skel/ > pkg/ChangeLog
	${R} CMD build pkg > build.log 2>&1
	-git stash pop -q
	rm -f pkg/ChangeLog

pkg:
	echo "Roxygenizing package..."
	./tools/roxygenize > roxygen.log 2>&1
	rm -fR pkg/inst ## Stupid roxygen!
	echo "Updating 'Version' field..."
	./tools/set-version


DESTDIR=
prefix=install
exec_prefix=${prefix}
datarootdir=${prefix}/share
datadir=${DESTDIR}${datarootdir}
bindir=${DESTDIR}${exec_prefix}/bin
libdir=${DESTDIR}${exec_prefix}/lib
includedir=${DESTDIR}${prefix}/include
projectsubdir=lib/gnat
projectdir=${DESTDIR}${prefix}/${projectsubdir}

MKDIR=mkdir -p
CP=cp -p
GPRBUILD=gprbuild

all: build

build: force
	${GPRBUILD} -Pgnatbdd_main.gpr -j0 -p

clean: force
	gprclean -Pgnatbdd_main -r

install: force
	gprinstall --prefix=${prefix} -q -p -f --install-name=gnatbdd --project-subdir=${projectsubdir} src/gnatbdd.gpr
	${MKDIR} ${bindir}
	${CP} obj/gnatbdd ${bindir}/

# Adding new scenarios does not erquire recompiling the driver
test: build install build_driver
	-./example/obj/driver --output=full  --features=example/features
	-./example/obj/driver --output=full -o test.html --features=example/features

# Driver only needs to be recompiled when the step definitions change
build_driver: 
# Build example/obj/driver.adb
	${bindir}/gnatbdd -Pexample/calc.gpr
# Compile this driver
	GPR_PROJECT_PATH=${projectdir} ${GPRBUILD} -P example/obj/driver.gpr

force:

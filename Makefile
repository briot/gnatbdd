all: build

build:
	gprbuild -Pgnatbdd.gpr


# Adding new scenarios does not erquire recompiling the driver
test: build_driver
	#-./obj/driver --output=hide_passed
	#-./obj/driver --output=quiet
	#-./obj/driver --output=dots
	-./obj/driver --output=full 
	-./obj/driver --output=full -o test.html

# Driver only needs to be recompiled when the step definitions change
build_driver: features/step_definitions/*
	obj/gnatbdd
	gprbuild -P obj/driver.gpr

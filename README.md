Haskell SpatiaLite Package
==========================

This package ```spatialite``` is based on ```sqlite-simple``` and in turn ```direct-sqlite```
we provide some initialisation for the GIS by loading ```mod_spatialite``` into SQLite3 and
ensuring that the SpatiaLite schema is created.

There is ```GIS``` monad to thread connections among a bunch of xxxxxGIS wrappers for the underlying
 ```sqlite-simple``` query api.

N.B. This is a work in progress and is not yet released or tested. There are a number of package
configuration issues around installing ```direct-sqlite``` with the ```systemlib``` flag that need
to be ironed out, i.e. building amd linking against a ```libsqlite3``` shared library that supports
dynamic module loading and presents the correct api.

The ```mod_spatialite``` shared library needs to be installed also and be compatible.

All of these problems have bitten me, and continue to crop up. So a proper configure based install is required
to ensure this is portable and usuable according to how the libraries have been installed.

Then there's a bunch of GIS schema functions to provide.
Watch this space.

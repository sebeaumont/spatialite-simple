Experimental SpatiaLite Package
===============================

This package ```spatialite-simple``` is based on ```sqlite-simple``` and in turn ```direct-sqlite```
we provide some initialisation for the GIS by loading ```mod_spatialite``` into SQLite3 and
ensuring that the SpatiaLite schema is created.

There is ```GIS``` monad to thread connections among a bunch of xxxxxGIS wrappers for the underlying
 ```sqlite-simple``` query api.

N.B. This is a work in progress and is not yet released or robuts. There are a number of package
configuration issues around installing ```direct-sqlite``` with the ```systemlib``` flag that need
to be ironed out, i.e. building amd linking against a ```libsqlite3``` shared library that supports
dynamic module loading and presents the correct api.

The ```mod_spatialite``` shared library needs to be installed also and be compatible.

All of these problems have bitten me, and continue to crop up. So a proper configure based install is required
to ensure this is portable and usuable according to how the libraries have been installed.

Also I have experienced what I think is a race condition when loading modules in a threaded RTS,
this first appeared in GHCI when running the ```initGIS``` code where I beleive the SQL execute_ to
load the module was running on one thread whilst some further SQL then ran concurrently before the
module is loaded - this is mighty dangerous and resulted in a segvio! One solution is maybe to run all
the initialisation code in a SQL transaction but currenly we use a new ffi procedure in direct-sqlite
to load the module via the c interface - this is marked unsafe and seems to eliminate the race condition.

Alltogether rather a damned faff. I am rethinking using Spatialite at all as we have a native ```geodetics```
package which can do everything that we want on top of ```sqlite-simple```.

Tho' I like the threading of the connection in the GIS monad as this makes implementing queries cleaner.
So maybe we just keep the ```notGIS``` interface and throw the Spatialite module out.
Then there's a bunch of GIS schema functions to provide.

Watch this space.

# g3-data-analysis

To set up renv in an R project, you first install the renv package globally, then initialize it within your project using renv::init(), and finally manage your package dependencies using functions like renv::snapshot() and renv::restore()

install.packages("renv")

### Key renv Functions
renv::init() - Initializes renv in a project, creating the local library and lockfile.
renv::install()	- Installs packages into the project library (similar to install.packages()).
renv::snapshot() - Records the current state of installed packages into the renv.lock file.
renv::restore()	- Reinstalls the exact package versions specified in the renv.lock file.
renv::status() - Checks the consistency of the project environment with the lockfile.
renv::clean() - Removes unused packages from the project library.
rVersion <- '4.0'
renv::restore()

# Documentation, testing, check
devtools::document()

# Build source and binary versions
repoPath <- 'd:/_DEPLOYMENT/hivModelling/pkgBuilds'
deployPath <- 'd:/_DEPLOYMENT/hivEstimatesAccuracy2'
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = repoPath, binary = FALSE)
devtools::build(path = repoPath, binary = TRUE, args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = 'DESCRIPTION'))
version <- as.character(descr$Version)

tarFileName <- paste0('hivModelling_', version, '.tar.gz')
zipFileName <- paste0('hivModelling_', version, '.zip')

# Copy package files to appropriate subfolders
file.copy(
  file.path(repoPath, tarFileName),
  file.path(deployPath, 'repo', 'src', 'contrib', tarFileName),
  overwrite = TRUE
)
file.copy(
  file.path(repoPath, zipFileName),
  file.path(deployPath, 'repo', 'bin', 'windows', 'contrib', rVersion, zipFileName),
  overwrite = TRUE
)

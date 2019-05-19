rVersion <- '3.6'
packrat::restore(overwrite.dirty = TRUE)

# Documentation, testing, check
devtools::document()

# Build source and binary versions
repoPath <- 'd:/_DEPLOYMENT/hivModelling/pkgBuilds'
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = repoPath,
                binary = FALSE)
devtools::build(path = repoPath,
                binary = TRUE,
                args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = 'DESCRIPTION'))
version <- as.character(descr$Version)

tarFileName <- paste0('hivModelling_', version, '.tar.gz')
zipFileName <- paste0('hivModelling_', version, '.zip')

# Copy package files to appropriate subfolders
file.copy(file.path(repoPath, tarFileName),
          file.path(repoPath, '..', 'repo', 'src', 'contrib', tarFileName),
          overwrite = TRUE)
file.copy(file.path(repoPath, zipFileName),
          file.path(repoPath, '..', 'repo', 'bin', 'windows', 'contrib', rVersion, zipFileName),
          overwrite = TRUE)

# Update repo metafiles
tools::write_PACKAGES(dir = file.path(repoPath, '..', 'repo', 'src', 'contrib'), type = 'source')
tools::write_PACKAGES(dir = file.path(repoPath, '..', 'repo', 'bin', 'windows', 'contrib', rVersion), type = 'win.binary')

# Update currect version string
writeLines(version, file.path(repoPath, 'version.txt'))

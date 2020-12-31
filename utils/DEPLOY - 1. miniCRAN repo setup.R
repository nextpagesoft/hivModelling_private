library(miniCRAN)

rVersion <- '4.0'
repoPath <- 'd:/_DEPLOYMENT/hivModelling/repo'
repoCRAN <- 'https://cran.r-project.org/'

descr <- as.data.frame(read.dcf('DESCRIPTION'))
packageVersions <- strsplit(gsub('\n', '', descr$Imports), ',')[[1]]
pkgs <- unname(sapply(
  packageVersions, function(packageVersion) {
    gsub("(\\w+) .*", "\\1", packageVersion)
  }
))
pkgs <- setdiff(
  pkgs,
  c('R', 'grid', 'graphics', 'parallel', 'stats', 'tools', 'utils')
)
pkgList <- pkgDep(pkgs, repos = repoCRAN, type = 'source', suggests = FALSE)
dg <- makeDepGraph(pkgs, repos = repoCRAN, type = 'source', suggests = FALSE)
plot(dg)

if (dir.exists(repoPath)) {
  unlink(repoPath, recursive = TRUE)
}
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)

makeRepo(pkgList, path = repoPath, repos = repoCRAN, type = c('source', 'win.binary'))
oldPackages(path = repoPath)
updatePackages(path = repoPath, repos = repoCRAN, type = 'win.binary', ask = FALSE)
updatePackages(path = repoPath, repos = repoCRAN, type = 'source', ask = FALSE)

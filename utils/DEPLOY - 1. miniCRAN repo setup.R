library(miniCRAN)

rVersion <- '4.0'
repoCRAN <- 'https://cran.r-project.org/'

pkgs <- c(
  'cli', 'data.table', 'fs', 'future', 'ggplot2', 'nloptr', 'xml2', 'Rcpp', 'utils', 'stats'
)
pkgs <- setdiff(pkgs, c('grid', 'graphics', 'parallel', 'stats', 'tools', 'utils'))

pkgList <- pkgDep(pkgs, repos = repoCRAN, type = 'source', suggests = FALSE)

repoPath <- 'd:/_DEPLOYMENT/hivModelling/repo'
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)

makeRepo(pkgList, path = repoPath, repos = repoCRAN, type = c('source', 'win.binary'))

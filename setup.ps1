$ErrorActionPreference = 'Stop'
$Rscript = 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe'
if (-not (Test-Path $Rscript)) { $cmd = Get-Command Rscript -ErrorAction SilentlyContinue; if ($cmd) { $Rscript = $cmd.Source } else { throw 'Rscript was not found. Install R 4.6.1 and restart PowerShell.' } }
$lib = Join-Path $env:LOCALAPPDATA 'R\win-library\4.6'
New-Item -ItemType Directory -Force $lib | Out-Null
$env:R_LIBS_USER = $lib
& $Rscript -e "lib <- Sys.getenv('R_LIBS_USER'); dir.create(lib, recursive=TRUE, showWarnings=FALSE); .libPaths(c(lib,.libPaths())); p <- c('shiny','bslib','dplyr','tidyr','readr','ggplot2','plotly','DT','shinyjs','httr','jsonlite'); m <- p[!vapply(p, requireNamespace, logical(1), quietly=TRUE)]; if(length(m)) install.packages(m, lib=lib, repos='https://cloud.r-project.org'); m <- p[!vapply(p, requireNamespace, logical(1), quietly=TRUE)]; if(length(m)) stop('Could not install: ', paste(m, collapse=', ')); cat('CricLens packages are ready.\n')"
if ($LASTEXITCODE -ne 0) { throw 'Package setup failed. Check internet access and the R messages above.' }

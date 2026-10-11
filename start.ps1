$ErrorActionPreference = 'Stop'
$lib = Join-Path $env:LOCALAPPDATA 'R\win-library\4.6'
$env:R_LIBS_USER = $lib
$Rscript = 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe'
if (-not (Test-Path $Rscript)) { $cmd = Get-Command Rscript -ErrorAction SilentlyContinue; if ($cmd) { $Rscript = $cmd.Source } else { throw 'Rscript was not found.' } }
& $Rscript run_app.R

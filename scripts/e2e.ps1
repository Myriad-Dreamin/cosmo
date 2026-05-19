function RunTestPoint {
    Write-Host ">> Running test point: $args"
    cosmo run $args
    Write-Host "<< Test point completed
"
}

. ./Venv.ps1
RunTestPoint samples/legacy/Class/method.cos
RunTestPoint samples/legacy/Class/staticMethod.cos
RunTestPoint samples/legacy/Io/readFile.cos
RunTestPoint samples/legacy/Vec/push.cos
# RunTestPoint samples/legacy/DataProcessing/readJson.cos
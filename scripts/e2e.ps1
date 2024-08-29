function RunTestPoint {
    Write-Host ">> Running test point: $args"
    cosmo run $args
    Write-Host "<< Test point completed
"
}

. ./Venv.ps1
RunTestPoint samples/Class/method.cos
RunTestPoint samples/Class/staticMethod.cos
RunTestPoint samples/Io/readFile.cos
RunTestPoint samples/Vec/push.cos
# RunTestPoint samples/DataProcessing/readJson.cos
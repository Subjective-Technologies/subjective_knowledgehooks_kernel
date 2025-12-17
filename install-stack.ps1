# PowerShell script to install Haskell Stack on Windows
# Run this script as Administrator

Write-Host "=== Haskell Stack Installation Script ===" -ForegroundColor Cyan
Write-Host ""

# Check if running as Administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "WARNING: Not running as Administrator. Some operations may require admin privileges." -ForegroundColor Yellow
    Write-Host "Right-click PowerShell and select 'Run as Administrator' for best results." -ForegroundColor Yellow
    Write-Host ""
    $continue = Read-Host "Continue anyway? (Y/N)"
    if ($continue -ne "Y" -and $continue -ne "y") {
        exit
    }
}

Write-Host "Step 1: Installing GHCup (Haskell tool installer)..." -ForegroundColor Green
Write-Host "This is the recommended method for Windows." -ForegroundColor Gray
Write-Host ""

try {
    # Set execution policy for this session
    Set-ExecutionPolicy Bypass -Scope Process -Force -ErrorAction SilentlyContinue
    
    # Enable TLS 1.2 for secure downloads
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    
    # Download and run GHCup installer
    Write-Host "Downloading GHCup installer..." -ForegroundColor Yellow
    $ghcupScript = Invoke-WebRequest -UseBasicParsing -Uri 'https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1'
    
    Write-Host "Running GHCup installer..." -ForegroundColor Yellow
    Invoke-Command -ScriptBlock ([ScriptBlock]::Create($ghcupScript.Content)) -ArgumentList $true
    
    Write-Host "GHCup installation completed!" -ForegroundColor Green
    Write-Host ""
    
    Write-Host "Step 2: Installing Stack via GHCup..." -ForegroundColor Green
    Write-Host ""
    
    # Refresh PATH for current session
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    
    # Install Stack
    & ghcup install stack
    & ghcup set stack
    
    Write-Host ""
    Write-Host "=== Installation Complete! ===" -ForegroundColor Green
    Write-Host ""
    Write-Host "IMPORTANT: Please close and reopen PowerShell for PATH changes to take effect." -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Then verify installation with:" -ForegroundColor Cyan
    Write-Host "  stack --version" -ForegroundColor White
    Write-Host ""
    
} catch {
    Write-Host ""
    Write-Host "ERROR: Installation failed!" -ForegroundColor Red
    Write-Host $_.Exception.Message -ForegroundColor Red
    Write-Host ""
    Write-Host "Alternative: Download Stack installer manually from:" -ForegroundColor Yellow
    Write-Host "  https://get.haskellstack.org/stable/windows-x86_64-installer.exe" -ForegroundColor Cyan
    Write-Host ""
    exit 1
}


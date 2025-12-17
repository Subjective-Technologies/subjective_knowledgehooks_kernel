# Windows Installation Guide for Knowledge Hook Kernel

## Quick Start: Install Haskell Stack on Windows

You have **two options** to install Haskell Stack. Choose one:

---

## Option 1: GHCup (Recommended - Modern & Easy)

GHCup is the recommended way to install Haskell tools on Windows.

### Step 1: Install GHCup

1. **Download GHCup Installer:**
   - Visit: https://www.haskell.org/ghcup/install/
   - Click "Windows" tab
   - Download the installer or run the PowerShell command shown on the page

2. **Or run directly in PowerShell (as Administrator):**
   ```powershell
   # Run PowerShell as Administrator (Right-click PowerShell -> Run as Administrator)
   Set-ExecutionPolicy Bypass -Scope Process -Force
   [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
   Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest -UseBasicParsing 'https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1').Content)) -ArgumentList $true
   ```

3. **After installation, close and reopen PowerShell**, then verify:
   ```powershell
   ghcup --version
   ```

### Step 2: Install Stack via GHCup

```powershell
ghcup install stack
ghcup set stack
```

### Step 3: Verify Installation

```powershell
stack --version
```

You should see something like: `Version 2.x.x`

---

## Option 2: Direct Stack Installer

### Step 1: Download Stack Installer

1. Visit: https://get.haskellstack.org/stable/windows-x86_64-installer.exe
2. Download the installer file

### Step 2: Run Installer

1. Double-click the downloaded `.exe` file
2. Follow the installation wizard:
   - Accept the license
   - Choose installation directory (default is fine)
   - **Make sure "Add to PATH" is checked**
   - Complete installation

### Step 3: Verify Installation

1. **Close and reopen PowerShell** (important for PATH to update)
2. Run:
   ```powershell
   stack --version
   ```

---

## After Installation: Build and Run the Project

Once Stack is installed, run these commands in PowerShell:

```powershell
# Navigate to project directory (if not already there)
cd C:\brainboost\brainboost_research\subjective_knowledgehooks_kernel

# Build the project
stack build

# Run the demo
stack run knowledgehook-demo

# Run tests
stack test
```

---

## Troubleshooting

### If `stack` command still not found after installation:

1. **Restart PowerShell completely** (close and reopen)

2. **Check if Stack is in PATH:**
   ```powershell
   $env:PATH -split ';' | Select-String -Pattern "stack"
   ```

3. **Find Stack installation location:**
   - GHCup: Usually in `C:\Users\<YourUsername>\AppData\Roaming\ghcup\bin`
   - Direct installer: Usually in `C:\Program Files\Haskell Stack\<version>\bin`

4. **Manually add to PATH** (if needed):
   ```powershell
   # Run as Administrator
   [Environment]::SetEnvironmentVariable("Path", $env:Path + ";C:\path\to\stack\bin", "User")
   ```
   Replace `C:\path\to\stack\bin` with actual Stack location

### If build fails:

1. **First build might download GHC** (this takes a few minutes):
   ```powershell
   stack setup
   ```

2. **Then build the project:**
   ```powershell
   stack build
   ```

### Alternative: Use WSL (Windows Subsystem for Linux)

If you prefer Linux environment:

1. Install WSL: `wsl --install`
2. Inside WSL, install Stack:
   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   ```
3. Navigate to project and build:
   ```bash
   cd /mnt/c/brainboost/brainboost_research/subjective_knowledgehooks_kernel
   stack build
   ```

---

## What Gets Installed

- **Stack**: Build tool for Haskell projects
- **GHC** (Glasgow Haskell Compiler): Will be downloaded automatically on first `stack build`
- **Cabal**: Package manager (included with GHC)

---

## Next Steps

Once installed successfully:

1. ✅ Verify: `stack --version`
2. ✅ Build: `stack build` 
3. ✅ Test: `stack test`
4. ✅ Run demo: `stack run knowledgehook-demo`

Enjoy working with the Knowledge Hook Kernel! 🚀


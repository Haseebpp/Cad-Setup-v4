<#
.SYNOPSIS
    Registers (or unregisters) the custom AutoCAD system variable MBUTTONORBIT
    under HKLM\SOFTWARE\Autodesk\AutoCAD\R*\ACAD-*\Variables.

.DESCRIPTION
    Creates the registry schema so AutoCAD recognizes MBUTTONORBIT as a native
    per-profile integer system variable (0..1) accessible via the command prompt,
    SETVAR, AutoLISP (getvar/setvar), and .NET Application.TryGetSystemVariable.

    Run from an elevated (Administrator) PowerShell prompt, then restart AutoCAD.

.EXAMPLE
    .\scripts\Register-MButtonOrbit.ps1

.EXAMPLE
    .\scripts\Register-MButtonOrbit.ps1 -Unregister
#>
[CmdletBinding()]
param(
    [ValidateSet(0, 1)]
    [int]$DefaultValue = 1,

    [switch]$Unregister
)

$ErrorActionPreference = 'Stop'

$principal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Error "Administrator privileges are required to write under HKLM:\SOFTWARE\Autodesk\AutoCAD. Re-run this script in an elevated PowerShell prompt."
    exit 1
}

$acadRoot = 'HKLM:\SOFTWARE\Autodesk\AutoCAD'
if (-not (Test-Path $acadRoot)) {
    Write-Error "Registry root not found: $acadRoot"
    exit 1
}

$varContainers = Get-ChildItem $acadRoot -ErrorAction SilentlyContinue |
    ForEach-Object { Get-ChildItem $_.PSPath -ErrorAction SilentlyContinue } |
    Where-Object { $_.PSChildName -like 'ACAD-*' } |
    ForEach-Object { Join-Path $_.PSPath 'Variables' } |
    Where-Object { Test-Path $_ }

if (-not $varContainers) {
    Write-Error "No AutoCAD product Variables keys found under $acadRoot\R*\ACAD-*"
    exit 1
}

foreach ($container in $varContainers) {
    $varKey = Join-Path $container 'MBUTTONORBIT'

    if ($Unregister) {
        if (Test-Path $varKey) {
            Remove-Item -Path $varKey -Recurse -Force
            Write-Host "[OK] Removed $varKey"
        } else {
            Write-Host "[SKIP] Not present: $varKey"
        }
        continue
    }

    if (-not (Test-Path $varKey)) {
        New-Item -Path $varKey -Force | Out-Null
    }

    # Matches AutoCAD's boolean RTSHORT per-profile system variable schema
    Set-ItemProperty -Path $varKey -Name '(Default)'     -Value ([string]$DefaultValue) -Type String
    New-ItemProperty -Path $varKey -Name 'PrimaryType'   -Value 5003 -PropertyType DWord -Force | Out-Null
    New-ItemProperty -Path $varKey -Name 'StorageType'   -Value 2    -PropertyType DWord -Force | Out-Null
    New-ItemProperty -Path $varKey -Name 'SecondaryType' -Value 1    -PropertyType DWord -Force | Out-Null
    New-ItemProperty -Path $varKey -Name 'LowerBound'    -Value 0    -PropertyType DWord -Force | Out-Null
    New-ItemProperty -Path $varKey -Name 'UpperBound'    -Value 1    -PropertyType DWord -Force | Out-Null

    Write-Host "[OK] Registered $varKey (Default=$DefaultValue, Range=0..1, StorageType=PerProfile)"
}

if ($Unregister) {
    $hkcuRoot = 'HKCU:\Software\Autodesk\AutoCAD'
    if (Test-Path $hkcuRoot) {
        Get-ChildItem $hkcuRoot -Recurse -ErrorAction SilentlyContinue |
            Where-Object { $_.PSChildName -eq 'Variables' } |
            ForEach-Object {
                $prop = Get-ItemProperty -Path $_.PSPath -Name 'MBUTTONORBIT' -ErrorAction SilentlyContinue
                if ($null -ne $prop) {
                    Remove-ItemProperty -Path $_.PSPath -Name 'MBUTTONORBIT' -ErrorAction SilentlyContinue
                    Write-Host "[OK] Cleaned user profile value in $($_.PSPath)"
                }
            }
    }
}

Write-Host "[Notice] Restart AutoCAD for registry schema changes to take effect."


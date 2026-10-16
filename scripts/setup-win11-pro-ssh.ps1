#Requires -RunAsAdministrator
$ErrorActionPreference = 'Stop'

# Run locally in the win11-pro VM after Windows setup. Never copy the private key.
$publicKey = 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsFe/LEbPeyh0LXXp4y8kn72A7lNRScI9bLX7gaiFY7 win11-pro-vm'
$keyFile = Join-Path $env:ProgramData 'ssh\administrators_authorized_keys'

$capability = Get-WindowsCapability -Online -Name 'OpenSSH.Server~~~~0.0.1.0'
if ($capability.State -ne 'Installed') {
    Add-WindowsCapability -Online -Name 'OpenSSH.Server~~~~0.0.1.0' | Out-Null
}

Set-Service -Name sshd -StartupType Automatic
if ((Get-Service -Name sshd).Status -ne 'Running') {
    Start-Service -Name sshd
}

if (-not (Test-Path -LiteralPath $keyFile)) {
    New-Item -ItemType File -Path $keyFile -Force | Out-Null
}
if (-not (Select-String -LiteralPath $keyFile -Pattern $publicKey -SimpleMatch -Quiet)) {
    Add-Content -LiteralPath $keyFile -Value $publicKey -Encoding ascii
}

# Windows OpenSSH requires only Administrators and SYSTEM to access this file.
& icacls.exe $keyFile /inheritance:r /grant:r '*S-1-5-32-544:F' '*S-1-5-18:F' | Out-Null
if ($LASTEXITCODE -ne 0) { throw 'Failed to set authorized_keys permissions.' }

# Windows normally creates an unrestricted SSH firewall rule during installation.
# Scope that rule to the Fedora host on the libvirt NAT network.
$rule = Get-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -ErrorAction SilentlyContinue
if ($rule) {
    Set-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -Enabled True -Profile Any -RemoteAddress '192.168.124.1'
} else {
    New-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -DisplayName 'OpenSSH Server (sshd)' `
        -Enabled True -Direction Inbound -Profile Any -Protocol TCP -LocalPort 22 `
        -Action Allow -RemoteAddress '192.168.124.1' | Out-Null
}

Write-Host 'OpenSSH server ready. SSH key authentication must be tested before disabling passwords.'
Write-Host "Windows account: $(whoami)"
Write-Host 'Host key fingerprint:'
& ssh-keygen.exe -lf "$env:ProgramData\ssh\ssh_host_ed25519_key.pub"

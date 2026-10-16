---
name: windows-vm-dev
description: Use the local win11-pro Windows 11 libvirt VM as a fast build/test worker for Linux-developed Python, PyInstaller, Rust and C/C++ projects. Covers lifecycle, manual RAM targets, incremental rsync-over-SSH, uv and test feedback. Use when asked to test, build, sync or troubleshoot a project in the Windows VM.
---

# Windows VM development worker

Keep editing on Fedora. Do not use Git pushes, set up a Windows Git identity, or run a full CI workflow merely to test a local change. Sync the worktree incrementally, run Windows-native commands over SSH, and report test results. Do not install more toolchains unless the project needs them and the user approves.

## Known local configuration

- libvirt system domain: `win11-pro` (`virsh -c qemu:///system`). It is normally shut off; preserve its initial power state after a one-off task unless asked otherwise.
- Host SSH alias: `win11-pro` -> guest account `h`. Do not inspect or copy its SSH private key. Guest OpenSSH uses Windows PowerShell 5.1 as the default shell.
- Domain memory: **6 GiB persistent current, 16 GiB maximum**, VirtIO Balloon installed. Neither increases nor decreases happen automatically. Raise a running VM explicitly with `virsh -c qemu:///system setmem win11-pro 8G --live` (up to 16G), and lower it when appropriate. `--live` alone does not change the next boot's 6 GiB baseline. Before lowering, check guest available RAM to avoid starving Windows; before raising, check host memory with `free -h`.
- Guest `uv.exe`: `C:\Users\h\.local\bin\uv.exe`. Use the explicit path in SSH sessions; a new SSH session may not have the installer directory on PATH. Guest rsync: `C:\msys64\usr\bin\rsync.exe` (MSYS2); host rsync is `/usr/bin/rsync`.
- The Windows ISO is ejected; do not reattach installers for routine testing. Keep Secure Boot enabled.

## Fast edit -> test loop

1. Inspect project-specific build/test instructions and select a safe project directory name (`PROJECT`, simple letters, digits, hyphens or underscores). Do not assume every project uses pytest or uv.
2. Check `virsh -c qemu:///system domstate win11-pro`; start with `virsh -c qemu:///system start win11-pro` only if needed. Wait for `ssh -o BatchMode=yes -T win11-pro 'Write-Output ready'` to succeed. Guest SSH is available only while Windows is running.
3. Ensure the dedicated destination exists (PowerShell runs remotely):

   `ssh -T win11-pro 'New-Item -ItemType Directory -Force -Path "$env:USERPROFILE\dev\PROJECT" | Out-Null'`

4. From Fedora, run incremental rsync, with a trailing slash on the source to transfer its *contents*. Substitute an absolute project path and the same simple `PROJECT` in both commands:

   `rsync -rt --omit-dir-times --exclude=.git/ --exclude=.ssh/ --exclude=.env --exclude=.env.* --exclude='*.pem' --exclude='*.key' --exclude='*.pfx' --exclude=.venv/ --exclude=__pycache__/ --exclude=.pytest_cache/ --exclude=.mypy_cache/ --exclude=.ruff_cache/ --exclude=target/ --exclude=build/ --exclude=dist/ --exclude=node_modules/ -e ssh --rsync-path='& C:/msys64/usr/bin/rsync.exe' '/absolute/path/to/project/' 'win11-pro:/c/Users/h/dev/PROJECT/'`

   These options sync changed files by size/mtime; they do not replicate Linux ownership or modes. Re-run after each edit. Default is **no deletion**: retained files on the guest are safer but stale renamed/deleted files can affect tests. For a clean mirror, dry-run `--delete` and inspect every removal first; never delete outside the explicitly dedicated project directory. Beware of case-only renames, symlinks (not copied by `-rt`) and generated output; keep guest-only `.venv`, `target`, `build`, etc. out of the host transfer. Do not sync credentials, `.env` files or secrets without explicit approval. For sensitive repositories use an allowlist of paths instead of a broad sync.

5. Run project-specific tests; for a Python project with `uv.lock` and pytest declared as a dependency, for example:

   `ssh -T win11-pro 'Set-Location "$env:USERPROFILE\dev\PROJECT"; & "$env:USERPROFILE\.local\bin\uv.exe" sync --locked; if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }; & "$env:USERPROFILE\.local\bin\uv.exe" run --locked pytest -q; exit $LASTEXITCODE'`

   Use `uv run --locked python -m pytest` if pytest is installed as a module rather than a script; if no lockfile exists, use the project's own uv instructions rather than forcing `--locked`. Check SSH exit status and return concise stdout/stderr to the user. Re-running `uv sync --locked` is cheap when dependencies are unchanged. Run PyInstaller **inside Windows**, after installing its dependencies there, and test the resulting executable there. Rust/C++ require separate Windows-native toolchains; do not claim they are installed.

6. If artifacts are needed on Fedora, retrieve only the relevant paths with `scp` or reverse `rsync` after verifying the destination. Avoid copying entire Windows environments or build caches. Shut the VM down with `virsh -c qemu:///system shutdown win11-pro` if this task started it and the user did not request it stay running; wait for `domstate` to say `shut off`.

## Safety and troubleshooting

- `--rsync-path='& C:/msys64/usr/bin/rsync.exe'` matters: Windows OpenSSH starts PowerShell, not a Unix shell. The `&` call operator launches the MSYS2 executable. Use simple guest paths under `/c/Users/h/dev/`; test quoting carefully before syncing a path with spaces or special characters. This was verified with an SSH rsync dry run, not assumed from Unix behavior.
- Do not open rsync's unencrypted daemon port 873, make shared folders writable, disable SSH authentication, weaken Windows firewall, or change the VM's configured baseline just to run tests.
- A failed test is feedback, not a reason to silently mutate dependencies or host/guest security settings. Check `virsh ... dommemstat win11-pro` and Windows free memory before assuming a failure is caused by the balloon.
- The VM uses a persistent user profile and build caches; do not run destructive cleanup, use `--delete` unreviewed, or shut it down if it was already running before the task.

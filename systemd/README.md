# User services

Link these unit files and drop-ins into `~/.config/systemd/user`, or use Stow.
Noctalia's unit is supplied separately by the `uwsm` package.

Run `systemctl --user daemon-reload` after installing or editing units.
Enablement links under `*.wants/` and `*.requires/` are machine-local and are
not tracked. Environment/credential files referenced by units remain private.
Installing the files does not imply enabling or restarting every service.

## Per-customer Agent Shell uv credentials

The `agent-uv-credentials@` user service runs the trusted host-side
`refresh-agent-uv-credentials CUSTOMER` helper. It acquires a token and passes
it on stdin to `uv --no-config --no-preview auth login`, with generic keyring
access and native-auth preview features disabled. Credentials are plaintext files in a
private directory, not a keyring. Agent Shell must map each customer to only
that customer's store; this service does not configure or start agents.
Normal terminal authentication is unchanged.

Customer IDs must match `[a-z0-9][a-z0-9_-]*` in full. `_unconfigured` is
reserved for the empty agent fallback and cannot be refreshed. The helper reads
`$XDG_CONFIG_HOME/agent-uv-auth/CUSTOMER.toml` and writes only to
`$XDG_DATA_HOME/uv/agent-credentials/CUSTOMER`. An unset, empty, or relative
XDG variable falls back to `~/.config` or `~/.local/share`, respectively.
Ensure the trusted host and agent use the same absolute data root. The helper
sets umask `077` and makes the customer store mode `0700`.

### Install and operate

Install the scripts and systemd Stow packages, or link the helper to
`~/.local/bin/refresh-agent-uv-credentials` and both templates to
`~/.config/systemd/user/`. The helper requires Python 3.11+ and an installed
`uv` supporting `UV_CREDENTIALS_DIR` and `uv auth login --password -`.
Make the installed helper executable:

```sh
chmod 700 ~/.local/bin/refresh-agent-uv-credentials
mkdir -p ~/.config/agent-uv-auth
chmod 700 ~/.config/agent-uv-auth
systemctl --user daemon-reload
```

Create a private customer config as below and `chmod 600` it. The service sets
`PATH` to `~/.local/bin:/usr/local/bin:/usr/bin:/bin`; install `uv` and provider
commands there or specify an absolute provider executable in `command`.
Manual refresh and optional timer activation, using a fictional customer:

```sh
~/.local/bin/refresh-agent-uv-credentials northwind
systemctl --user enable --now agent-uv-credentials@northwind.timer
systemctl --user start agent-uv-credentials@northwind.service
systemctl --user list-timers 'agent-uv-credentials@*'
journalctl --user -u agent-uv-credentials@northwind.service
```

Installing these files does **not** enable anything. The timer first fires two
minutes after the user service manager starts (normally login, or boot with
lingering), then every 30 minutes after activation. If enabled after that first
deadline it may fire immediately. Prefer one-hour provider tokens so a refresh
can fail without immediately interrupting access. No linger configuration is
changed. To stop automatic refresh:
`systemctl --user disable --now agent-uv-credentials@northwind.timer`.

### Trusted config and execution boundary

These are host-owned executable configurations, **not agent input**. Anyone who
can edit a config, its helper, or executables resolved through its `PATH` can
execute arbitrary commands with your host privileges. Keep configs and provider
credentials outside agent mounts; do not accept configs from repositories or
agent-generated files. Customer identifiers are not secrets, but real customer
names and configuration must remain untracked.

Each TOML file has exactly `service`, `username`, `command`, and optionally
`environment`. `service` is an HTTPS origin only (an optional trailing slash is
accepted): no userinfo, repository path, query, or fragment. `command` is a
nonempty argv array, executed directly without a shell. `environment` maps
variable names to strings, overlaid on the host environment for both the
provider and uv. Only an environment value equal to `~` or beginning `~/`
expands to the host home directory; command arguments have no expansion.
There is no `$VARIABLE`, command substitution, or other shell evaluation.
The helper overrides `UV_CREDENTIALS_DIR` and `UV_KEYRING_PROVIDER` for uv,
removes inherited `UV_CONFIG_FILE`, `UV_PREVIEW`, and `UV_PREVIEW_FEATURES`,
and passes `--no-preview`. It does not pass an invalid empty feature list.

The provider must print only the token on stdout and exit successfully.
Leading/trailing whitespace is stripped. Provider stdin is closed, so renew
interactive sessions separately on the trusted host. The helper captures token
stdout, suppresses provider stderr and uv stdout/stderr, and reports only the
failing stage and exit code (or a generic startup/configuration error). Do not
wrap the provider in commands that log tokens elsewhere. Failed or empty token
acquisition does not invoke uv or modify the existing customer credentials.
Failures inside uv itself are reported without a transactional rollback
guarantee.

### AWS CodeArtifact example (fictional)

Prerequisites: install AWS CLI v2, provision a dedicated read-only CodeArtifact
role/profile, and grant only the required repository reads plus
`codeartifact:GetAuthorizationToken` and `sts:GetServiceBearerToken` (scoped
appropriately; the latter uses the CodeArtifact service condition). The role
must already exist in the customer's account; this helper does not create IAM
roles, configure cloud resources, or acquire permissions. The following names,
account, region, and SSO URL are examples to replace privately.

Put this in `~/.config/agent-uv-auth/northwind.toml`:

```toml
service = "https://northwind-111122223333.d.codeartifact.eu-west-1.amazonaws.com"
username = "aws"
command = [
  "aws", "--profile", "agent-northwind", "--region", "eu-west-1",
  "codeartifact", "get-authorization-token",
  "--domain", "northwind", "--domain-owner", "111122223333",
  "--duration-seconds", "3600", "--query", "authorizationToken",
  "--output", "text", "--no-cli-pager",
]

[environment]
AWS_CONFIG_FILE = "~/.config/aws/agent-northwind/config"
AWS_SHARED_CREDENTIALS_FILE = "~/.local/share/aws/agent-northwind/credentials"
AWS_PAGER = ""
```

Create the two private parent directories with mode `0700`. Put this dedicated
SSO profile in `~/.config/aws/agent-northwind/config` (mode `0600`):

```ini
[profile agent-northwind]
sso_session = agent-northwind
sso_account_id = 111122223333
sso_role_name = AgentPackageRead
region = eu-west-1

[sso-session agent-northwind]
sso_start_url = https://northwind.awsapps.com/start
sso_region = eu-west-1
sso_registration_scopes = sso:account:access
```

`~/.local/share/aws/agent-northwind/credentials` is the explicitly selected
shared credentials path; an SSO profile does not require a credentials file.
Do not put static access keys there merely to make this example work.
Authenticate on the trusted host using the same explicit paths:

```sh
AWS_CONFIG_FILE="$HOME/.config/aws/agent-northwind/config" \
AWS_SHARED_CREDENTIALS_FILE="$HOME/.local/share/aws/agent-northwind/credentials" \
aws sso login --profile agent-northwind
chmod 600 ~/.config/agent-uv-auth/northwind.toml
~/.local/bin/refresh-agent-uv-credentials northwind
```

SSO caches remain host-private under AWS CLI's normal cache locations. The
timer refreshes CodeArtifact tokens only while that SSO session permits it;
renew expired SSO sessions interactively with the same `aws sso login` command.
Use an index URL such as
`https://northwind-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/python/simple/`
in the agent's package configuration, without embedding a password.

### Azure Artifacts example (fictional)

Install the Python `keyring` CLI with `artifacts-keyring` and the runtime needed
by its Azure Artifacts Credential Provider. A PAT is **not required**: the
provider can use an existing sign-in/session cache or interactive Azure login.
Explicitly select the Azure backend instead of the general backend chain.

Put this in `~/.config/agent-uv-auth/contoso.toml`, mode `0600`:

```toml
service = "https://pkgs.dev.azure.com"
username = "VssSessionToken"
command = [
  "keyring", "--keyring-backend", "artifacts_keyring.ArtifactsKeyringBackend",
  "get",
  "https://pkgs.dev.azure.com/contoso/packages/_packaging/python/pypi/simple/",
  "VssSessionToken",
]

[environment]
ARTIFACTS_KEYRING_NONINTERACTIVE_MODE = "true"
```

The provider receives the full feed URL, while uv stores the resulting package
credential against the HTTPS origin. It runs only in the trusted host
refresher; agent subprocess keyring access remains disabled. Existing Azure
sign-in/session caches are not copied into the agent environment.

Then run `~/.local/bin/refresh-agent-uv-credentials contoso` and enable
`agent-uv-credentials@contoso.timer` as above. Configure the agent's index URL
without a password, for example
`https://VssSessionToken@pkgs.dev.azure.com/contoso/packages/_packaging/python/pypi/simple/`.
The refresh username must match the username in the selected uv index URL.
All feeds sharing this HTTPS origin share the customer store's credential.

The timer is noninteractive. If Azure requires sign-in again, renew it from a
trusted terminal, discarding the returned secret rather than printing it:

```sh
env ARTIFACTS_KEYRING_NONINTERACTIVE_MODE=false \
  keyring --keyring-backend artifacts_keyring.ArtifactsKeyringBackend get \
  https://pkgs.dev.azure.com/contoso/packages/_packaging/python/pypi/simple/ \
  VssSessionToken > /dev/null
```

Then start the customer's refresh service again. This flow does not require
AWS credentials and does not grant general Azure Resource Manager access.
The resulting package credential follows the Azure DevOps identity's
permissions: it is **not automatically read-only** and may permit publishing.
Use a restricted identity or a **Packaging: Read** PAT if that narrower
authorization is required.

A PAT remains an optional alternative: store it interactively using
`keyring --keyring-backend keyring.backends.SecretService.Keyring set agent-uv-contoso packaging-read`
and configure `command` to retrieve that same entry with the Secret Service
backend. PAT expiry/revocation requires replacement; copying it on a timer does
not renew it. Avoid plaintext/fallback keyring backends.

### Manual smoke scenarios

After installing real private configuration, refresh each customer separately
and check that only its selected store changes and retains mode `0700`.
Exercise an invalid customer ID (`../other` and `_unconfigured`), a service
with HTTP, userinfo, or a repository path, and malformed TOML. With existing
credentials, exercise a provider that fails, a successful provider returning
empty/whitespace stdout, and a missing provider executable: all must leave
credentials untouched. Check absent and relative XDG roots use the documented
fallbacks and absolute roots select the intended directories. Finally exercise
an expired SSO session/locked keyring, manual session renewal, a successful
agent package read, and timer execution. Inspect the journal for stage-only
errors, never tokens; do not dump credential files to logs while inspecting.

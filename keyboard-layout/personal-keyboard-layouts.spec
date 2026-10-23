%global xkb_extension_root %(pkg-config --variable=xkb_unversioned_extensions_path xkbcommon)
%global xkb_extension_dir %{xkb_extension_root}/%{name}

Name:           personal-keyboard-layouts
Version:        1.0
Release:        1%{?dist}
Summary:        Personal XKB layouts for Nordic keyboards
License:        LicenseRef-NotLicensed
BuildArch:      noarch

Source0:        uf
Source1:        evdev.xml

BuildRequires:  pkgconfig(xkbcommon) >= 1.13
BuildRequires:  xkeyboard-config >= 2.45
Requires:       libxkbcommon >= 1.13
Requires:       xkeyboard-config >= 2.45

%description
The uf layout emulates a US keyboard on Nordic hardware, extends the left
Shift key, and swaps the Return and backslash keys. Installs an additive XKB
extension and layout-picker metadata without changing keyboard selection,
compositor configuration, or distribution-owned XKB files.

%prep

%build

%install
install -Dpm 0644 "%{SOURCE0}" "%{buildroot}%{xkb_extension_dir}/symbols/uf"
install -Dpm 0644 "%{SOURCE1}" "%{buildroot}%{xkb_extension_dir}/rules/evdev.xml"

%files
%dir %{xkb_extension_root}
%{xkb_extension_dir}/

%changelog
* Tue Oct 20 2026 henkka - 1.0-1
- Package uf and its registry entry using the XKB extension directory.

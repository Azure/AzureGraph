# AzureGraph 1.0.5.9000

- Treat the access token as opaque; this prevents errors when logging in without an AAD tenant.

# AzureGraph 1.0.5

- Fix a bug in user methods for listing objects when the result is empty.
- Fix a bug in retrieving users added to an Azure Active Directory (AAD) tenant from an external directory.

# AzureGraph 1.0.4

- Allow AAD v2.0 tokens to be used for authenticating. Note that AAD v1.0 is still the default and recommended version.
- Use `utils::askYesNo` for confirmation prompts on R >= 3.5, eg when deleting objects; this fixes a bug in reading the input. As a side-effect, Windows users who are using RGUI.exe will see a popup dialog box instead of a message in the terminal.
- Various other bug fixes.

# AzureGraph 1.0.3

- Improved handling of null object properties.

# AzureGraph 1.0.2

- Changes to login functionality to better accommodate AzureAuth options. As part of this, the `config_file` argument for `az_graph$new` has been removed; to use a configuration file, call the (recommended) `create_graph_login` function.

# AzureGraph 1.0.1

- Fix some bugs in the login functionality.
- Add direct support for creating apps with certificate credentials.

# AzureGraph 1.0.0

- Submitted to CRAN

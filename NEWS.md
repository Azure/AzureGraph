# AzureGraph 1.2.0.9000

- Allow setting an optional limit to the number of objects returned by the private `ms_object$get_paged_list()` method.
- The private `ms_object$init_list_objects()` method now has a `...` argument to allow passing extra parameters to class constructors.
- Add documentation on how to use `get_paged_list` and `init_list_objects`.

# AzureGraph 1.2.0

- Internal refactoring to support future extensibility, including transferring some utility functions from AzureRMR to here.
- New "Extending AzureGraph" vignette, showing how to extend this package to represent other object types in Microsoft Graph.
- Switch to AAD v2.0 as the default for authenticating.
- Enhance `get_graph_login` to allow specifying scopes.

# AzureGraph 1.1.2

- Change maintainer email address.

# AzureGraph 1.1.1

- Switch to the v1.0 REST endpoint.

# AzureGraph 1.1.0

- Updated to use the new Graph API calls for managing app passwords. Call the `az_app$add_password()` method to add a password to an app, and `az_app$remove_password()` to remove it. As a security measure, app passwords can no longer be manually specified; instead all passwords are now auto-generated on the server with a cryptographically secure PRNG.
  - The `az_app$update_password()` method is defunct.
- Better handling of app creation with certificates:
  - The `certificate` argument to `ms_graph$create_app()` can be the name of a .pfx or .pem file, an `openssl::cert` object, an `AzureKeyVault::stored_cert` object, or a raw or character vector containing the certificate.
  - New `az_app$add_certificate()` and `az_app$remove_certificate()` methods, matching `add_password` and `remove_password`.
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

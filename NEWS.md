# AzureGraph 1.3.2

- Minor backend fixes.

# AzureGraph 1.3.1

- Fix a bug in `ms_object$get_list_pager()` where the `default_generator` argument wasn't being used.
- Add basic print methods for the `ms_graph_pager` and `graph_request` R6 classes.
- Add "Authentication basics" vignette providing more information on this topic.

# AzureGraph 1.3.0

- New API for working with paged result sets:
  - New `ms_graph_pager` R6 class, which is an _iterator_ for the pages in the result.
  - The `ms_object` base class now has a `get_list_pager()` method which returns an object of class `ms_graph_pager`.
  - New `extract_list_values()` function to get all or part of the results from a paged result set.
- The current (private) `ms_object$get_paged_list()` and `ms_object$init_list_objects()` methods are retained for backward compatibility, but are otherwise deprecated.
- The `ms_graph$get_user()` method can now get a user by email or display name. Similarly, the `get_group()` method can get a group by display name.
- Fix a bug in retrieving a paged list of values as a data frame, when `n` (the maximum number of rows) is supplied.
- New `ms_graph$get_aad_object()` method to retrieve an Azure Active Directory object by ID. Mostly intended for use with the `list_object_memberships()` and `list_group_memberships()` methods, which return only IDs and not full object information.
- All `list_*` R6 methods now have `filter` and `n` arguments to filter the result set and cap the number of results. The default values are `filter=NULL` and `n=Inf`. If `n=NULL`, the `ms_graph_pager` iterator object is returned instead to allow manual iteration over the results.
- Export the `find_class_generator()` function.
- New "Batching and paging" vignette describing these APIs.
- Add `list_users()`, `list_groups()`, `list_apps()` and `list_service_principals()` methods to the main `ms_graph` client class.

# AzureGraph 1.2.2

- Add support for batch requests:
  - Each individual request is stored in an object of R6 class `graph_request`.
  - Add `call_batch_endpoint()` function and `ms_graph$call_batch_endpoint()` method for calling the batch endpoint with a list of requests.
- Handle throttling (HTTP 429 errors) gracefully.

# AzureGraph 1.2.1

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

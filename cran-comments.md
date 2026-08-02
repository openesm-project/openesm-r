## Resubmission

This is a resubmission addressing the note and error from the 0.2.0 check.

Changes in version 0.2.1:

* Fixed NOTE "checking for new files in some other directories": examples that
  call `list_datasets()` or `get_dataset()` are now guarded with
  `@examplesIf interactive()`, preventing execution in non-interactive check
  environments and eliminating writes to the user cache directory during checks.
* Replaced live `get_dataset()` calls in the examples for `cite()`, `notes()`,
  `print.openesm_dataset()`, and `print.openesm_dataset_list()` with minimal
  mock objects.
* `get_cache_dir()` now accepts a `create` argument; `cache_info()` and
  `clear_cache()` pass `create = FALSE` so they no longer create the cache
  directory as a side effect of inspecting it.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

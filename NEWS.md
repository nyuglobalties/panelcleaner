# panelcleaner 0.0.5

* Add `replace_missing_with_na` parameter to `homogenize_panel()` to allow panelcleaner to create
  variables in the waves that are missing their raw variables. This is helpful during data collection
  when not all variables have had submissions but you want to keep the original panel mapping
  specification.
  
# panelcleaner 0.0.4

* Fix bug where panelcleaner would not subset the variables to those that are present in the wave database if the user wants to allow missing variables to not stop homogenization
* Add specific error message for duplicated names to reduce confusion

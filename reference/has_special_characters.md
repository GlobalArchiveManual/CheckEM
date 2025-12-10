# Check if a String Contains Special Characters

This function checks whether a given string contains any special
characters. Special characters are defined as any character that is not
alphanumeric (letters or numbers).

## Usage

``` r
has_special_characters(string)
```

## Arguments

- string:

  A character string to be checked for special characters.

## Value

A logical value: `TRUE` if the string contains any special characters,
`FALSE` otherwise.

## Examples

``` r
# Check if a string contains special characters
has_special_characters("HelloWorld123") # Returns FALSE
#> [1] FALSE
has_special_characters("Hello@World!")  # Returns TRUE
#> [1] TRUE
```

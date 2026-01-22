library(reticulate)

# Install Dan's wrapper for Jim's library
virtualenv_create("test")
virtualenv_install("https://github.com/AutomatedFishID/emtmlibpy.git")

# Try importing the installed wrapper
import("emtmlibpy")

virtualenv_install(envname = "test", "https://github.com/AutomatedFishID/emtmlibpy.git")

py_install("https://github.com/AutomatedFishID/emtmlibpy.git")

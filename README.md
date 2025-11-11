# AMP website documentation

## R Project Setup with `renv`

This project uses the `renv` package to manage R package versions. This ensures that everyone working on the project has the exact same package versions, which prevents "it works on my machine" errors and library conflicts.

Your setup is simple and you only need to do it once.

### One-Time Setup Instructions

1.  **Clone the Repository:** Clone this project from Git onto your computer.
2.  **Open the Project:** Open the `acropora-monitoring-program.Rproj` file in RStudio.
3.  **Install `renv` (if needed):** If you've never used `renv` before, you may need to install it. Run this in the R console:
    ```r
    install.packages("renv")
    ```
4.  **Restore the Library:** When you open the project, the `.Rprofile` file should automatically activate `renv`. You may see a message in the console. To install all the correct package versions, simply run:
    ```r
    renv::restore()
    ```
    This command will read the `renv.lock` file, which is the project's "recipe," and install all the packages listed in it. Say "yes" (Y) to any prompts.

That's it\! Your project library is now set up and matches the one used by the rest of the team. You can now render the Quarto website or run any of the R scripts.

### Ongoing Work

If you pull changes from Git and see that the `renv.lock` file has been updated, it means a package was added or changed.

Just run `renv::restore()` again to sync your library with the new lockfile.
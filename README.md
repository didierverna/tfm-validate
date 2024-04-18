# TFM
TFM (for TeX Font Metrics) is the standard font description format used by
TeX. The TFM Validate library provides facilities for checking the correctness
of TFM files and generating non-compliance reports.

TFM Validate depends on [TFM](https://github.com/didierverna/tfm) for parsing
TFM files, and couldn't exist without the powerful Common Lisp Condition
System!


## Quick Start
In your favorite Lisp REPL, type something like this:
```
(asdf:load-system :net.didierverna.tfm-validate)
(net.didierverna.tfm-validate:nickname-package)
```

You may now check the validity of a TFM file by doing:
```
(tfm-validate:invalidate-font "/some/file.tfm")
```
If the file is valid, this function returns `nil`. Otherwise, it returns a
list of encountered warnings and errors (Common Lisp conditions are first
class objects!).

You may also call the function `invalidate-directory` to get TFM compliance
reports for a whole directory tree.

Finally, call the function `(invalidate-texlive YEAR)` to generate a website
full of compliance reports. The site is located in `~/tfm-validate`.

"""
Python dot expression completion using Pymacs.

This almost certainly needs work, but if you add

    (require 'pycomplete)

to your .xemacs/init.el file (.emacs for GNU Emacs) and have Pymacs
installed, when you hit TAB it will try to complete the dot expression
before point.  For example, given this import at the top of the file:

    import time

typing "time.cl" then hitting TAB should complete "time.clock".

See pycomplete.el for the Emacs Lisp side of things.
"""
import sys
import os.path
import string 
from Pymacs import lisp

sys.path.append(".")

try:
    x = set
except NameError:
    from sets import Set as set
else:
    del x

def get_all_completions(s, imports=None):
    """Return contextual completion of s (string of >= zero chars).

    If given, imports is a list of import statements to be executed first.
    """
    locald = {}
    if imports is not None:
        for stmt in imports:
            try:
                exec stmt in globals(), locald
            except TypeError:
                raise TypeError, "invalid type: %s" % stmt
            except:
                continue
    dots = s.split(".") 
    if not s or len(dots) == 1:
        keys = set()
        keys.update(locald.keys())
        keys.update(globals().keys())
        import __builtin__
        keys.update(dir(__builtin__))
        keys = list(keys)
        keys.sort()
        if s:
            return [k for k in keys if k.startswith(s)]
        else:
            return keys

    sym = None
    for i in range(1, len(dots)):
        s = ".".join(dots[:i])   
        try:
            sym = eval(s, globals(), locald)
        except NameError:
            try:
                sym = __import__(s, globals(), locald, [])
            except ImportError:
                return []
    if sym is not None:  
        s = dots[-1]     
        return [k for k in dir(sym) if k.startswith(s)]

def pycomplete(s, imports=None):
    completions = get_all_completions(s, imports)
    dots = s.split(".")
    result = os.path.commonprefix([k[len(dots[-1]):] for k in completions])

    if result == "":
        if completions:
            width = lisp.window_width() - 2
            colum = width / 20
            white = "                    "

            msg = ""

            counter = 0
            for completion in completions :
                if completion.__len__() < 20 :
                    msg += completion + white[completion.__len__():]
                    counter += 1
                else :
                    msg += completion + white[completion.__len__() - 20:]
                    counter += 2

                if counter >= colum :
                    counter = 0
                    msg += '\n'

        else:
            msg = "no completions!"
        lisp.message(msg)
    return  result       

if __name__ == "__main__":
    print " ->", pycomplete("")
    print "sys.get ->", pycomplete("sys.get")
    print "sy ->", pycomplete("sy")
    print "sy (sys in context) ->", pycomplete("sy", imports=["import sys"])
    print "foo. ->", pycomplete("foo.")
    print "Enc (email * imported) ->", 
    print pycomplete("Enc", imports=["from email import *"])
    print "E (email * imported) ->",
    print pycomplete("E", imports=["from email import *"])

    print "Enc ->", pycomplete("Enc")
    print "E ->", pycomplete("E")

# Local Variables :
# pymacs-auto-reload : t
# End :

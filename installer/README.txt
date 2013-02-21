My Emacs For Common Lisp
0.1
by Xiaofeng Yang ( n.akr.akiiya at gmail dot com )

EMACS 23.1:
    With some extensions.
    Custom the `.emacs' file as you wish.
    Default font: Consolas 11.  The Microsoft Consolas Font Pack is included.
    Default encoding: UTF-8. If you open a file that isn't UTF-8, it can recognized the encoding also.

ABCL 1.0.1:
    Default encoding: UTF-8.
    Full source included.
    Using JRE 7, which included in the package.
    Compiled from official source code distribution.
    JNA included in classpath by default. If you use CFFI, this is required.
    More dlls included in the `dist' directory: libeay32.dll, libssl32.dll, ssleay32.dll. They are required by some Common Lisp libraries.

CLISP 2.49:
    No threads support.
    This is the official binary.

Clozure CL 1.7:
    Include both x86 and x86_64 versions.
    With full source.
    Can update by running `svn update'.
    Some known issues there. For example, you can't using M-x slime to restart it.
    This is the official binary.

ECL 11.1.1:
    Using C compiler by default to make SLIME useful (caused by an ECL's bug). You can change to bytecode compiler anytime.
    Include header files and libraries.
    Compiled from official source code distribution using MinGW compiler (gcc 4.5.0) which included in the package.
    
SBCL 1.0.55.1:
    Use the fork which with threads support.
    This is the official binary.
    With full source (using the official source distribution).
    Include both x86 and x86_64 versions.
    "Use it with your own risk."
    
MinGW: 
    MSYS 1.0 included.
    GCC 4.5.0.

JRE7:
    Official JRE 1.7.0 Update 2 binary from Oracle.
    
HyperSpec:
    A full web version included.
    
******************************************************************************************************
If you have problems with SLIME, delete the `.slime' directory in your home folder (`Documents and Settings\yourname' in XP and `Users\yourname' in Vista) and restart Emacs.
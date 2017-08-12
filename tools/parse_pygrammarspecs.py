# Usage:
#     python parse_pygrammarspecs.py > template.hy

from pyparsing import *
import re

#==============================================================================
# Model definition
#==============================================================================
module = Forward()
body = Forward()
datatypedeclarationwithattr = Forward()
classdeclaration = Forward()
classdeclarationwithattr = Forward()
datatypedeclaration = Forward()
classdefexpression = Forward()
classdef = Forward()
datatypedef = Forward()
argpairs = Forward()
argpair = Forward()
attributeclause = Forward()

w = Word(alphanums + '_*?')
groupname = w.copy()
classname = w.copy()
argtype = w.copy()
argname = w.copy()

attrindicator = "#withattributes"
groupindicator = "#group"
datatypeindicator = "#datatype"
attributetag = Empty().setParseAction(lambda s,l,t: [attrindicator])
grouptag = Empty().setParseAction(lambda s,l,t: [groupindicator])
datatypetag = Empty().setParseAction(lambda s,l,t: [datatypeindicator])

module << (Literal("module").suppress()
           + Literal("Python").suppress()
           + nestedExpr('{', '}', content=body))
body << OneOrMore(classdeclarationwithattr
                  | classdeclaration
                  | datatypedeclarationwithattr
                  | datatypedeclaration)
classdeclarationwithattr << Group(attributetag
                                  + grouptag
                                  + groupname
                                  + Literal("=").suppress()
                                  + classdefexpression
                                  + attributeclause)
classdeclaration << Group(grouptag
                          + groupname
                          + Literal("=").suppress()
                          + classdefexpression)
datatypedeclarationwithattr << Group(attributetag
                                     + datatypetag
                                     + Group(groupname
                                             + Literal("=").suppress()
                                             + datatypedef)
                                     + attributeclause)
datatypedeclaration << Group(datatypetag
                             + Group(groupname
                                     + Literal("=").suppress()
                                     + datatypedef))
classdefexpression << delimitedList(classdef, delim='|')
classdef << (Group(classname + nestedExpr('(', ')', content=argpairs))
             | Group(classname + Group(Empty())))
datatypedef << nestedExpr('(', ')', content=argpairs)
argpairs << delimitedList(argpair, delim=',')
argpair << Group(argtype + argname)
attributeclause << (Literal("attributes").suppress()
                           + nestedExpr('(', ')', content=argpairs))


#==============================================================================
# Parsing
#==============================================================================
with open("./tools/pygrammarspecs.txt", 'r') as f:
    data = f.read()
# Delete comments
data = re.sub("--.*", "", data)
parsedlist = module.parseString(data).asList()[0]

# Print templates for py2hy AST transform declarations
def fprint(fname, fargs):
    keylist = map(lambda l: l[1], fargs)
    docstring = map(lambda l: l[1] + " (" + l[0] + ")"
                              + (" [optional]" if l[0].endswith("?") else "")
                              + (" [list]" if l[0].endswith("*") else ""), fargs)
    if len(fargs) > 0:
        print("(defsyntax " + fname + " [" + " ".join(keylist) + "]")
        print("  \"Args:")
        print("      " + "\n      ".join(docstring) + "\"")
        print("  None)")
    else:
        # Constant expression
        print("(defsyntax " + fname + " []")
        print("  \"Constant expression\" None)")
    print()

print("; Auto-generated template\n")
for decl in parsedlist:
    it = iter(decl)
    varpairs_append = []
    if decl[0] == attrindicator:
        # Popping `decl` affects `it` as well
        varpairs_append = decl.pop()
        next(it)
    typetag = next(it)
    if typetag == groupindicator:
        groupname = next(it)
        print(";==============================================================================")
        print("; Classgroup `" + groupname + "`")
        print(";==============================================================================")
        for syntaxdef in it:
            fname, fargs = syntaxdef
            fprint(fname, fargs + varpairs_append)
    elif typetag == datatypeindicator:
        syntaxdef = next(it)
        fname, fargs = syntaxdef
        print(";==============================================================================")
        print("; Datatype `" + fname + "`")
        print(";==============================================================================")
        fprint(fname, fargs + varpairs_append)
    print()

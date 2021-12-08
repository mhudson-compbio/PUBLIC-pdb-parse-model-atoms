# PUBLIC-pdb-parse-model-atoms

Update Dec 8 2021

Very old version, dumpster fire, hopefully I'll do the full release someday. This is mostly barebones and primitive functions and data structures, basically all the simulation code is currently in the workshop, I'm sort of deprecating this as it's in the process of being merged into my CANDO platform modification which incorporates some additional compoound-proteome simulation pipelines, analytics, and an information theory based framework for enhancing CANDO stuff.

As far as actually parsing and modeling PDB stuff you're probably better off using a lot of the better python based stuff (e.g. anything written by Zack or Will, or use the data structures from the PDB and parse them with the appropriate libraries) -- these old clojure primitives were necessary for the foundation of my CANDO mod, but eventually there will be Clojure, Common Lisp, and Python Implementations of the CANDO MOD (to be titleded) which opperate on common data sources (currently curated SQLite databases -- but subject to change -- and will be accordingly handled by each implementation). 

The source multiple code distributions will be for the convenience of maintaning it (basically everyone is using python -- but executables based on each source are being prototyped, each with their own advantages --- Clojure (leveraging the JVM and its ecosystem and being a great language is the primary dev version), Python (for compatability with existing source code and admittedly some good libraries), and Common Lisp (because its lisp and you can compile it down to be really, really fast machine code -- plus if you're going to not do functional programming and do object oriented programming to play nicely with others -- you might as well use the grandaddy greatest of all time OOP system in CLOS common lisp object system).

It's likely dumb to implement the same thing in three different languages, but I accept that -- and the executables will make them cross platform, like literally anything more advanced than an arduino, but there might be some variation in features and performance -- so I'm also considering a server based solution which draws from all three.

It's a beautiful mess.

--- below is the old readme --

group repo for code for parsing data representations about protein/ligand atoms and modeling/simulating these atoms

incomplete, so far just some of the utility functions

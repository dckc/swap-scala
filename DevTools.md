See also [Fun and Frustration with Scala](http://www.advogato.org/person/connolly/diary/71.html), 18 Jan 2010

Start by grabbing xsbt-launch-0.6.10.jar (an experimental version) from the [sbt downloads](http://code.google.com/p/simple-build-tool/downloads/list). Create a launch script as per [sbt Setup](http://code.google.com/p/simple-build-tool/wiki/Setup).

Audit trail:

```
$ md5sum xsbt-launch-0.6.10.jar 
54ccec7e4c95c2b2ce1440caae7be7f9  xsbt-launch-0.6.10.jar
```

Then you should be able to build and test a la:

```
$ ~/bin/sbt
[info] Building project RDF Semantics 0.1 against Scala 2.8.0.Beta1-RC5
[info]    using TestingProject with sbt 0.6.10 and Scala 2.7.7
> update
...
> test
...
[info] Passed: : Total 50, Failed 0, Errors 0, Passed 49, Skipped 1
[info] All tests PASSED.
[info] Total time: 44 s, completed Jan 18, 2010 10:12:13 PM
```

Yes, it grabs version 2.7.x **and** version 2.8.x of the scala compiler and libraries; sbt runs itself using 2.7, but it's configured to build and runs the project code using 2.8.
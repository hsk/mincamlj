call scalac type.scala id.scala syntax.scala
call scalac typing.scala

call jflex lexer.l
yacc -J -Jextends=mincaml.Syntax -Jpackage=mincaml parser.y
javac -classpath "C:\Program Files\Scala\lib\scala-library.jar;." Parser.java ParserVal.java YyLex.java
call scala Parser a.txt
call scalac kNormal.scala

javac -classpath "C:\Program Files\Scala\lib\scala-library.jar;." test.java

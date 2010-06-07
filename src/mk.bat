call scalac Type.scala
call scalac Id.scala
call scalac Syntax.scala
call jflex lexer.l
yacc -J -Jextends=Syntax parser.y
javac -classpath "C:\Program Files\Scala\lib\scala-library.jar;." Parser.java
call scala Parser a.txt

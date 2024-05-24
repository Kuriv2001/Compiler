package alpine.transpiler

import alpine._
import sys.process._
import scala.io.Source
import java.nio.file.{ Files, Path }
import java.util.Comparator
import java.io.File


object TranspilerUtils:
   /**
    * Represents a test case for the transpiler
    *
    * @param name The name of the test, with the number of points as (Xpts) or (Xpt)
    * @param input The lines of the input file, i.e., the alpine code
    * @param expected The expected lines of the std out when running the transpiled program
    */
  case class TranspilerTest_to_C(name: String, input: List[String], expected: List[String])

  /**
    * Parses a file with the correct format and produces a list of TranspilerTest instances
    *
    * @param lines
    * @return
    */
  def parseTests(lines: List[String]): List[TranspilerTest_to_C] = 
    val indicesBeginningTests = lines.zipWithIndex.filter(p => p._1.startsWith("//BEGIN")).map(_._2)
    val indicesEndTests = lines.zipWithIndex.filter(p => p._1.startsWith("//END")).map(_._2 + 1)
    val boundsOfTests = indicesBeginningTests.zip(indicesEndTests)
    val testLists = boundsOfTests.map(p => lines.slice(p._1, p._2))
    testLists.map(l =>
      val name = l.head.replace("//BEGIN ", "")
      val codeEndIndex = l.tail.indexWhere(s => s.startsWith("//OUT"))
      val code = l.tail.slice(0, codeEndIndex).filter(!_.isEmpty())
      val out = l.slice(l.indexWhere(_.startsWith("//OUT")), l.indexWhere(_.startsWith("//END"))).filter(!_.contains("//OUT"))
      // println(f"name = '$name'")
      // println(f"code = $code")
      // println(f"out = $out")
      TranspilerTest_to_C(name = name, input = code, expected = out)
    )

  

  /** Util class that spawns a temporary directory to run Scala files */
  class Runner:
    case class ScalaRunError(message: String) extends Exception(message):
      override def toString: String = f"Failed to run Scala: \n$message"
    case class ScalacCompileError(file: String, message: String) extends Exception(message):
      override def toString: String = f"Failed to compile the Scala file $file: \n$message"
    case class BackendError(exception: Throwable) extends Exception(exception):
      override def toString: String = f"Error from Alpine: \n$exception"

    val tmpDir = Files.createTempDirectory("transpilerC")

    private val gcc = "gcc"
    private val scalac = if System.getProperty("os.name").startsWith("Windows") then "where.exe scalac".!! else "scalac"
    private val scala = if System.getProperty("os.name").startsWith("Windows") then "where.exe scala".!! else "scala"

    /** Executes the cmd and returns (errorCode, output) */
    private def spawn(cmd: String, cwd: Option[File] = Some(tmpDir.toFile), ignoreStderr: Boolean = false): (Int, String, String) =
      val output = new StringBuilder
      val stderr = new StringBuilder
      val logger = ProcessLogger(x => output.append(f"$x\n"), x => stderr.append(f"$x\n")) // Ignoring stderr
      val errorCode = Process(cmd, Some(tmpDir.toFile)).!(logger)
      (errorCode, output.toString, stderr.toString)

    /** Runs the given Scala file in the temporary directory */
    def run(input: Path, libPaths: List[String]): Either[ScalaRunError, String] =
      val libPathO = libPaths.map(filename => tmpDir.resolve(appendOExtension(filename))).mkString(" ")
      val absolutePaths = input.toAbsolutePath()
      val absolutePathsOutput = input.resolveSibling(input.getFileName.toString.stripSuffix(".c") + ".o")
      val absolutePathsOutputExecutable = input.resolveSibling(input.getFileName.toString.stripSuffix(".c"))
      val parent = absolutePaths.getParent()
      //val options = f"-classpath ${parent.toString}"

      spawn(f"$gcc -c $absolutePaths -o $absolutePathsOutput", ignoreStderr = true)
      // print("second:" + f"$gcc -c $absolutePaths -o $absolutePathsOutput \n")

      spawn(f"$gcc $absolutePathsOutput $libPathO  -o $absolutePathsOutputExecutable", ignoreStderr = true)
      // print("third:" + f"$gcc $absolutePathsOutput $libPathO  -o $absolutePathsOutputExecutable \n")

      // print("fourth:" + f".$absolutePathsOutputExecutable \n")
      val (exitCode, output, stderr) = spawn(f"$absolutePathsOutputExecutable", ignoreStderr = true)  
     
      // 255 (-1) is reserved for panic
      if exitCode == 0 || exitCode == 255 then Right(output)
      else Left(ScalaRunError(stderr ++ "\n-- stderr --\n" ++ output))


    /**
    * Run the alpine transpiler, returning the Path to the generated .scala file
    *
    * @param inputFile
    * @return
    */
    def runAlpineTranspiler(inputFile: Path): Either[BackendError, Path] =
      try
        val source = SourceFile.withContentsOfFile(inputFile.toAbsolutePath.toString).get
        val parsed = parsing.Parser(source).program()
        val typed = { val typer = typing.Typer(); typer.check(parsed) }
        val transpiled = codegen.CPrinter(typed).transpile()
        println("From test: " + inputFile)
        println("File transpiled to C: " + transpiled)
        Right(writeCFile("main", transpiled))
      catch (e: Throwable) =>
        Left(BackendError(e))

    /** Compiles the given Scala files in the temporary directory */
    def compileLibrary(inputs: List[String]): Unit =
      val absolutePaths = inputs.map(filename => tmpDir.resolve(appendCExtension(filename))).mkString(" ")
      val absolutePathsOutput = inputs.map(filename => tmpDir.resolve(appendOExtension(filename))).mkString(" ")
      // print(f"first: $gcc -c $absolutePaths -o $absolutePathsOutput \n")
      spawn(f"$gcc -c $absolutePaths -o $absolutePathsOutput", Some(tmpDir.toFile)) match
        case (0, _, _) => ()
        case (_, stdout, stderr) => throw ScalacCompileError("c_rt", stderr ++ "\n-- stderr --\n" ++ stdout)

    /** Writes a Scala file in the temporary directory. Prepends the .scala if needed extension */
    def writeCFile(name: String, content: String): Path =
      val file = tmpDir.resolve(appendCExtension(name))
      Files.write(file, content.getBytes)
      file

    def writeHFile(name: String, content: String): Path =
      val file = tmpDir.resolve(appendHExtension(name))
      Files.write(file, content.getBytes)
      file  

    /** Writes an Alpine file in the temporary directory. Prepends the .al if needed extension */
    def writeAlpineFile(name: String, content: String): Path =
      val file = tmpDir.resolve(appendAlpineExtension(name))
      Files.write(file, content.getBytes)
      file

    /** Deletes the temporary directory */
    def delete: Unit =
      Files.walk(tmpDir)
        .sorted(Comparator.reverseOrder())
        .map(_.toFile)
        .forEach(_.delete)

    private def appendScalaExtension(filename: String): String =
      if filename.endsWith(".scala") then filename else f"$filename.scala"

    private def appendAlpineExtension(filename: String): String =
      if filename.endsWith(".al") then filename else f"$filename.al"

    private def appendCExtension(filename: String): String =
      if filename.endsWith(".c") then filename else f"$filename.c"  

    private def appendOExtension(filename: String): String =
      if filename.endsWith(".o") then filename else f"$filename.o" 

    private def appendHExtension(filename: String): String =
      if filename.endsWith(".h") then filename else f"$filename.h"   

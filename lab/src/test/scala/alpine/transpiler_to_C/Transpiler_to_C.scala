package alpine.transpiler

import scala.io.Source

class TranspilerTests_to_C extends munit.FunSuite:
  private val lineSeparator = System.lineSeparator()

  var runner: Option[TranspilerUtils.Runner] = None
  
  val inputFileAlpineTests = "./src/test/res/transpiler/test_cases.al"
  
  // Uncomment this line to run the codegen tests with the transpiler
  // val inputFileAlpineTests2 = "./src/test/res/codegen/test_cases.al"

  /**
    * Parses the given file and run the obtained test cases
    *
    * @param filename
    * @param loc
    */
  def runTestsFromFile(filename: String)(implicit loc: munit.Location): Unit = {
    val lines: List[String] = Source.fromFile(filename).getLines().toList
    val tests = TranspilerUtils.parseTests(lines)
    for t <- tests do
      test(t.name) {
        val r = runner.get
        val alpineTestFilename = "Input.al"
        val inputAlpineFilePath = r.writeAlpineFile(alpineTestFilename, t.input.mkString(lineSeparator))
        val outputScalaFile = r.runAlpineTranspiler(inputAlpineFilePath)
        val outputOfScala = outputScalaFile.flatMap(outputScalaFile => r.run(outputScalaFile, List("lib")).map(_.replace("\r\n", "\n")))
        outputOfScala match {
          case Right(output) =>
            val expected = t.expected.mkString(lineSeparator)
            assertEquals(output.stripSuffix("\n"), expected)
          case Left(error) =>
            throw error
        }
      }
  }

  override def beforeAll(): Unit =
    runner = Some(TranspilerUtils.Runner())
    // Setup-ing the standard library should be done here
    runner.get.writeHFile("lib", scala.io.Source.fromFile("./c_rt/rt.h").mkString)
    runner.get.writeCFile("lib", scala.io.Source.fromFile("./c_rt/rt.c").mkString)
    runner.get.compileLibrary(List("lib"))

  override def afterAll(): Unit =
    runner.foreach(_.delete)

  runTestsFromFile(inputFileAlpineTests)
  
  // Uncomment this line to run the codegen tests with the transpiler
  // runTestsFromFile(inputFileAlpineTests2)

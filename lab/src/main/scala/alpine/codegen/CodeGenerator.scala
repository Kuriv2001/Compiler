package alpine
package codegen

import alpine.symbols
import alpine.wasm.WasmTree._
import alpine.ast._
import alpine.wasm.Wasm
import scala.collection.mutable
import alpine.parsing.Token.Kind
import scala.collection.mutable.ListBuffer

/** The transpilation of an Alpine program to Scala. */
final class CodeGenerator(syntax: TypedProgram) extends ast.TreeVisitor[CodeGenerator.Context, Unit]:
  import CodeGenerator._

  /** The program being evaluated. */
  private given TypedProgram = syntax

  /** Returns a WebAssembly program equivalent to `syntax`. */
  /** THIS IS AN EXAMPLE MODULE! */

  def compile(): Module = 
    given a: Context = Context()
    syntax.declarations.foreach(_.visit(this))
    println("")
    println(a.functions.foldLeft("")((acc, b) => acc ++ b.mkString)) // for debugging purposes

    Module(
    List(
      ImportFromModule("api", "print", "print", List(I32), None),
      ImportFromModule("api", "print", "fprint", List(F32), None),
      ImportFromModule("api", "print-char", "print-char", List(I32), None),
      ImportFromModule("api", "show-memory", "show-memory", List(I32), None),
      ImportMemory("api", "mem", 100)
    ),
    a.functions.map(x => x.asInstanceOf[wasm.WasmTree.Function]).toList.reverse) // Careful main should be last, is maybe first here!! 
    // List(
    //   FunctionDefinition("heap-test", body =
    //     List(
    //       IConst(0),
    //       IConst(0xdeadbeef),
    //       IStore,
    //       IConst(0),
    //       Call("show-memory")
    //     )
    //   ),
    //   FunctionDefinition("local-test", locals = List(F32, F32), returnType = Some(F32), body =
    //     List(
    //       FConst(3.14),
    //       LocalSet(0),
    //       FConst(1.67),
    //       LocalSet(1),
    //       LocalGet(0),
    //       LocalGet(1),
    //       FSub
    //     )
    //   ),
    // MainFunction(
    //   List(
    //     IConst(1),
    //     IConst(2),
    //     IAdd,
    //     Call("print"),
    //     Call("heap-test"),
    //     Call("local-test"),
    //     Call("fprint"),
    //     IConst(0x41),
    //     Call("print-char"),

    //     FConst(42) // Return
    //   ),
    //   Some(F32)
    // )
    
  // Tree visitor methods

  /** Visits `n` with state `a`. */
  def visitLabeled[T <: Tree](n: Labeled[T])(using a: Context): Unit = 
    n.value.visit(this)

  /** Visits `n` with state `a`. */
  def visitBinding(n: Binding)(using a: Context): Unit = 
    if a.isTopLevel then
      if n.identifier == "main" then
        //Get what instructions have been run so far and store them in a MainFunction
        n.initializer.get.visit(this)
        a.functions.append(MainFunction(a.runningInstructions.toList, None))
      else 
        n.initializer match
          case Some(initializer) => 
            initializer match
              case Application(f, args, site) =>

              case Record(ident, fields, site) =>

              case _ => //Variable/Literal
                //If it is a global variable we need to store it in the memory
                a.runningInstructions.append(IConst(a.storedGlobals.length))
                initializer.visit(this)
                a.runningInstructions.append(IStore)
                a.storedGlobals.append(n.identifier)
          case None => 
            a.functions.append(Unreachable) 
    else
      n.initializer match
          case Some(initializer) => 
            initializer match
              case Application(f, args, site) =>

              case Record(ident, fields, site) =>

              case _ => //Variable/Literal
                initializer.visit(this)
                a.runningInstructions.append(LocalSet(a.storedLocals.length))
                a.storedLocals.append(n.identifier)
                a.storedLocalsTypes.append(I32) //TODO actual typees
          case None => 
            a.functions.append(Unreachable)

  /** Visits `n` with state `a`. */
  def visitTypeDeclaration(n: TypeDeclaration)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitMethod(n: Method)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitFunction(n: ast.Function)(using a: Context): Unit = 
    //Process input parameters
    n.inputs.foreach(_.visit(this))
    val inputTypes = a.functionCreateTypes.toList
    val outputType = n.output match
      case Some(output) =>
        output.visit(this)
        Some(a.functionCreateTypes.last)
      case None => 
        None
    a.functionCreateTypes.clear()
    //Simple function start
    n.body.visit(this)
    a.functions.append(
      FunctionDefinition(name = n.identifier, 
                          params = inputTypes, 
                          locals = List(), //TOdo wtf how to get
                          returnType = outputType, 
                          body = a.runningInstructions.toList)) 
    a.runningInstructions.clear()                      

  
  /** Visits `n` with state `a`. */
  def visitParameter(n: Parameter)(using a: Context): Unit = 
    // n.label match
    //   case Some(label) => 
    //     a.functionCreateLabels.append(label)
    //   case None => 
    //     a.functionCreateTypes.append(None)
    n.ascription match
      case Some(ascription) => 
        ascription.visit(this)
      case None => 
        a.functionCreateTypes.append(I32) //TODO

  /** Visits `n` with state `a`. */
  def visitIdentifier(n: Identifier)(using a: Context): Unit =  
    if a.isTopLevel then
      if a.storedGlobals.contains(n.value) then
        a.runningInstructions.append(IConst(a.storedGlobals.indexOf(n.value)))
        a.runningInstructions.append(ILoad)
      else
        n.value match
        case "print" => 
          a.runningInstructions.last match 
          case IConst(_) => 
            a.runningInstructions.append(Call("print"))
          case FConst(_) => 
            a.runningInstructions.append(Call("fprint"))  
          case _ =>
            a.runningInstructions.append(Call(n.value))
        case _ =>
          a.runningInstructions.append(Call(n.value)) 
    else
      if a.storedLocals.contains(n.value) then
        a.runningInstructions.append(LocalGet(a.storedLocals.indexOf(n.value)))
      else
        n.value match
          case "print" => 
            a.runningInstructions.last match 
            case IConst(_) => 
              a.runningInstructions.append(Call("print"))
            case FConst(_) => 
              a.runningInstructions.append(Call("fprint"))  
            case _ =>
              a.runningInstructions.append(Call(n.value))
          case _ =>
            a.runningInstructions.append(Call(n.value))    

  /** Visits `n` with state `a`. */
  def visitBooleanLiteral(n: BooleanLiteral)(using a: Context): Unit = 
    val boolAsString = if n.value == "true" then "1" else "0"
    visitIntegerLiteral(IntegerLiteral(boolAsString, n.site))

  /** Visits `n` with state `a`. */
  def visitIntegerLiteral(n: IntegerLiteral)(using a: Context): Unit = 
    a.runningInstructions.append(IConst(n.value.toInt))

  /** Visits `n` with state `a`. */
  def visitFloatLiteral(n: FloatLiteral)(using a: Context): Unit = 
    a.runningInstructions.append(FConst(n.value.toFloat))

  /** Visits `n` with state `a`. */
  def visitStringLiteral(n: StringLiteral)(using a: Context): Unit = ???
    // val bytes = n.value.getBytes("UTF-8")
    // val allocateMemory = List(IConst(bytes.length), Call("allocate"))
    // a.output.append(allocateMemory.mkString)

  /** Visits `n` with state `a`. */
  def visitRecord(n: Record)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitSelection(n: Selection)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitApplication(n: Application)(using a: Context): Unit = 
    n.arguments.foreach(_.visit(this))
    n.function.visit(this)
    // Store arguments in the stack then call the function
    
  /** Visits `n` with state `a`. */
  def visitPrefixApplication(n: PrefixApplication)(using a: Context): Unit = 
    a.runningInstructions.append(Call(n.function.value))
    n.argument.visit(this)

  /** Visits `n` with state `a`. */
  def visitInfixApplication(n: InfixApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitConditional(n: Conditional)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitMatch(n: Match)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitMatchCase(n: Match.Case)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitLet(n: Let)(using a: Context): Unit = //WHAAAT?
    n.body.visit(this)
    n.binding.visit(this)
    // let x = 4 -> variable
    // let main = print(42) -> function
    // We can implement it as a function call, if we have a variable we can store it in the stack

  /** Visits `n` with state `a`. */
  def visitLambda(n: Lambda)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedExpression(n: ParenthesizedExpression)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitAscribedExpression(n: AscribedExpression)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeIdentifier(n: TypeIdentifier)(using a: Context): Unit = 
    n.value match
      case "Int" => a.functionCreateTypes.append(I32)
      case "Float" => a.functionCreateTypes.append(F32)
      case _ => a.functionCreateTypes.append(I32) //TODO edit
  /** Visits `n` with state `a`. */
  def visitRecordType(n: RecordType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeApplication(n: TypeApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitArrow(n: Arrow)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitSum(n: Sum)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedType(n: ParenthesizedType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitValuePattern(n: ValuePattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecordPattern(n: RecordPattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitWildcard(n: Wildcard)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitError(n: ErrorTree)(using a: Context): Unit = ???

object CodeGenerator:

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    /** The (partial) result of the transpilation. */
    var functions: ListBuffer[alpine.wasm.WasmTree.DepthFormattable] = ListBuffer()

    var runningInstructions = ListBuffer[Instruction]()

    var storedLocals = ListBuffer[String]()
    var storedLocalsTypes = ListBuffer[alpine.wasm.WasmTree.WasmType]()

    var storedGlobals = ListBuffer[String]()

    var functionCreateTypes = ListBuffer[alpine.wasm.WasmTree.WasmType]()

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

    /** Returns `action` applied on `this` where `output` has been exchanged with `o`. */
    def swappingOutputBuffer[R](o: StringBuilder)(action: Context => R): R =
      val old = _output
      _output = o
      try action(this) finally _output = old

    /** Returns `action` applied on `this` where `isTopLevel` is `false`. */
    def inScope[R](action: Context => R): R =
      var tl = _isTopLevel
      _isTopLevel = false
      try action(this) finally _isTopLevel = tl

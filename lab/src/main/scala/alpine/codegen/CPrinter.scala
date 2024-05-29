package alpine
package codegen

import alpine.ast
import alpine.symbols
import alpine.symbols.Entity.builtinModule
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import alpine.symbols.Type
import alpine.symbols.Type.Bool
import scala.compiletime.ops.double
import scala.quoted.Expr
import alpine.ast.Expression
import alpine.ast.Typecast

/** The transpilation of an Alpine program to Scala. */
final class CPrinter(syntax: TypedProgram) extends ast.TreeVisitor[CPrinter.Context, Unit]:

  import CPrinter.Context

  /** The program being evaluated. */
  private given TypedProgram = syntax

  /** Returns a Scala program equivalent to `syntax`. */
  def transpile(): String =
    given c: Context = Context()
    c.output ++= "\nint main(int argc, char *argv[]) {\n"
    syntax.declarations.foreach(_.visit(this))
    c.recordsToFree.foreach(freeRecord)
    c.output ++= "}\n\n"
    c.functionsToEmit.foreach(emitFunctionDefinition)
    val imports = "#include <stdio.h>\n#include <stdlib.h>\n#include \"lib.h\"\n\n"
    val functionDefs = c.functionsToEmit.map(emitFunctionDeclaration).mkString("\n")
    imports + functionDefs + c.typesToEmit.map(emitRecord).mkString("\n") + c.output.toString


  /** creates function declaration */
  private def emitFunctionDeclaration(t: alpine.ast.Function)(using context: Context): String =
    val sb = new StringBuilder()
    sb ++= "ArtVariant "
    sb ++= transpiledReferenceTo(t.entityDeclared)
    sb ++= "("
    var idxFields = 0
    for field <- t.inputs do
      sb ++= "ArtVariant "
      sb ++= " "
      sb ++= field.identifier
      if idxFields < t.inputs.size - 1 then
        sb ++= ", "
      idxFields += 1
    sb ++= ");\n"
    sb.toString()
    
  /** creates function init for the record */  
  private def emitFunctionDefinition(t: alpine.ast.Function)(using context: Context): Unit =
    context.output ++= "  " * context.indentation
    context.output ++= transpiledType(symbols.Type.Arrow.from(t.tpe).get.output) //tu es la
    context.output ++= " "
    context.output ++= transpiledReferenceTo(t.entityDeclared)
    context.output ++= "("
    context.output.appendCommaSeparated(t.inputs) { (o, a) =>
        o ++= transpiledType(a.tpe)
        o ++= " "
        o ++= a.identifier
    }
    context.output ++= ") {\n"
 
    context.indentation += 1
    
    context.output ++= "  " * context.indentation
    context.output ++= "return  "
    context.inScope((c) => t.body.visit(this)(using c))  
    context.output ++= ";"
    
    context.indentation -= 1
    context.output ++= "\n"
    context.output ++= "  " * context.indentation
    context.output ++= "}\n\n"

  /** frees a record */
  private def freeRecord(name: String)(using context: Context): Unit =
    context.output ++= "free_record(&"
    context.output ++= name
    context.output ++= ");\n"

  /** Writes the C declaration of `t` to the output buffer. */
  private def emitRecord(t: symbols.Type.Record)(using context: Context): String =
    val sb = new StringBuilder
    sb ++= s"ArtVariant create_record_${discriminator(t)}("
    //Parameters: ArtVariant create_record_discrimRecord(ArtVariant field1, ArtVariant field2, ...)
    var idxFields = 0
    for field <- t.fields do
      sb ++= "ArtVariant "
      sb ++= " "
      sb ++= field.label.getOrElse("no_label" + idxFields)
      if idxFields < t.fields.size - 1 then
        sb ++= ", "
      idxFields += 1
    sb ++= ")"
    //Function body: 
    //{ArtVariant tempRecord; 
    //init_record(&tempRecord, "discrimRecord", 2); 
    //add_field_to_record(&tempRecord, 0, field1); add_field_to_record(&tempRecord, 1, field2); 
    //return tempRecord;}
    sb ++= "{\n"
    context.indentation += 2
    sb ++= "  " * context.indentation
    sb ++= s"ArtVariant tempRecord;\n"
    sb ++= "  " * context.indentation
    sb ++= s"init_record(&tempRecord, \"${discriminator(t)}\", ${t.fields.size});\n"
    idxFields = 0
    for field <- t.fields do
      sb ++= "  " * context.indentation
      sb ++= s"add_field_to_record(&tempRecord, ${idxFields}, "
      sb ++= field.label.getOrElse("no_label" + idxFields)
      sb ++= ");\n"
      idxFields += 1
    sb ++= "  " * context.indentation
    sb ++= "return tempRecord;\n"
    sb ++= "}\n\n"
    context.indentation -= 2  
    sb.toString()

  /** Returns the transpiled form of `t`. */
  private def transpiledType(t: symbols.Type)(using context: Context): String =
    t match
      case u: symbols.Type.Builtin =>
        transpiledBuiltin(u)
      case u: symbols.Type.Record =>
        transpiledRecord(u)
      case u: symbols.Type.Arrow =>
        transpiledArrow(u)
      case u: symbols.Type.Sum =>
        transpiledSum(u)
      case _ => throw Error(s"type '${t}' is not representable in Scala")

  /** Returns the transpiled form of `t`. */ //TODO done
  private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
    t match
      case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in Scala")
      case symbols.Type.Bool => "ArtVariant" //potential short
      case symbols.Type.Int => "ArtVariant"
      case symbols.Type.Float => "ArtVariant"
      case symbols.Type.String => "ArtVariant"
      case symbols.Type.Any => "ArtVariant"//TODO pas sur

  /** Returns the transpiled form of `t`. */
  private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String =
    if t == symbols.Type.Unit then
      "Unit"
    else
      context.registerUse(t)
      val d = discriminator(t)
      if t.fields.isEmpty then s"${d}.type" else d

  /** Returns the transpiled form of `t`. */
  private def transpiledArrow(t: symbols.Type.Arrow)(using context: Context): String =
    val r = StringBuilder()
    r ++= "("
    r.appendCommaSeparated(t.inputs) { (o, a) => o ++= transpiledType(a.value) }
    r ++= " => "
    r ++= transpiledType(t.output)
    r ++= ")"
    r.toString()

  /** Returns the transpiled form of `t`. */
  private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String =
    if t.members.isEmpty then "N" else
      t.members.map(transpiledType).mkString(" | ")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type): String =
    t match
      case u: symbols.Type.Builtin =>
        discriminator(u)
      case u: symbols.Type.Meta =>
        s"M${discriminator(u.instance)}"
      case u: symbols.Type.Definition =>
        "D" + u.identifier
      case u: symbols.Type.Record =>
        discriminator(u)
      case u: symbols.Type.Arrow =>
        discriminator(u)
      case u: symbols.Type.Sum =>
        discriminator(u)
      case _ =>
        throw Error(s"unexpected type '${t}'")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Builtin): String =
    t match
      case symbols.Type.BuiltinModule => "Z"
      case symbols.Type.Bool => "B"
      case symbols.Type.Int => "I"
      case symbols.Type.Float => "F"
      case symbols.Type.String => "S"
      case symbols.Type.Any => "A"

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Record): String =
    val b = StringBuilder("R")
    b ++= t.identifier.drop(1)
    for f <- t.fields do
      b ++= f.label.getOrElse("")
      b ++= discriminator(f.value)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Arrow): String =
    val b = StringBuilder("X")
    for i <- t.inputs do
      b ++= i.label.getOrElse("")
      b ++= discriminator(i.value)
    b ++= discriminator(t.output)
    b.toString

    //TODO done
  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Sum): String =
    if t.members.isEmpty then "N" else
      "E" + t.members.map(discriminator).mkString

  /** Returns a transpiled reference to `e`. */
  private def transpiledReferenceTo(e: symbols.Entity): String =
    e match
      case symbols.Entity.Builtin(n, _) => s"art_${n.identifier}"
      case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
      case _: symbols.Entity.Field => ???

  /** Returns a string representation of `n` suitable for use as a Scala identifier. */
  private def scalaized(n: symbols.Name): String =
    n.qualification match
      case Some(q) =>
        s"${scalaized(q)}_${n.identifier}"
      case None =>
        "_" + n.identifier
        //"_" + n.identifier

  override def visitLabeled[T <: ast.Tree](n: ast.Labeled[T])(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitBinding(n: ast.Binding)(using context: Context): Unit =
    // Bindings represent global symbols at top-level.
    if context.isTopLevel then
      context.output ++= "  " * context.indentation

      // If the is the entry point if it's called "main".
      if n.identifier == "main" then
        context.output ++= "\n//Start of main:\n"
        // context.output ++= "int main(int argc, char *argv[]) {\n"//TODO do what?
        // context.indentation += 1
      else
        
        //context.output ++= transpiledType(n.tpe)
        context.output ++= "ArtVariant "
        context.output ++= " "
        
        //if is record
        if n.tpe.isInstanceOf[Type.Record] then
          context.output ++= ""
          context.registerFree(transpiledReferenceTo(n.entityDeclared))
          
        context.output ++= transpiledReferenceTo(n.entityDeclared)
        
        // Top-level bindings must have an initializer.
        assert(n.initializer.isDefined)
        context.indentation += 1
        context.output ++= " = "

      context.output ++= "  " * context.indentation
      context.inScope((c) => n.initializer.get.visit(this)(using c))
      context.output ++= ";\n\n"
      context.indentation -= 1

    // Bindings at local-scope are used in let-bindings and pattern cases.
    else
      // context.output ++= s"val "
      context.output ++= transpiledReferenceTo(n.entityDeclared)
      context.output ++= ": "
      context.output ++= transpiledType(n.tpe)
      n.initializer.map { (i) =>
        context.output ++= " = "
        context.inScope((c) => i.visit(this)(using c))
      }

  override def visitTypeDeclaration(n: ast.TypeDeclaration)(using context: Context): Unit =
    unexpectedVisit(n)

    //TODO done
  override def visitFunction(n: ast.Function)(using context: Context): Unit =
    context.output ++= ""
    context.registerFunction(n)

  override def visitParameter(n: ast.Parameter)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitIdentifier(n: ast.Identifier)(using context: Context): Unit =
    context.output ++= transpiledReferenceTo(n.referredEntity.get.entity)

  override def visitBooleanLiteral(n: ast.BooleanLiteral)(using context: Context): Unit =
    context.output ++= s"(ArtVariant){.type = BOOL, .value.b = $n.value, .label = \"\", .num_fields = 1}"

  override def visitIntegerLiteral(n: ast.IntegerLiteral)(using context: Context): Unit =
    context.output ++= s"(ArtVariant){.type = INT, .value.i = ${n.value}, .label = \"\", .num_fields = 1}"

  override def visitFloatLiteral(n: ast.FloatLiteral)(using context: Context): Unit =
    context.output ++= s"(ArtVariant){.type = FLOAT, .value.f = ${n.value}f, .label = \"\", .num_fields = 1}"

  override def visitStringLiteral(n: ast.StringLiteral)(using context: Context): Unit =
    context.output ++= s"(ArtVariant){.type = STRING, .value.s = ${n.value}, .label = \"\", .num_fields = 1}"

  override def visitRecord(n: ast.Record)(using context: Context): Unit =

    //New way of how to do it
    //Add the generic function definition
    context.registerUse(n.tpe.asInstanceOf[Type.Record])
    context.output ++= s"create_record_${discriminator(n.tpe)}("
    var idxFields = 0
    for field <- n.fields do
      field.value.visit(this)
      if idxFields < n.fields.size - 1 then
        context.output ++= ", "
      idxFields += 1
    context.output ++= ")"


    // // Have a function that would intialize the record
    // context.output ++= s"create_record_${discriminator(n.tpe)}(){\n"
    // context.recordsToCreate += discriminator(n.tpe)
    // context.recordsToFree += discriminator(n.tpe)

    // context.indentation += 1

    // //Init record
    // context.output ++= "  " * context.indentation
    // context.output ++= "ArtVariant tempRecord;\n"
    // context.output ++= "  " * context.indentation
    // context.output ++= s"init_record(&tempRecord, \"${discriminator(n.tpe)}\", ${n.fields.size});\n"

    // //Add fields to record
    // var idxFields = 0
    // for field <- n.fields do
    //   context.output ++= "  " * context.indentation
    //   context.output ++= s"add_field_to_record(&tempRecord, ${idxFields}, "
    //   field.value.visit(this)
    //   context.output ++= ");\n"
    //   idxFields += 1

    // //Return the record
    // context.output ++= "  " * context.indentation
    // context.output ++= "return tempRecord;\n"
    // context.indentation -= 1
    // context.output ++= "}"

  override def visitSelection(n: ast.Selection)(using context: Context): Unit =
    //n.qualification.visit(this)
    n.qualification.visit(this)
    n.referredEntity match
      // case Some(symbols.EntityReference(e: symbols.Entity.Record, _)) => TODO
      //   context.output ++= "->fields["
      //   context.output ++= e.index
      //   context.output ++= "]"
      case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
        context.output ++= ".value.recordFields[" + e.index + "]"
      case _ =>
        unexpectedVisit(n.selectee)

        //TODO done
  override def visitApplication(n: ast.Application)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
    context.output ++= ")"

    //TODO done
  override def visitPrefixApplication(n: ast.PrefixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.argument.visit(this)
    context.output ++= ")"

    //TODO done
  override def visitInfixApplication(n: ast.InfixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.lhs.visit(this)
    context.output ++= ", "
    n.rhs.visit(this)
    context.output ++= ")"

    //TODO done
  override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
    context.output ++= "if ("
    n.condition.visit(this)
    context.output ++= ") {\n"
    n.successCase.visit(this)
    context.output ++= "\n} else {\n"
    n.failureCase.visit(this)
    context.output ++= "\n}\n\n"

  override def visitMatch(n: ast.Match)(using context: Context): Unit =
    // context.output ++= "switch ("
    // n.scrutinee.visit(this)
    // context.output ++= ") {\n"
    // context.indentation += 1
    val cases = n.cases

    for c <- cases do
      context.output ++= "  " * context.indentation
      context.output ++= "if (art_compare("
      n.scrutinee.visit(this)
      context.output ++= ", "
      c.visit(this)

    context.output ++= "  " * context.indentation
    context.output ++= "{art_panic();}\n"

    // Default case is optional, we could implement it later here.

    // context.indentation -= 1
    // context.output ++= "}\n"

  override def visitMatchCase(n: ast.Match.Case)(using context: Context): Unit =
    // context.output ++= "case "

    // We might need to delete the val from the binding like we did in ScalaPrinter.scala
    n.pattern.visit(this)
    context.output ++= ")) {"
    context.output ++= "\n"
    context.indentation += 1
    context.output ++= "  " * context.indentation
    n.body.visit(this)
    context.output ++= "\n"
    context.output ++= "}\nelse "
    context.indentation -= 1

  override def visitLet(n: ast.Let)(using context: Context): Unit =
    // Use a block to uphold lexical scoping.
    context.output ++= "{\n"
    context.indentation += 1
    context.output ++= "  " * context.indentation
    n.binding.visit(this)
    context.output ++= "\n"
    context.output ++= "  " * context.indentation
    n.body.visit(this)
    context.output ++= "\n"
    context.indentation -= 1
    context.output ++= "  " * context.indentation
    context.output ++= "}"

  override def visitLambda(n: ast.Lambda)(using context: Context): Unit =
    context.output ++= "("
    context.output.appendCommaSeparated(n.inputs) { (o, a) =>
      o ++= a.identifier
      o ++= ": "
      o ++= transpiledType(a.tpe)
    }
    context.output ++= ") => ("
    n.body.visit(this)
    context.output ++= "): "
    context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output)

  override def visitParenthesizedExpression(
      n: ast.ParenthesizedExpression
  )(using context: Context): Unit =
    context.output ++= "("
    n.inner.visit(this)
    context.output ++= ")"

    //TODO Problem, have no types haha, just ArtVariant
  override def visitAscribedExpression(
      n: ast.AscribedExpression
  )(using context: Context): Unit =
    val castType = transpiledType(n.tpe)
    n.operation match
      case Typecast.NarrowUnconditionally =>
        n.inner.visit(this)
        // context.output ++= s"art_narrowUnconditionally(${castType}, "
        // n.inner.visit(this)
        // context.output ++= ")"
      case Typecast.Narrow =>
        n.inner.visit(this)
        // context.output ++= s"art_narrow(${castType}, "
        // n.inner.visit(this)
        // context.output ++= ")"
      case Typecast.Widen =>
        n.inner.visit(this)

  override def visitTypeIdentifier(n: ast.TypeIdentifier)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitRecordType(n: ast.RecordType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitTypeApplication(n: ast.TypeApplication)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitArrow(n: ast.Arrow)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitSum(n: ast.Sum)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitParenthesizedType(n: ast.ParenthesizedType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitValuePattern(n: ast.ValuePattern)(using context: Context): Unit =
    n.value.visit(this)

    //TODO lots of fixing todo
  override def visitRecordPattern(n: ast.RecordPattern)(using context: Context): Unit =
    context.registerUse(n.tpe.asInstanceOf[Type.Record])
    context.output ++= transpiledType(n.tpe.asInstanceOf[Type.Record])

    if(!n.fields.isEmpty) {
      context.output.appendCommaSeparated(n.fields)((output, f) =>
        
        val before = context.output.lastIndexOf("val")
        f.value.visit(this)
        val after = context.output.lastIndexOf("val")

        if (before != after) then context.output.delete(after, after+4))     
    }
    context.output ++= ")"

  override def visitWildcard(n: ast.Wildcard)(using context: Context): Unit =
    context.output ++= "(Artvariant) {.label = \"\", .num_fields = 0, .type = WILDCARD, .value = { .i = 0 }})"
  

  override def visitError(n: ast.ErrorTree)(using context: Context): Unit =
    unexpectedVisit(n)

object CPrinter:

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    /** The functions that must be emitted in the program.  Mutable because we need to add functions to it. */
    private var _functionsToEmit = mutable.Set[alpine.ast.Function]()

    /** The functions that must be emitted in the program. */
    def functionsToEmit: Set[alpine.ast.Function] = _functionsToEmit.toSet

    /** The records that must be freed in the program. */
    private var _recordsToFree = mutable.Set[String]()

    /** The records that must be freed in the program. */
    def recordsToFree: Set[String] = _recordsToFree.toSet

    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** add function to the set of functions that are used by the transpiled program. */
    def registerFunction(f: ast.Function): Unit =
      _functionsToEmit.add(f)

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

    /** Register record to free */
    def registerFree(t: String): Unit =
      _recordsToFree.add(t)

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

  end Context

end CPrinter

extension (self: StringBuilder) def appendCommaSeparated[T](ls: Seq[T])(
    reduce: (StringBuilder, T) => Unit
): Unit =
    var f = true
    for l <- ls do
      if f then f = false else self ++= ", "
      reduce(self, l)

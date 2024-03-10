package alpine
package parsing

import alpine.ast
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.SeqView.Reverse
import scala.compiletime.ops.double
import scala.collection.immutable.LazyList.cons
import scala.annotation.meta.param

class Parser(val source: SourceFile):

  import ast.*
  import Token.Kind as K

  /** The token stream. */
  private var tokens = Lexer(source)

  /** The boundary of the last consumed token. */
  private var lastBoundary = 0

  /** The next token in the stream if it has already been produced. */
  private var lookahead: Option[Token] = None

  /** The errors collected by the parser. */
  private var errors = mutable.ListBuffer[SyntaxError]()

  /** A stack of predicates identifying tokens suitable for error recovery. */
  private var recoveryPredicates = mutable.ListBuffer[Token => Boolean]()

  /** The diagnostics collected by the parser. */
  def diagnostics: DiagnosticSet =
    DiagnosticSet(errors)

  // --- Declarations ---------------------------------------------------------

  /** Parses and returns a program. */
  def program(): alpine.Program =
    @tailrec def loop(partialResult: List[Declaration]): IArray[Declaration] =
      if peek != None then
        loop(partialResult :+ topLevel())
      else
        IArray.from(partialResult)
    Program(loop(List()))

  /** Parses and returns a top-level declaration. */
  def topLevel(): Declaration =
    peek match
      case Some(Token(K.Let, _)) =>
        binding()
      case Some(Token(K.Fun, _)) =>
        function()
      case Some(Token(K.Type, _)) =>
        typeDeclaration()
      case _ =>
        recover(ExpectedTree("top-level declaration", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a binding declaration. */
  private[parsing] def binding(initializerIsExpected: Boolean = true): Binding = //TODO GREG
  /*
    val let_exp = expect(K.Let)
    val name = identifier()
    peek match
      case Some(Token(K.Colon, _)) =>
        val type_exp = tpe()
        val eq_exp = expect(K.Eq)
        val initializer = expression()
        Binding(
          name.toString,
          Some(type_exp),
          Some(initializer),
          let_exp.site.extendedToCover(initializer.site)
        )
      case Some(Token(K.Eq, _)) =>
        val type_exp = None
        val eq_exp = expect(K.Eq)
        val initializer = expression()
        Binding(
          name.toString,
          type_exp,
          Some(initializer),
          let_exp.site.extendedToCover(initializer.site)
        )
      case _ =>
        report(SyntaxError("expected ':' or '='", name.site.extendedTo(lastBoundary)))
        return Binding(name.toString, None, None, let_exp.site.extendedTo(lastBoundary)) //TODO not correct, but return what then?
  */
    val letTok = expect(K.Let)
    val i = identifier()

    if initializerIsExpected then
      peek match
        case Some(Token(K.Colon, _)) =>
          expect(K.Colon)
          val ascriptionType = tpe()

          if take(K.Eq) == None then
            recover(ExpectedTokenError(K.Eq, emptySiteAtLastBoundary), ErrorTree.apply)

          val initializer = expression()
          Binding(i.value, Some(ascriptionType), Some(initializer), letTok.site.extendedTo(initializer.site.end))

        case Some(Token(K.Eq, _)) =>
          expect(K.Eq)
          val initializer = expression()
          Binding(i.value, None, Some(initializer), letTok.site.extendedTo(initializer.site.end))

        case _ =>
          recover(ExpectedTree("':' or '='", emptySiteAtLastBoundary), ErrorTree.apply)
          Binding(i.value, None, None, letTok.site.extendedTo(i.site.end)) //TODO Antoine: False but what to return then?
    
    else
      peek match
        case Some(Token(K.Colon, _)) =>
          expect(K.Colon)
          val ascriptionType = tpe()
          Binding(i.value, Some(ascriptionType), None, letTok.site.extendedTo(ascriptionType.site.end))

        case _ =>
          Binding(i.value, None, None, letTok.site.extendedTo(i.site.end))
      

    

  /** Parses and returns a function declaration. */
  private[parsing] def function(): Function =
    val fun = expect(K.Fun)
    val identifier = expect(K.Identifier)

    val parameters = valueParameterList()
    // expect(K.LParen)
    // val parameters = peek match
    //   case Some(Token(K.RParen, _)) => valueParameterList()
    //   case _ => List()
    // expect(K.RParen)

    val funType = peek match
      case Some(Token(K.Arrow, _)) => 
        expect(K.Arrow)
        Some(tpe())
      case _ => None

    expect(K.LBrace)
    val expression = infixExpression()
    val lastToken = expect(K.RBrace)

    new Function(identifier.toString, parameters, List(), funType, expression, fun.site.extendedToCover(lastToken.site))

    
    

  /** Parses and returns the identifier of a function. */
  private def functionIdentifier(): String =
    take(K.Identifier) match
      case Some(s) =>
        s.site.text.toString
      case _ if peek.map((t) => t.kind.isOperatorPart).getOrElse(false) =>
        operatorIdentifier()(1).text.toString
      case _ =>
        missingName

  /** Parses and returns a list of parameter declarations in parentheses. */
  private[parsing] def valueParameterList(): List[Parameter] = 
    val inParens = inParentheses(() => commaSeparatedList(K.RParen.matches, parameter))
    inParens.collect({ case p: Parameter => p })
    

  /** Parses and returns a parameter declaration. */
  private[parsing] def parameter(): Declaration =
    // peek match
    //   case Some(Token(K.Identifier, _)) =>
    //     val ident_1 = identifier()
    //     val ident_2 = identifier()
    //     peek match //<identifier> <identifier> [: <type>] // labeled
    //       case Some(Token(K.Colon, _)) => 
    //         val type_exp = tpe()
    //         Parameter(Some(ident_1.toString()), ident_2.toString(), Some(type_exp), ident_1.site.extendedToCover(type_exp.site))
    //       case _ =>
    //         Parameter(Some(ident_1.toString()), ident_2.toString(), None, ident_1.site.extendedToCover(ident_2.site))

    //   case Some(Token(K.Underscore, _)) => // '_' <identifier> [: <type>] // unlabeled
    //     val underscore_exp = expect(K.Underscore)
    //     val ident_exp = identifier()
    //     peek match
    //       case Some(Token(K.Colon, _)) =>
    //         val type_exp = tpe()
    //         Parameter(None, ident_exp.toString(), Some(type_exp), underscore_exp.site.extendedToCover(type_exp.site))
    //       case _ =>
    //         Parameter(None, ident_exp.toString(), None, underscore_exp.site.extendedToCover(ident_exp.site)) 
        
    //   case _ => //<keyword> <identifier> [: <type>] // labeled by keyword TODO what?
    //     Parameter(None, identifier().toString, None, emptySiteAtLastBoundary)
    peek match
      case Some(Token(K.Label, _)) => //<identifier> <identifier> [: <type>] // labeled or //<keyword> <identifier> [: <type>] // labeled by keyword
            val ident_1 = identifier() //TODO how to handle keyword correctly??
            val ident_2 = identifier()
            peek match
              case Some(Token(K.Colon, _)) => 
                val colon_exp = expect(K.Colon)
                val type_exp = tpe()
                Parameter(Some(ident_1.toString()), ident_2.toString(), Some(type_exp), ident_1.site.extendedToCover(type_exp.site))
              case _ =>
                Parameter(Some(ident_1.toString()), ident_2.toString(), None, ident_1.site.extendedToCover(ident_2.site))

      case Some(Token(K.Underscore, _)) => //'_' <identifier> [: <type>] // unlabeled
        val underscore_exp = expect(K.Underscore)
        val ident_exp = identifier()
        peek match
          case Some(Token(K.Colon, _)) =>
            val colon_exp = expect(K.Colon)
            val type_exp = tpe()
            Parameter(None, ident_exp.toString(), Some(type_exp), underscore_exp.site.extendedToCover(type_exp.site))
          case _ =>
            Parameter(None, ident_exp.toString(), None, underscore_exp.site.extendedToCover(ident_exp.site))

      case _ =>
        recover(ExpectedTree("parameter", emptySiteAtLastBoundary), ErrorTree.apply)
    

  /** Parses and returns a type declaration. */
  private[parsing] def typeDeclaration(): TypeDeclaration = //TODO need to add generic type parameters?
    val type_exp = expect(K.Type)
    val name = expect(K.Identifier)
    val type_eq = expect(K.Eq)
    val type_body = tpe()
    TypeDeclaration(name.toString, List(), type_body, type_exp.site.extendedToCover(type_body.site)) 

  /** Parses and returns a list of parameter declarations in angle brackets. */
  //--- This is intentionally left in the handout /*+++ +++*/
  private def typeParameterList(): List[Parameter] =
    inAngles(() => commaSeparatedList(K.RAngle.matches, parameter))
      .collect({ case p: Parameter => p })

  // --- Expressions ----------------------------------------------------------

  /** Parses and returns a term-level expression. */
  def expression(): Expression =
    infixExpression()

  /** Parses and returns an infix expression. */
  private[parsing] def infixExpression(precedence: Int = ast.OperatorPrecedence.min): Expression =
    
    def infixExpressionInt(lhs: Expression, precedence: Int): Expression =
      var lhs_mut = lhs

      var (op, opSite) = peek match
        case Some(Token(K.Operator, _)) =>
          operatorIdentifier()
        case _ =>
          return lhs

      while op.get.precedence >= precedence do
        val rhs = ascribed()

        val opIdentifier = Identifier(op.get.toString, opSite)

        var (op2, opSite2) = if peek.map((t) => t.kind.isOperatorPart).getOrElse(false) then
          operatorIdentifier()
        else
          return InfixApplication(opIdentifier, lhs_mut, rhs, opSite.extendedTo(rhs.site.end))

        var op2_precedence = op2.get.precedence
        val op2Identifier = Identifier(op2.get.toString, opSite2)

        while op2_precedence > op.get.precedence do
          val rhs2 = infixExpressionInt(rhs, op2_precedence)

          op2_precedence = if peek.map((t) => t.kind.isOperatorPart).getOrElse(false) then
            operatorIdentifier()._1.get.precedence
          else
            ast.OperatorPrecedence.min

        lhs_mut = InfixApplication(op2Identifier, lhs_mut, rhs, opSite.extendedTo(rhs.site.end))

      return lhs

    infixExpressionInt(ascribed(), precedence)
      


  /** Parses and returns an expression with an optional ascription. */
  private[parsing] def ascribed(): Expression =
    val exp = prefixExpression()

    peek match
      case Some(Token(K.At, _)) | Some(Token(K.AtQuery, _)) | Some(Token(K.AtBang, _)) =>
        val token_casted = typecast()
        val type_t = tpe()

        AscribedExpression(exp, token_casted, type_t, exp.site.extendedTo(type_t.site.end))
      case _ =>
        exp
    
  /** Parses and returns a prefix application. */
  private[parsing] def prefixExpression(): Expression =
    peek match
      case Some(Token(K.Operator, _)) =>
        val o = operator()

        if noWhitespaceBeforeNextToken then
          val e = compoundExpression()

          return PrefixApplication(o.asInstanceOf[Identifier], e, o.site.extendedTo(e.site.end))
        else
          return o
      case _ =>
        return compoundExpression()
    

  /** Parses and returns a compound expression. */
  private[parsing] def compoundExpression(): Expression =
    val pe = primaryExpression()

    peek match
      case Some(Token(K.Dot, _)) | Some(Token(K.LParen, _)) => ()
      case _ => return pe

    peek.get.kind match
      case K.Dot =>
        take()

        peek match
          case Some(Token(K.Integer, _)) =>
            val i = integerLiteral()
            return Selection(pe, i, pe.site.extendedToCover(i.site))

          case Some(Token(K.Identifier, _)) =>
            val i = identifier()
            return Selection(pe, i, pe.site.extendedToCover(i.site))

          case Some(t) if t.kind.isOperatorPart =>
            operator() match
              case i: Identifier => return Selection(pe, i, pe.site.extendedTo(i.site.end))
              case _ => throw FatalError("expected identifier", emptySiteAtLastBoundary)

          case _ =>
            recover(ExpectedTree("identifier or integer literal", emptySiteAtLastBoundary), ErrorTree.apply)

      case K.LParen =>
        val args = parenthesizedLabeledList(() => expression())
        return Application(pe, args, pe.site.extendedToCover(args.last.site))

      case _ =>
        throw FatalError("expected '.' or '('", emptySiteAtLastBoundary)

    throw FatalError("unreachable", emptySiteAtLastBoundary)


  /** Parses and returns a term-level primary exression.
   *
   *  primary-expression ::=
   *    | value-identifier
   *    | integer-literal
   *    | float-literal
   *    | string-literal
   */
  private[parsing] def primaryExpression(): Expression =
    peek match
      case Some(Token(K.Identifier, s)) =>
        identifier()
      case Some(Token(K.True, _)) =>
        booleanLiteral()
      case Some(Token(K.False, _)) =>
        booleanLiteral()
      case Some(Token(K.Integer, _)) =>
        integerLiteral()
      case Some(Token(K.Float, _)) =>
        floatLiteral()
      case Some(Token(K.String, _)) =>
        stringLiteral()
      case Some(Token(K.Label, _)) =>
        recordExpression()
      case Some(Token(K.If, _)) =>
        conditional()
      case Some(Token(K.Match, _)) =>
        mtch()
      case Some(Token(K.Let, _)) =>
        let()
      case Some(Token(K.LParen, _)) =>
        lambdaOrParenthesizedExpression()
      case Some(t) if t.kind.isOperatorPart =>
        operator()
      case _ =>
        recover(ExpectedTree("expression", emptySiteAtLastBoundary), ErrorTree.apply)
  
  /** Parses and returns an Boolean literal expression. */
  private[parsing] def booleanLiteral(): BooleanLiteral =
    val s = expect("Boolean literal", K.True | K.False)
    BooleanLiteral(s.site.text.toString, s.site)

  /** Parses and returns an integer literal expression. */
  private[parsing] def integerLiteral(): IntegerLiteral =
    val s = expect(K.Integer)
    IntegerLiteral(s.site.text.toString, s.site)

  /** Parses and returns a floating-point literal expression. */
  private[parsing] def floatLiteral(): FloatLiteral =
    val s = expect(K.Float)
    FloatLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a string literal expression. */
  private[parsing] def stringLiteral(): StringLiteral =
    val s = expect(K.String)
    StringLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a term-level record expression. */
  private def recordExpression(): Record = //TODO correct?
  /*
    val label_got = expect(K.Label)
    val identf_got = expect(K.Identifier)
    peek match
      case Some(Token(K.LBrace, _)) =>
        val fields = recordExpressionFields()
        Record(identf_got.toString, fields, label_got.site.extendedToCover(fields.last.site))
      case _ =>
        Record(identf_got.toString, List(), label_got.site.extendedToCover(identf_got.site)) //TODO GREG, really empty list?
  */
    return record(recordExpressionFields, (name, fields, site) => Record(name, fields, site))


  /** Parses and returns the fields of a term-level record expression. */
  private def recordExpressionFields(): List[Labeled[Expression]] = //TODO good?
    commaSeparatedList(K.RParen.matches, () => labeled(() => expression()))
    

  /** Parses and returns a conditional expression. */
  private[parsing] def conditional(): Expression = //TODO GREG
    // Needs to become IfExpression := 'if' Expression 'then' Expression 'else' Expression
    val if_exp = expect(K.If)
    val condition = expression()

    expect(K.Then)
    val success_case = expression()

    expect(K.Else)
    val failure_case = expression()

    Conditional(condition, success_case, failure_case, if_exp.site.extendedToCover(failure_case.site))

  /** Parses and returns a match expression. */
  private[parsing] def mtch(): Expression =
    val matchTok = expect(K.Match)
    val e = expression()
    val matchCases = matchBody()

    Match(e, matchCases, matchTok.site.extendedToCover(matchCases.last.site))

  /** Parses and returns a the cases of a match expression. */
  private def matchBody(): List[Match.Case] =
    @tailrec def loop(partialResult: List[Match.Case]): List[Match.Case] =
      peek match
        case Some(Token(K.RBrace, _)) =>
          partialResult
        case Some(Token(K.Case, _)) =>
          loop(partialResult :+ matchCase())
        case _ =>
          report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
          discardUntilRecovery()
          if peek == None then partialResult else loop(partialResult)
    inBraces({ () => recovering(K.Case.matches, () => loop(List())) })

  /** Parses and returns a case in a match expression. */
  private def matchCase(): Match.Case =
    val s = peek.map((t) => t.site)
    if take(K.Case) == None then
      report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
    val p = pattern()
    if take(K.Then) == None then
      recover(
        ExpectedTokenError(K.Then, emptySiteAtLastBoundary),
        (e) => Match.Case(p, ErrorTree(e), s.get.extendedTo(lastBoundary)))
    else
      val b = expression()
      Match.Case(p, b, s.get.extendedTo(lastBoundary))

  /** Parses and returns a let expression. */
  private[parsing] def let(): Let =
    val b = binding() //TODO Antoine: Check if this is correct

    expect(K.LBrace)
    val e = expression()
    val rBrace = expect(K.RBrace)

    return Let(b, e, b.site.extendedToCover(rBrace.site))

  /** Parses and returns a lambda or parenthesized term-level expression. */
  private def lambdaOrParenthesizedExpression(): Expression = //TODO UFF check
    val s = snapshot()
    try {
      val parameters = valueParameterList()
      
      val arrow_exp = expect(K.Arrow)
      peek match
        case Some(Token(K.Type, _)) =>
          val ascribedType = tpe()
          val LBrace_exp = expect(K.LBrace)
          val e = expression()
          val rBrace_exp = expect(K.RBrace)
          Lambda(parameters, Some(ascribedType), e, arrow_exp.site.extendedToCover(rBrace_exp.site)) //TODO site correct?
        case _ =>
          val LBrace_exp = expect(K.LBrace)
          val e = expression()
          val rBrace_exp = expect(K.RBrace)
          Lambda(parameters, None, e, arrow_exp.site.extendedToCover(rBrace_exp.site)) //TODO site correct?
    }catch {
      case _ =>
        restore(s)
        val l_parent = expect(K.LParen)
        val e = expression()
        val r_parent = expect(K.RParen)
        ParenthesizedExpression(e, l_parent.site.extendedToCover(r_parent.site))
    }

    // val parenthese_exp = expect(K.LParen)
    // val first_tpe = tpe()
    // peek match 
    //   case Some(Token(K.RParen, _)) =>
    //     val rparen_exp = expect(K.RParen)
    //     ParenthesizedType(first_tpe, parenthese_exp.site.extendedToCover(rparen_exp.site))
    //   case _ =>
    //     //TODO continue??   

    // try {
    //   val tokLparen = expect(K.LParen)
    //   val e = expression()
    //   val tokRparen = expect(K.RParen)
    //   ParenthesizedExpression(e, tokLparen.site.extendedToCover(tokRparen.site))
    // } catch {
    //   case _ =>
    //     //restore(s) TODO Antoine fix??
        
    //     val parameters = valueParameterList()

    //     peek match
    //       case Some(Token(K.Arrow, _)) =>
    //         expect(K.Arrow)
    //         val ascribedType = tpe()
    //         expect(K.LBrace)
    //         val e = expression()
    //         val tokRbrace = expect(K.RBrace)

    //         //TODO Antoine: fix site
    //         Lambda(parameters, Some(ascribedType), e, ascribedType.site.extendedToCover(tokRbrace.site))
            
    //       case _ =>
    //         expect(K.LBrace)
    //         val e = expression()
    //         val tokRbrace = expect(K.RBrace)

    //         //TODO Antoine: fix site
    //         Lambda(parameters, None, e, e.site.extendedToCover(tokRbrace.site))

    // }

  /** Parses and returns an operator. */
  private def operator(): Expression =
    operatorIdentifier() match
      case (Some(o), p) => Identifier(p.text.toString, p)
      case (_, p) => ErrorTree(p)

  /** Parses and returns an operator identifier, along with its source positions.
   *
   *  If the the parsed operator is undefined, a diagnostic is reported and the returned identifier
   *  is `None`. In any case, the returned span represents the positions of the parsed identifier.
   */
  private def operatorIdentifier(): (Option[ast.OperatorIdentifier], SourceSpan) =
    import ast.OperatorIdentifier as O

    @tailrec def loop(start: Int, end: Int): (Option[ast.OperatorIdentifier], SourceSpan) =
      if takeIf((t) => t.isOperatorPartImmediatelyAfter(end)) != None then
        loop(start, lastBoundary)
      else
        val p = source.span(start, end)
        val s = p.text
        val o = if s == "||" then
          Some(O.LogicalOr)
        else if s == "&&" then
          Some(O.LogicalAnd)
        else if s == "<" then
          Some(O.LessThan)
        else if s == "<=" then
          Some(O.LessThanOrEqual)
        else if s == ">" then
          Some(O.GreaterThan)
        else if s == ">=" then
          Some(O.GreaterThanOrEqual)
        else if s == "==" then
          Some(O.Equal)
        else if s == "!=" then
          Some(O.NotEqual)
        else if s == "..." then
          Some(O.ClosedRange)
        else if s == "..<" then
          Some(O.HaflOpenRange)
        else if s == "+" then
          Some(O.Plus)
        else if s == "-" then
          Some(O.Minus)
        else if s == "|" then
          Some(O.BitwiseOr)
        else if s == "^" then
          Some(O.BitwiseXor)
        else if s == "*" then
          Some(O.Star)
        else if s == "/" then
          Some(O.Slash)
        else if s == "%" then
          Some(O.Percent)
        else if s == "&" then
          Some(O.Ampersand)
        else if s == "<<" then
          Some(O.LeftShift)
        else if s == ">>" then
          Some(O.RightShift)
        else if s == "~" then
          Some(O.Tilde)
        else if s == "!" then
          Some(O.Bang)
        else
          report(SyntaxError(s"undefined operator '${s}'", p))
          None
        (o, p)

    val h = expect("operator", (t) => t.kind.isOperatorPart)
    loop(h.site.start, h.site.end)

  /** Parses and returns a type cast operator. */
  private def typecast(): Typecast =
    peek match
      case Some(Token(K.At, _)) =>
        take(); Typecast.Widen
      case Some(Token(K.AtQuery, _)) =>
        take(); Typecast.Narrow
      case Some(Token(K.AtBang, _)) =>
        take(); Typecast.NarrowUnconditionally
      case _ =>
        throw FatalError("expected typecast operator", emptySiteAtLastBoundary)

  // --- Types ----------------------------------------------------------------

  /** Parses and returns a type-level expression. */
  private[parsing] def tpe(): Type =
    val t = primaryType()

    if take(K.Operator) != None then
      val (o, _) = operatorIdentifier() //TODO Antoine: Error here apprently, don't know why

      if Some(o) == ast.OperatorIdentifier.BitwiseOr then
        val t2 = primaryType()
        return Sum(List(t, t2), t.site.extendedTo(t2.site.end))

    TypeIdentifier(t.site.text.toString, t.site)
    

  /** Parses and returns a type-level primary exression. */
  private def primaryType(): Type =
    peek match
      case Some(Token(K.Identifier, s)) =>
        typeIdentifier()
      case Some(Token(K.Label, _)) =>
        recordType()
      case Some(Token(K.LParen, _)) =>
        arrowOrParenthesizedType()
      case _ =>
        recover(ExpectedTree("type expression", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a type identifier. */
  private def typeIdentifier(): Type =
    val i = identifier()
    TypeIdentifier(i.value, i.site)

  /** Parses and returns a list of type arguments. */
  private def typeArguments(): List[Labeled[Type]] =
    parenthesizedLabeledList(() => tpe())

  /** Parses and returns a type-level record expressions. */
  private[parsing] def recordType(): RecordType =  //TODO thats enough??
    record(recordTypeFields, (name, fields, site) => RecordType(name, fields, site))


  /** Parses and returns the fields of a type-level record expression. */
  private def recordTypeFields(): List[Labeled[Type]] = //TODO good?
    commaSeparatedList(K.RParen.matches, () => labeled(() => tpe()))

  /** Parses and returns a arrow or parenthesized type-level expression. */
  private[parsing] def arrowOrParenthesizedType(): Type =
    ??? //TODO

  // --- Patterns -------------------------------------------------------------

  /** Parses and returns a pattern. */
  private[parsing] def pattern(): Pattern =
    peek match
      case Some(Token(K.Underscore, _)) =>
        wildcard()
      case Some(Token(K.Label, _)) =>
        recordPattern()
      case Some(Token(K.Let, _)) =>
        bindingPattern()
      case _ =>
        valuePattern()

  /** Parses and returns a wildcard pattern. */
  def wildcard(): Wildcard =
    val s = expect(K.Underscore)
    Wildcard(s.site)

  /** Parses and returns a record pattern. */
  private def recordPattern(): RecordPattern =
    record(recordPatternFields, (name, fields, site) => RecordPattern(name, fields, site))

  /** Parses and returns the fields of a record pattern. */
  private def recordPatternFields(): List[Labeled[Pattern]] =
    commaSeparatedList(K.RParen.matches, () => labeled(() => pattern()))

    

  /** Parses and returns a binding pattern. */
  private def bindingPattern(): Binding =
    val letTok = expect(K.Let)
    val identifierExpr = identifier()

    peek match
      case Some(Token(K.Colon, _)) => 
        val colon_exp = expect(K.Colon)
        val type_exp = tpe()
        Binding(identifierExpr.value, Some(type_exp), None, letTok.site.extendedToCover(type_exp.site))
      case _ =>
        Binding(identifierExpr.value, None, None, letTok.site.extendedToCover(identifierExpr.site))
    
    // //TODO Antoine: Check if this is correct
    // if take(K.Colon) != None then
    //   val ascriptionType = tpe()

    //   return Binding(identifierExpr.value, Some(ascriptionType), None, letTok.site.extendedToCover(ascriptionType.site))
    // else
    //   return Binding(identifierExpr.value, None, None, letTok.site.extendedToCover(identifierExpr.site))

  /** Parses and returns a value pattern. */
  private def valuePattern(): ValuePattern =
    val e = expression()
    ValuePattern(e, e.site)

  // --- Common trees ---------------------------------------------------------

  /** Parses and returns an identifier. */
  private def identifier(): Identifier =
    val s = expect(K.Identifier)
    Identifier(s.site.text.toString, s.site)

  // --- Combinators ----------------------------------------------------------

  /** Parses and returns a record.
   *
   *  @param fields A closure parsing the fields of the record.
   *  @param make A closure constructing a record tree from its name, fields, and site.
   */
  private def record[Field <: Labeled[Tree], T <: RecordPrototype[Field]](
      fields: () => List[Field],
      make: (String, List[Field], SourceSpan) => T
  ): T =
    val identifier = expect(K.Label)

    // Ensure the first character is a '#'
    if identifier.site.text.toString.charAt(0) != '#' then
      report(SyntaxError("record identifier must start with a '#'", identifier.site))
      throw FatalError("expected '#'", emptySiteAtLastBoundary)

    // Singleton record
    if take(K.LParen) == None then
      return make(identifier.site.text.toString, List(), identifier.site)
    else
      val f = fields()

      if take(K.RParen) == None then
        report(ExpectedTokenError(K.RParen, emptySiteAtLastBoundary))
        throw FatalError("expected ')'", emptySiteAtLastBoundary)

      make(identifier.site.text.toString, f, identifier.site)

  /** Parses and returns a parenthesized list of labeled value.
   *
   *  See also [[this.labeledList]].
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def parenthesizedLabeledList[T <: Tree](
      value: () => T
  ): List[Labeled[T]] =
    expect(K.LParen)
    val l = commaSeparatedList(K.RParen.matches, () => labeled(value))
    expect(K.RParen)
    return l

  /** Parses and returns a value optionally prefixed by a label.
   *
   *  This combinator attempts to parse a label `n` followed by a colon and then applies `value`.
   *  If that succeeds, returned tree is the result of `value` labeled by `n`. If there is no label,
   *  the combinator backtracks, re-applies `value`, and returns its result sans label.
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def labeled[T <: Tree](
      value: () => T
  ): Labeled[T] =
    if peek == None | (peek.get.kind != K.Identifier && !peek.get.kind.isKeyword) then
      val v = value()
      return Labeled(None, v, v.site)

    val t = take().get
    expect(K.Colon)
    val v = value()

    return Labeled(Some(t.site.text.toString), v, t.site.extendedToCover(v.site))    

  /** Parses and returns a sequence of `element` separated by commas and delimited on the RHS  by a
   *  token satisfying `isTerminator`.
   */
  private[parsing] def commaSeparatedList[T](isTerminator: Token => Boolean, element: () => T): List[T] =
    @tailrec def loop(partialResult: List[T]): List[T] =
      if peek.map(isTerminator).getOrElse(false) then
        partialResult
      else
        val nextPartialResult = partialResult :+ recovering(K.Comma.matches, element)
        if peek.map(isTerminator).getOrElse(false) then
          nextPartialResult
        else if take(K.Comma) != None then
          loop(nextPartialResult)
        else
          report(ExpectedTokenError(K.Comma, emptySiteAtLastBoundary))
          loop(nextPartialResult)
    loop(List())

  /** Parses and returns `element` surrounded by a pair of parentheses. */ 
  private[parsing] def inParentheses[T](element: () => T): T =
    val left = expect(K.LParen)
    val contents = recovering(K.RParen.matches, element)
    val right = expect(K.RParen)
    contents
    
  /** Parses and returns `element` surrounded by a pair of braces. */
  private[parsing] def inBraces[T](element: () => T): T =
    val left = expect(K.LBrace)
    val contents = recovering(K.RBrace.matches, element)
    val right = expect(K.RBrace)
    contents

  /** Parses and returns `element` surrounded by angle brackets. */
  private[parsing] def inAngles[T](element: () => T): T =
    val left = expect(K.LAngle)
    val contents = recovering(K.RAngle.matches, element)
    val right = expect(K.RAngle)
    contents

  /** Parses and returns `element` surrounded by a `left` and `right`. */
  private[parsing] def delimited[T](left: Token.Kind, right: Token.Kind, element: () => T): T =
    if take(left) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    val contents = recovering(right.matches, element)
    if take(right) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    contents

  /** Returns the result of `element` with `isRecoveryToken` added to the recovery predicates. */
  private def recovering[T](isRecoveryToken: Token => Boolean, element: () => T): T =
    recoveryPredicates += isRecoveryToken
    try
      element()
    finally
      recoveryPredicates.dropRightInPlace(1)

  // --- Utilities ------------------------------------------------------------

  /** Returns `true` iff there isn't any whitespace before the next token in the stream. */
  private def noWhitespaceBeforeNextToken: Boolean =
    peek.map((t) => lastBoundary == t.site.start).getOrElse(false)

  /** Reports a missing identifier and returns "_". */
  def missingName =
    report(ExpectedTokenError(K.Identifier, emptySiteAtLastBoundary))
    "_"

  /** Reports `error`, advances the stream to the next recovery token, and returns the result of
   *  calling `errorTree` on the skipped positions.
   */
  private def recover[T](error: SyntaxError, errorTree: SourceSpan => T): T =
    report(error)
    errorTree(discardUntilRecovery())

  /** Advances the stream to the next recovery token and returns the skipped positions. */
  private def discardUntilRecovery(): SourceSpan =
    @tailrec def loop(s: Int): SourceSpan =
      if !peek.isDefined || Reverse(recoveryPredicates).exists((p) => p(peek.get)) then
        source.span(s, lastBoundary)
      else
        take()
        loop(s)
    loop(lastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(construct: String, predicate: (Token) => Boolean): Token =
    takeIf(predicate) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${construct}", emptySiteAtLastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(k: Token.Kind): Token =
    take(k) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${k}", emptySiteAtLastBoundary)

  /** Returns the next token in the stream without consuming it. */
  private def peek: Option[Token] =
    if lookahead == None then
      lookahead = tokens.next()
    lookahead

  /** Consumes the next token in the stream iff it has kind `k` and returns the result of `action`
   *  applied on that token. */
  private def taking[T](k: Token.Kind, action: Token => T): Option[T] =
    take(k).map(action)

  /** Consumes and returns the next token in the stream. */
  private def take(): Option[Token] =
    peek.map({ (next) =>
      lastBoundary = next.site.end
      lookahead = None
      next
    })

  /** Consumes and returns the next token in the stream iff it has kind `k`. */
  private def take(k: Token.Kind): Option[Token] =
    takeIf(k.matches)

  /** Consumes and returns the next character in the stream iff it satisfies `predicate`. */
  private def takeIf(predicate: Token => Boolean): Option[Token] =
    if peek.map(predicate).getOrElse(false) then take() else None

  /** Returns an empty range at the position of the last consumed token. */
  private def emptySiteAtLastBoundary: SourceSpan =
    source.span(lastBoundary, lastBoundary)

  /** Reports the given diagnostic. */
  private def report(d: SyntaxError): Unit =
    errors += d

  /** Returns a backup of this instance's state. */
  private[parsing] def snapshot(): Parser.Snapshot =
    Parser.Snapshot(
      tokens.copy(), lastBoundary, lookahead, errors.length, recoveryPredicates.length)

  /** Restores this instance to state `s`. */
  private[parsing] def restore(s: Parser.Snapshot): Unit =
    tokens = s.tokens
    lastBoundary = s.lastBoundary
    lookahead = s.lookahead
    errors.dropRightInPlace(errors.length - s.errorCount)
    recoveryPredicates.dropRightInPlace(recoveryPredicates.length - s.recoveryPredicateCount)

end Parser

object Parser:

  /** The information necessary to restore the state of a parser instance. */
  private[parsing] final case class Snapshot(
      tokens: Lexer,
      lastBoundary: Int,
      lookahead: Option[Token],
      errorCount: Int,
      recoveryPredicateCount: Int)

end Parser

extension (self: Token.Kind) def | (other: Token.Kind): (Token) => Boolean =
  (t) => (t.kind == self) || (t.kind == other)

extension (self: Token => Boolean) def | (other: Token.Kind): (Token) => Boolean =
  (t) => self(t) || (t.kind == other)

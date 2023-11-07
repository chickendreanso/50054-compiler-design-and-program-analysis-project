package sutd.compiler.simp.syntax

import sutd.compiler.simp.syntax.Lexer.*
import sutd.compiler.simp.syntax.SrcLoc.*
import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.syntax.Parsec.*
import org.scalactic.Bool
import java.nio.channels.ReadPendingException

object Parser {
    /**
     * S ::= X = E ; | return X ; | nop | if E { \overline{S} } else { \overline{S} } | while E { \overline{S} } 
     * E ::= E Op E | X | C | (E)
     * \overline{S} ::= S | S \overline{S}
     * Op ::= + | - | *  
     * C ::= 1 | 2 | ... | true | false 
     * X ::= a | b | c | d 
     * */


    import Stmt.*
    import Exp.*
    import Const.* 
    
    import LToken.*
    import Progress.*
    import Result.*

    case class PEnv(toks: List[LToken])

    /**
      * check whether the parsing is done based on the list of tokens left.
      *
      * @param env
      * @return boolean
      */
    def done(env:PEnv):Boolean = env match {
        case PEnv(Nil) => true
        case _ => false
    }

    /**
      * type class instance of ParserEnv[PEnv, LToken]
      */
    given penvParserEnv: ParserEnv[PEnv, LToken] = new ParserEnv[PEnv, LToken] {
        override def getTokens(env: PEnv): List[LToken] = env match {
            case PEnv(toks) => toks
        }
        override def getCol(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => col
                }
        }
        override def getLine(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => ln
                }
        }
        override def setTokens(ts: List[LToken])(env: PEnv): PEnv = env match {
            case PEnv(_) => PEnv(ts)
        }

    }
    /**
      * The top level parser
      */
    def parse:Parser[PEnv, List[Stmt]] = p_stmts

    /**
      * Parsing a sequence of statements, 
      * we skip the preceeding and the proceeding white spaces for each statement.
      * for individual statement parser, 
      * we only need to skip the whitespace in between.
      *
      * @return
      */
    def p_stmts:Parser[PEnv, List[Stmt]] = {
        def p_one:Parser[PEnv, Stmt] = for {
            _ <- p_spaces
            s <- p_stmt
            _ <- p_spaces
        } yield s
        many(p_one)
    } 

    /**
      * Parsing a statement
      *
      * @return
      */
    def p_stmt:Parser[PEnv, Stmt] = choice(p_assign)(choice(p_ret)(choice(p_nop)(choice(p_ifelse)(p_while))))

    /**
      * Parsing a Nop statement
      *
      * @return
      */
    def p_nop:Parser[PEnv, Stmt] = for {
        _ <- sat((tok:LToken) => tok match {
            case NopKW(src) => true 
            case _ => false 
        })
        _ <- p_spaces
        _ <- p_semicolon
    } yield Nop

    /**
      * Parsing an assignment statement
      *
      * @return
      */
    def p_assign:Parser[PEnv, Stmt] = for {
        x <- p_var
        _ <- p_spaces
        _ <- p_equal
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_semicolon
    } yield Assign(x, e)

    /**
      * Parsing a return statement
      *
      * @return
      */
    def p_ret:Parser[PEnv, Stmt] = for {
        _ <- p_returnKW
        _ <- p_spaces
        x <- p_var
        _ <- p_spaces
        _ <- p_semicolon
    } yield Ret(x)

    /**
      * Parsing an if-else statement
      *
      * @return
      */
    def p_ifelse:Parser[PEnv, Stmt] = for {
        _ <- p_ifKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        s1 <- p_stmts
        _ <- p_rbrace
        _ <- p_spaces 
        _ <- p_elseKW
        _ <- p_spaces
        _ <- p_lbrace
        s2 <- p_stmts
        _ <- p_rbrace
    } yield If(e, s1, s2)

    /**
      * Parsing a while statement
      *
      * @return
      */
    def p_while:Parser[PEnv, Stmt] = for {
        _ <- p_whileKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        s <- p_stmts
        _ <- p_rbrace
    } yield While(e, s)

    /** Lab 1 Task 1.1
      * parsing / skipping whitespaces
      *
      * @return
      */

    def p_space:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhiteSpace(src, c) => true
        case _ => false
    })
    
    def p_spaces:Parser[PEnv, List[LToken]] = many(p_space) // fixme

    /** Lab 1 Task 1.1 end */


    /** Lab 1 Task 1.2 
      * Parsing an expression
      * Note that 
      *   E ::= E Op E | X | C | (E) contains left recursion
      *  without left recursion: 
      *   E ::= X E' | C E' | (E) E'
      *   E' ::= Op E E' | \epsilon
      * @return
      */ 
    def p_exp:Parser[PEnv, Exp] = for {
        ele <- p_expLE
    } yield fromExpLE(ele)
     // fixme

    // def p_opexp:Parser[PEnv, Exp] = for { //left recursion
    //     e1 <- p_exp
    //     _ <- p_spaces
    //     op <- choice(choice(choice(p_plus)(p_minus))(p_mult))(choice(p_lthan)(choice(p_dequal)(p_equal)))
    //     _ <- p_spaces
    //     e2 <- p_exp
    // } yield OpExp(op, e1, e2)

    // def p_varExp:Parser[PEnv, Exp] = for {
    //     x <- p_var
    // } yield VarExp(x)

    // def p_constExp:Parser[PEnv, Exp] = for {
    //     c <- p_const
    // } yield ConstExp(c)

    // def p_paranExp:Parser[PEnv, Exp] = for {
    //     _ <- p_lparen
    //     e <- p_exp
    //     _ <- p_rparen
    // } yield e

    enum ExpLE {
        case VarExpLE(v: Var, e: ExpLEP)
        case ConstExpLE(c: Const, e: ExpLEP)
        case ParenExpLE(e: ExpLE, ep: ExpLEP)
    }

    enum ExpLEP{
        case PlusExpLEP(e: ExpLE, ep: ExpLEP)
        case MinusExpLEP(e: ExpLE, ep: ExpLEP)
        case MultExpLEP(e: ExpLE, ep: ExpLEP)
        case LtExpLEP(e: ExpLE, ep: ExpLEP)
        // case EqExpLEP(e: ExpLE, ep: ExpLEP)
        case DEqExpLEP(e: ExpLE, ep: ExpLEP)
        case Eps
    }

    def fromExpLE(e:ExpLE):Exp= e match{
        case ExpLE.VarExpLE(v, ep) => fromExpLEP(VarExp(v))(ep)
        case ExpLE.ConstExpLE(c, ep) => fromExpLEP(ConstExp(c))(ep)
        case ExpLE.ParenExpLE(e1, ep) => fromExpLEP(ParenExp(fromExpLE(e1)))(ep)

    }

    def fromExpLEP(e1: Exp)(ep1:ExpLEP):Exp= ep1 match{
        case ExpLEP.PlusExpLEP(e2, ep2) => {
            val e3 = fromExpLE(e2)
            val e4 = Exp.Plus(e1, e3)
            fromExpLEP(e4)(ep2)
        }
        case ExpLEP.MinusExpLEP(e2, ep2) => {
            val e3 = fromExpLE(e2)
            val e4 = Exp.Minus(e1, e3)
            fromExpLEP(e4)(ep2)
        }
        case ExpLEP.MultExpLEP(e2, ep2) => {
            val e3 = fromExpLE(e2)
            val e4 = Exp.Mult(e1, e3)
            fromExpLEP(e4)(ep2)
        }
        case ExpLEP.LtExpLEP(e2, ep2) => {
            val e3 = fromExpLE(e2)
            val e4 = Exp.LThan(e1, e3)
            fromExpLEP(e4)(ep2)
        }
        case ExpLEP.DEqExpLEP(e2, ep2) => {
            val e3 = fromExpLE(e2)
            val e4 = Exp.DEqual(e1, e3)
            fromExpLEP(e4)(ep2)
        }
        case ExpLEP.Eps => e1
    }

    def p_expLE:Parser[PEnv, ExpLE] = choice(choice(p_varExpLE)(p_constExpLE))(p_paranExpLE)

    def p_expLEP:Parser[PEnv, ExpLEP] = for{
        omt <- optional(choice(attempt(p_plusExp))(choice(attempt(p_minusExp))(choice(attempt(p_multExp))(choice(attempt(p_ltExp))(attempt(p_dEqExp))))))
        } yield {
            omt match{
                case Right(omt) => omt
                case Left(_) => ExpLEP.Eps
            }
        }
    

    def p_plusExp:Parser[PEnv, ExpLEP] = for{
        _ <- p_spaces
        _ <- p_plus
        _ <- p_spaces
        e <- p_expLE
        ep <- p_expLEP
    } yield ExpLEP.PlusExpLEP(e, ep)

    def p_minusExp:Parser[PEnv, ExpLEP] = for{
        _ <- p_spaces
        _ <- p_minus
        _ <- p_spaces
        e <- p_expLE
        ep <- p_expLEP
    } yield ExpLEP.MinusExpLEP(e, ep)

    def p_ltExp:Parser[PEnv, ExpLEP] = for{
        _ <- p_spaces
        _ <- p_lthan
        _ <- p_spaces
        e <- p_expLE
        ep <- p_expLEP
    } yield ExpLEP.LtExpLEP(e, ep)

    def p_multExp:Parser[PEnv, ExpLEP] = for{
        _ <- p_spaces
        _ <- p_mult
        _ <- p_spaces
        e <- p_expLE
        ep <- p_expLEP
    } yield ExpLEP.MultExpLEP(e, ep)

    // def p_eqExp:Parser[PEnv, ExpLEP] = for{
    //     _ <- p_spaces
    //     _ <- p_equal
    //     _ <- p_spaces
    //     e <- p_expLE
    //     ep <- p_expLEP
    // } yield ExpLEP.EqExpLEP(e, ep)

    def p_dEqExp:Parser[PEnv, ExpLEP] = for{
        _ <- p_spaces
        _ <- p_dequal
        _ <- p_spaces
        e <- p_expLE
        ep <- p_expLEP
    } yield ExpLEP.DEqExpLEP(e, ep)

    def p_varExpLE:Parser[PEnv, ExpLE] = for {
        x <- p_var
        e <- p_expLEP
    } yield ExpLE.VarExpLE(x, e)

    def p_constExpLE:Parser[PEnv, ExpLE] = for {
        c <- p_const
        e <- p_expLEP
    } yield ExpLE.ConstExpLE(c, e)

    def p_paranExpLE:Parser[PEnv, ExpLE] = for {
        _ <- p_lparen
        e <- p_expLE
        _ <- p_rparen
        ep <- p_expLEP
    } yield ExpLE.ParenExpLE(e, ep)
    /** Lab 1 Task 1.2 end */
    
    /**
      * Parsing operator symbols
      *
      * @return
      */
    def p_plus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case PlusSign(_) => true 
        case _ => false
    })

    def p_minus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case MinusSign(_) => true 
        case _ => false
    })

    def p_mult:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case AsterixSign(_) => true 
        case _ => false
    })

    def p_lthan:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case LThanSign(_) => true 
        case _ => false
    })

    def p_dequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case DEqSign(_) => true 
        case _ => false
    })

    def p_equal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case EqSign(_) => true 
        case _ => false
    })

    /**
      * Parsing a Variable
      *
      * @return
      */
    def p_var:Parser[PEnv, Var] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IdTok(src, v) => true
            case _ => false 
        })
        name <- someOrFail(tok)( t => t match {
            case IdTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an identifier, but None is returned.") // this error should never occur.
    } yield Var(name)


    /**
      * Parsing a Constant
      *
      * @return
      */
    def p_const:Parser[PEnv, Const] = choice(choice(p_true)(p_false))(p_int)

    def p_true:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case TrueKW(src) => true
            case _ => false 
        })
    } yield BoolConst(true)

    def p_false:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case FalseKW(src) => true
            case _ => false 
        })
    } yield BoolConst(false)

    def p_int:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IntTok(src, v) => true
            case _ => false
        })
        i <- someOrFail(tok)( t => t match {
            case IntTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an integer, but None is returned.") // this error should never occur.
    } yield IntConst(i)

    /**
      * Parsing keywords
      *
      * @return
      */
    def p_returnKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RetKW(src) => true
        case _ => false
    })

    def p_ifKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case IfKW(src) => true
        case _ => false
    })

    def p_elseKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case ElseKW(src) => true
        case _ => false
    })

    def p_whileKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhileKW(src) => true
        case _ => false
    })



    /**
      * Parsing symbols
      */
    def p_lbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LBrace(src) => true
        case _ => false
    })

    def p_rbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RBrace(src) => true
        case _ => false
    })


    def p_lparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LParen(src) => true
        case _ => false
    })

    def p_rparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RParen(src) => true
        case _ => false
    })

    def p_semicolon:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case SemiColon(src) => true
        case _ => false
    })



}
package sutd.compiler.simp

import sutd.compiler.simp.ir.PseudoAssembly.*
import sutd.compiler.simp.ir.PAPrettyPrinter.*
import org.scalatest.funsuite 
import scala.language.adhocExtensions

class TestPAPrettyPrinter extends funsuite.AnyFunSuite {
    import Instr.* 
    import Opr.*
    test("pretty print"){
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val y = Temp(AVar("y"))
        val z = Temp(AVar("z"))
        val w = Temp(AVar("w"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IPlus(y, x, IntLit(1))),
            (3, IPlus(z, y, IntLit(1))),
            (4, IMult(w, y, z)),
            (5, IMove(r_ret, w)),
            (6, IRet)
        
        )
        val expected = List(
            "1: x <- input",
            "2: y <- x + 1",
            "3: z <- y + 1",
            "4: w <- y * z",
            "5: _r_ret <- w",
            "6: ret "
        )

        assert(prettyList(pa) == expected)
        prettyPrint(prettyList(pa))
    }

    test("pretty print 2"){
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val s = Temp(AVar("s"))
        val c = Temp(AVar("c"))
        val b = Temp(AVar("b"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(s, IntLit(0))),
            (3, IMove(c, IntLit(0))),
            (4, ILThan(b, c, x)),
            (5, IIfNot(b, 9)),
            (6, IPlus(s, c, s)),
            (7, IPlus(c, c, IntLit(1))),
            (8, IGoto(4)),
            (9, IMove(r_ret, s)),
            (10, IRet)
        )

        val expected = List(
            "1: x <- input",
            "2: s <- 0",
            "3: c <- 0",
            "4: b <- c < x",
            "5: if not b goto 9",
            "6: s <- c + s",
            "7: c <- c + 1",
            "8: goto 4",
            "9: _r_ret <- s",
            "10: ret "
        )
        
        assert(prettyList(pa) == expected)
        prettyPrint(prettyList(pa))
    }

    test("pretty print 3") {
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val c = Temp(AVar("c"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(f, IntLit(0))),
            (3, IMove(s, IntLit(1))),
            (4, IMove(c, IntLit(0))),
            (5, IMove(t, IntLit(0))),
            (6, ILThan(b, c, x)),
            (7, IIfNot(b, 13)),
            (8, IMove(t, f)),
            (9, IMove(f, s)),
            (10, IPlus(s, t, f)),
            (11, IPlus(c, c, IntLit(1))),
            (12, IGoto(6)),
            (13, IMove(r_ret, s)),
            (14, IRet)
        )

        val expected = List(
            "1: x <- input",
            "2: f <- 0",
            "3: s <- 1",
            "4: c <- 0",
            "5: t <- 0",
            "6: b <- c < x",
            "7: if not b goto 13",
            "8: t <- f",
            "9: f <- s",
            "10: s <- t + f",
            "11: c <- c + 1",
            "12: goto 6",
            "13: _r_ret <- s",
            "14: ret "
        )

        assert(prettyList(pa) == expected)
        prettyPrint(prettyList(pa))
    }

    test("pretty print 4") {
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val r = Temp(AVar("r"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val i = Temp(AVar("i"))
        val j = Temp(AVar("j"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(r, IntLit(0))),
            (3, IMove(i, IntLit(0))),
            (4, ILThan(b, i, x)),
            (5, IIfNot(b, 20)),
            (6, IMove(f, IntLit(0))),
            (7, IMove(s, IntLit(1))),
            (8, IMove(j, IntLit(0))),
            (9, IMove(t, IntLit(0))),
            (10, ILThan(b, j, i)),
            (11, IIfNot(b, 17)),
            (12, IMove(t, f)),
            (13, IMove(f, s)),
            (14, IPlus(s, t, f)),
            (15, IPlus(j, j, IntLit(1))),
            (16, IGoto(10)),
            (17, IPlus(r, r, s)),
            (18, IPlus(i, i, IntLit(1))),
            (19, IGoto(4)),
            (20, IMove(r_ret, r)),
            (21, IRet)
        )

        val expected = List(
            "1: x <- input",
            "2: r <- 0",
            "3: i <- 0",
            "4: b <- i < x",
            "5: if not b goto 20",
            "6: f <- 0",
            "7: s <- 1",
            "8: j <- 0",
            "9: t <- 0",
            "10: b <- j < i",
            "11: if not b goto 17",
            "12: t <- f",
            "13: f <- s",
            "14: s <- t + f",
            "15: j <- j + 1",
            "16: goto 10",
            "17: r <- r + s",
            "18: i <- i + 1",
            "19: goto 4",
            "20: _r_ret <- r",
            "21: ret "
        )

        assert(prettyList(pa) == expected)
        prettyPrint(prettyList(pa))
    }
}
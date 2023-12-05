package sutd.compiler.simp.ir

import sutd.compiler.simp.ir.PseudoAssembly.*


object PAPrettyPrinter{
    import Instr.*
    import Opr.*
    
    def prettyList(paProg: List[LabeledInstr]): List[String] = {
        paProg.map(li => li match {
            case (int, instr) => instr match {
                case IMove(dest, src) => s"$int: " + getOperand(dest) + " <- " + getOperand(src)
                case IPlus(dest, src1, src2) => s"$int: " + getOperand(dest) + " <- " + getOperand(src1) + " + " + getOperand(src2)
                case IMinus(dest, src1, src2) => s"$int: " + getOperand(dest) + " <- " + getOperand(src1) + " - " + getOperand(src2)
                case IMult(dest, src1, src2) => s"$int: " + getOperand(dest) + " <- " + getOperand(src1) + " * " + getOperand(src2)
                case IDEqual(dest, src1, src2) => s"$int: " + getOperand(dest) + " <- " + getOperand(src1) + " == " + getOperand(src2)
                case ILThan(dest, src1, src2) => s"$int: " + getOperand(dest) + " <- " + getOperand(src1) + " < " + getOperand(src2)
                case IRet => s"$int: ret " 
                case IIfNot(cond, dest) => s"$int: if not " + getOperand(cond) + s" goto $dest"
                case IGoto(dest) => s"$int: goto $dest"
            }
        })
    }

    def prettyPrint(prettyList: List[String]) = {
        println(prettyList.mkString("\n"))
    }

    def getOperand(opr: Opr): String = {
        opr match {
            case Regstr(name) => s"$name"
            case IntLit(v) => s"$v"
            case Temp(AVar(v)) => s"$v"
        }
    }
}
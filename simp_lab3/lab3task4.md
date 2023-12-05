## Task 4: Mastering Scala and Compiler Design (2 marks)

### Pretty Printer for PA

The Scala code for the pretty printer is in the file `src/main/scala/sutd/compiler/simp/ir/PAPrettyPrinter.scala`.

The function `getOperand` takes in an operand `Opr` and returns a string representation of the operand.

The function `prettyList` takes in a list of labelled instruction `List[LabeledInstr]` and returns a list of prettified string `List[String]`

The function `prettyPrint` can be called to print the output of the function `prettyList` as a string output.

## Testing

The Scala code to test the pretty printer is in the file `src/test/scala/sutd/compiler/simp/TestPAPrettyPrinter.scala`. There are 4 test cases for the pretty printer.

`sbt "test:testOnly *TestPAPrettyPrinter"`

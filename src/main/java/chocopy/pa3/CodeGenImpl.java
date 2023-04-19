package chocopy.pa3;

import java.lang.reflect.Parameter;
import java.util.List;

import chocopy.common.analysis.SymbolTable;
import chocopy.common.analysis.AbstractNodeAnalyzer;
import chocopy.common.astnodes.*;
import chocopy.common.astnodes.Stmt;
import chocopy.common.astnodes.ReturnStmt;
import chocopy.common.codegen.*;

import static chocopy.common.codegen.RiscVBackend.Register.*;

/**
 * This is where the main implementation of PA3 will live.
 *
 * A large part of the functionality has already been implemented
 * in the base class, CodeGenBase. Make sure to read through that
 * class, since you will want to use many of its fields
 * and utility methods in this class when emitting code.
 *
 * Also read the PDF spec for details on what the base class does and
 * what APIs it exposes for its sub-class (this one). Of particular
 * importance is knowing what all the SymbolInfo classes contain.
 */
public class CodeGenImpl extends CodeGenBase {

    /** A code generator emitting instructions to BACKEND. */
    public CodeGenImpl(RiscVBackend backend) {
        super(backend);
    }

    /** Operation on None. */
    private final Label errorNone = new Label("error.None");
    /** Division by zero. */
    private final Label errorDiv = new Label("error.Div");
    /** Index out of bounds. */
    private final Label errorOob = new Label("error.OOB");

    /**
     * Emits the top level of the program.
     *
     * This method is invoked exactly once, and is surrounded
     * by some boilerplate code that: (1) initializes the heap
     * before the top-level begins and (2) exits after the top-level
     * ends.
     *
     * You only need to generate code for statements.
     *
     * @param statements top level statements
     */
    protected void emitTopLevel(List<Stmt> statements) {
        StmtAnalyzer stmtAnalyzer = new StmtAnalyzer(null);
        backend.emitADDI(SP, SP, -2 * backend.getWordSize(),
                         "Saved FP and saved RA (unused at top level).");
        backend.emitSW(ZERO, SP, 0, "Top saved FP is 0.");
        backend.emitSW(ZERO, SP, 4, "Top saved RA is 0.");
        backend.emitADDI(FP, SP, 2 * backend.getWordSize(),
                         "Set FP to previous SP.");

        for (Stmt stmt : statements) {
            stmt.dispatch(stmtAnalyzer);
        }
        backend.emitLI(A0, EXIT_ECALL, "Code for ecall: exit");
        backend.emitEcall(null);
    }

    /**
     * Emits the code for a function described by FUNCINFO.
     *
     * This method is invoked once per function and method definition.
     * At the code generation stage, nested functions are emitted as
     * separate functions of their own. So if function `bar` is nested within
     * function `foo`, you only emit `foo`'s code for `foo` and only emit
     * `bar`'s code for `bar`.
     */
    protected void emitUserDefinedFunction(FuncInfo funcInfo) {
        //FIXME
        backend.emitGlobalLabel(funcInfo.getCodeLabel());
        backend.emitMV(FP, SP, "Setting up frame pointer");
        backend.emitADDI(SP, SP, -4, "Decrementing sp");
        backend.emitSW(RA, SP, 0, "Pushing return address onto stack");

        StmtAnalyzer stmtAnalyzer = new StmtAnalyzer(funcInfo);

        for (Stmt stmt : funcInfo.getStatements()) {
            stmt.dispatch(stmtAnalyzer);
        }

        backend.emitLW(RA, SP, 0, "Popping return address off stack");
        backend.emitADDI(SP, SP, 4 * funcInfo.getParams().size() + 4, "Restoring frame pointer");
        backend.emitLW(FP, SP, 0, "Popping fp off stack");
        backend.emitJR(RA, "Jumping to return address");
    }

    /** An analyzer that encapsulates code generation for statements. */
    private class StmtAnalyzer extends AbstractNodeAnalyzer<Void> {
        /*
         * The symbol table has all the info you need to determine
         * what a given identifier 'x' in the current scope is. You can
         * use it as follows:
         *   SymbolInfo x = sym.get("x");
         *
         * A SymbolInfo can be one the following:
         * - ClassInfo: a descriptor for classes
         * - FuncInfo: a descriptor for functions/methods
         * - AttrInfo: a descriptor for attributes
         * - GlobalVarInfo: a descriptor for global variables
         * - StackVarInfo: a descriptor for variables allocated on the stack,
         *      such as locals and parameters
         *
         * Since the input program is assumed to be semantically
         * valid and well-typed at this stage, you can always assume that
         * the symbol table contains valid information. For example, in
         * an expression `foo()` you KNOW that sym.get("foo") will either be
         * a FuncInfo or ClassInfo, but not any of the other infos
         * and never null.
         *
         * The symbol table in funcInfo has already been populated in
         * the base class: CodeGenBase. You do not need to add anything to
         * the symbol table. Simply query it with an identifier name to
         * get a descriptor for a function, class, variable, etc.
         *
         * The symbol table also maps nonlocal and global vars, so you
         * only need to lookup one symbol table and it will fetch the
         * appropriate info for the var that is currently in scope.
         */

        /** Symbol table for my statements. */
        private SymbolTable<SymbolInfo> sym;

        /** Label of code that exits from procedure. */
        protected Label epilogue;

        /** The descriptor for the current function, or null at the top
         *  level. */
        private FuncInfo funcInfo;

        /** An analyzer for the function described by FUNCINFO0, which is null
         *  for the top level. */
        StmtAnalyzer(FuncInfo funcInfo0) {
            funcInfo = funcInfo0;
            if (funcInfo == null) {
                sym = globalSymbols;
            } else {
                sym = funcInfo.getSymbolTable();
            }
            epilogue = generateLocalLabel();
        }

        //Override everything in AbstractNodeAnalyzer in order

        @Override
        public Void analyze(AssignStmt stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(BinaryExpr stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(BooleanLiteral stmt) {
            // Push the boolean value onto the stack
            if (stmt.value) {
                backend.emitLI(A0, 1, "Pushing \"true\" literal onto stack");
            } else {
                backend.emitLI(A0, 0, "Pushing \"false\" literal onto stack");
            }
            //Box the boolean
            backend.emitJAL(makebool, "Boxing boolean");
            return null;
        }

        @Override
        public Void analyze(CallExpr stmt) {
            //Push fp
            backend.emitADDI(SP, SP, -4, "Pushing fp part 1: decrementing sp");
            backend.emitSW(FP, SP, 0, "Pushing fp part 2: saving fp");

            //Body
            for (int i = stmt.args.size() - 1; i >= 0; i--) {
                //Cgen the args...
                stmt.args.get(i).dispatch(this);
                //...and push them onto the stack
                backend.emitADDI(SP, SP, -4, "Decrementing sp");
                backend.emitSW(A0, SP, 0, "Pushing arg onto stack");
            }

            FuncInfo f;

            //FIXME: Find the funcInfo object associated with stmt (Is this correct? Should I be checking for if func is null first?)
            if (funcInfo == null) {
                f = (FuncInfo) globalSymbols.get(stmt.function.name);
            } else {
                f = (FuncInfo) sym.get(stmt.function.name);
            }

            //System.out.println("Function name: " + stmt.function.name);
            //System.out.println("Function info: " + f);

            //Jump to the function
            backend.emitJAL(f.getCodeLabel(), "Jumping to function " + stmt.function.name);

            //FIXME: Do I need to pop the args off the stack?
            //backend.emitADDI(SP, SP, 4 * stmt.args.size(), "Popping args off stack");
            return null;
        }

        @Override
        public Void analyze(ClassDef stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(ClassType stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(CompilerError stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(Errors stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(ExprStmt stmt) {
            stmt.expr.dispatch(this);
            return null;
        }

        @Override
        public Void analyze(ForStmt stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(FuncDef stmt) {
            //FIXME
            Label f = new Label(stmt.name.name);
            backend.emitGlobalLabel(f); //FIXME: Is it supposed to be global? Does it matter?
            System.out.println("Here inside analyze funcdef");
            backend.emitMV(FP, SP, "Setting up frame pointer");
            System.out.println("We are not skipping anything!");
            backend.emitADDI(SP, SP, -4, "Decrementing sp");
            backend.emitSW(RA, SP, 0, "Pushing return address onto stack");

            StmtAnalyzer sa = new StmtAnalyzer(funcInfo);

            //Generate all the declarations in the body
            for (Declaration d : stmt.declarations) {
                d.dispatch(sa);
            }
            //Generate all the statements in the body
            for (Stmt s : stmt.statements) {
                s.dispatch(sa);
            }

            backend.emitLW(RA, SP, 0, "Popping return address off stack");
            backend.emitADDI(SP, SP, 4 * stmt.params.size() + 4, "Restoring frame pointer");
            backend.emitLW(FP, SP, 0, "Popping fp off stack");
            backend.emitJR(RA, "Jumping to return address");
            return null;
        }

        @Override
        public Void analyze(GlobalDecl stmt) {
            //FIXME
            stmt.dispatch(this);

            return null;
        }

        @Override
        public Void analyze(Identifier stmt) {
            //FIXME
            SymbolInfo temp = sym.get(stmt.name);
            if (temp instanceof GlobalVarInfo) {
                GlobalVarInfo v = (GlobalVarInfo) temp;
                v.getInitialValue().dispatch(this);
                /*
                //System.out.println("Variable name: " + v.getVarName() + "\nVariable type: " + v.getVarType().className());
                backend.emitLW(A0, v.getLabel(), "Loading global variable " + stmt.name);
                if (v.getVarType().className().equals("int")) {
                    backend.emitJAL(makeint, "Boxing int");
                } else if (v.getVarType().className().equals("bool")) {
                    backend.emitJAL(makebool, "Boxing boolean");
                }*/
            } else if (temp instanceof FuncInfo) {
                FuncInfo f = (FuncInfo) temp;
                backend.emitLA(A0, f.getCodeLabel(), "Loading function " + stmt.name);
            } else if (temp instanceof ClassInfo) {
                //FIXME
                //ClassInfo c = (ClassInfo) temp;
                //backend.emitLA(A0, c.getCodeLabel(), "Loading class " + stmt.name);
            } else if (temp instanceof StackVarInfo) {
                //FIXME
                StackVarInfo v = (StackVarInfo) temp;
                //System.out.println("Variable name: " + v.getVarName() + "\nVariable type: " + v.getVarType().className()
                //+ "\nVariable initial value: " + v.getInitialValue());
                StmtAnalyzer sa = new StmtAnalyzer(v.getFuncInfo());
                v.getInitialValue().dispatch(sa);
            } else {
                //FIXME
            }
            return null;
        }

        @Override
        public Void analyze(IfExpr stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(IfStmt stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(IndexExpr stmt) {
            //FIXME
            return null;
        }

        @Override
        public Void analyze(IntegerLiteral stmt) {
            // Push the integer value onto the stack
            backend.emitLI(A0, stmt.value, "Pushing " + stmt.value + "literal onto stack");

            //Box the integer
            backend.emitJAL(makeint, "Boxing integer");
            return null;
        }

        @Override
        public Void analyze(NoneLiteral stmt) {
            //FIXME
            // Push the None literal onto the stack
            backend.emitLI(A0, 0, "Pushing None literal onto stack");
            return null;
        }

        @Override
        public Void analyze(Program stmt) {
            System.out.println("Here inside analyze program");
            //FIXME
            //Generate all declarations
            for (Declaration d : stmt.declarations) {
                d.dispatch(this);
            }
            //Generate all statements
            for (Stmt s : stmt.statements) {
                s.dispatch(this);
            }
            return null;
        }



        // FIXME: Example of statement.
        @Override
        public Void analyze(ReturnStmt stmt) {
            // FIXME: Here, we emit an instruction that does nothing. Clearly,
            // this is wrong, and you'll have to fix it.
            // This is here just to demonstrate how to emit a
            // RISC-V instruction.
            //backend.emitMV(ZERO, ZERO, "No-op");
            //return null;

            //Evaluate the expression?
            stmt.value.dispatch(this);
            return null;
        }

        @Override
        public Void analyze(StringLiteral stmt) {
            // Push the string value onto the stack
            backend.emitLA(A0, constants.getStrConstant(stmt.value), "Loading \"" + stmt.value + "\" literal onto stack");
            return null;
        }

        @Override
        public Void analyze(TypedVar stmt) {

            return null;
        }

        @Override
        public Void analyze(VarDef stmt) {
            //FIXME

            return null;
        }

        @Override
        public Void analyze(WhileStmt stmt) {
            //FIXME
            return null;
        }

        // FIXME: More, of course.

    }

    /**
     * Emits custom code in the CODE segment.
     *
     * This method is called after emitting the top level and the
     * function bodies for each function.
     *
     * You can use this method to emit anything you want outside of the
     * top level or functions, e.g. custom routines that you may want to
     * call from within your code to do common tasks. This is not strictly
     * needed. You might not modify this at all and still complete
     * the assignment.
     *
     * To start you off, here is an implementation of three routines that
     * will be commonly needed from within the code you will generate
     * for statements.
     *
     * The routines are error handlers for operations on None, index out
     * of bounds, and division by zero. They never return to their caller.
     * Just jump to one of these routines to throw an error and
     * exit the program. For example, to throw an OOB error:
     *   backend.emitJ(errorOob, "Go to out-of-bounds error and abort");
     *
     */
    protected void emitCustomCode() {
        emitErrorFunc(errorNone, "Operation on None");
        emitErrorFunc(errorDiv, "Division by zero");
        emitErrorFunc(errorOob, "Index out of bounds");
        emitBoxBoolean();
        emitBoxInteger();
    }

    /** Emit an error routine labeled ERRLABEL that aborts with message MSG. */
    private void emitErrorFunc(Label errLabel, String msg) {
        backend.emitGlobalLabel(errLabel);
        backend.emitLI(A0, ERROR_NONE, "Exit code for: " + msg);
        backend.emitLA(A1, constants.getStrConstant(msg),
                       "Load error message as str");
        backend.emitADDI(A1, A1, getAttrOffset(strClass, "__str__"),
                         "Load address of attribute __str__");
        backend.emitJ(abortLabel, "Abort");
    }

    final Label makebool = new Label("makebool");

    /** Emit a routine to box a boolean. */
    private void emitBoxBoolean() {
        backend.emitGlobalLabel(makebool);
        backend.emitSLLI(A0, A0, 4, "Shift boolean value into place");
        backend.emitLA(T0, constants.getBoolConstant(false), "Load address of False");
        backend.emitADD(A0, A0, T0, "Add False address to boolean value");
        backend.emitJR(RA, "Return from makebool");
    }

    final Label makeint = new Label("makeint");

    /** Emit a routine to box an integer. */
    private void emitBoxInteger() {
        backend.emitGlobalLabel(makeint);
        backend.emitADDI(SP, SP, -8, "Decrementing sp");
        backend.emitSW(RA, SP, 4, "Storing return address");
        backend.emitSW(A0, SP, 0, "Storing argument");
        backend.emitLA(A0, intClass.getPrototypeLabel(), "Load address of int prototype");
        backend.emitJAL(objectAllocLabel, "Allocate int object");
        backend.emitLW(T0, SP, 0, "Loading argument");
        backend.emitSW(T0, A0, "@.__int__", "Storing argument in int object");
        backend.emitLW(RA, SP, 4, "Loading return address");
        backend.emitADDI(SP, SP, 8, "Incrementing sp");
        backend.emitJR(RA, "Return from makeint");
    }
}

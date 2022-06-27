package main

import "fmt"
import "strconv"

// Simple imperative language

/*
vars       Variable names, start with lower-case letter

prog      ::= block
block     ::= "{" statement "}"
statement ::=  statement ";" statement           -- Command sequence
            |  vars ":=" exp                     -- Variable declaration
            |  vars "=" exp                      -- Variable assignment
            |  "while" exp block                 -- While
            |  "if" exp block "else" block       -- If-then-else
            |  "print" exp                       -- Print

exp ::= 0 | 1 | -1 | ...     -- Integers
     | "true" | "false"      -- Booleans
     | exp "+" exp           -- Addition
     | exp "*" exp           -- Multiplication
     | exp "||" exp          -- Disjunction
     | exp "&&" exp          -- Conjunction
     | "!" exp               -- Negation
     | exp "==" exp          -- Equality test
     | exp "<" exp           -- Lesser test
     | "(" exp ")"           -- Grouping of expressions
     | vars                  -- Variables
*/

// Values

type Kind int

const (
	ValueInt  Kind = 0
	ValueBool Kind = 1
	Undefined Kind = 2
)

type Val struct {
	flag Kind
	valI int
	valB bool
}

func mkInt(x int) Val {
	return Val{flag: ValueInt, valI: x}
}
func mkBool(x bool) Val {
	return Val{flag: ValueBool, valB: x}
}
func mkUndefined() Val {
	return Val{flag: Undefined}
}

func showVal(v Val) string {
	var s string
	switch v.flag {
	case ValueInt:
		s = Num(v.valI).pretty()
	case ValueBool:
		s = Bool(v.valB).pretty()
	case Undefined:
		s = "Undefined"
	}
	return s
}

// Types

type Type int

const (
	TyIllTyped Type = 0
	TyInt      Type = 1
	TyBool     Type = 2
)

func showType(t Type) string {
	var s string
	switch t {
	case TyInt:
		s = "Int"
	case TyBool:
		s = "Bool"
	case TyIllTyped:
		s = "Illtyped"
	}
	return s
}

// Value State is a mapping from variable names to values
type ValState map[string]Val

// Value State is a mapping from variable names to types
type TyState map[string]Type

// Interface

type Exp interface {
	pretty() string
	eval(s ValState) Val
	infer(t TyState) Type
}

type Stmt interface {
	pretty() string
	eval(s ValState)
	check(t TyState) bool
}

// Statement cases (incomplete)

type Seq [2]Stmt
type Decl struct {
	lhs string
	rhs Exp
}
type IfThenElse struct {
	cond      Exp
	thenBlock Block
	elseBlock Block
}

type While struct {
	cond Exp
	body Block
}

type Assign struct {
	lhs string
	rhs Exp
}

// Expression cases (incomplete)

type Block [1]Stmt
type Print [1]Exp
type Bool bool
type Num int
type Mult [2]Exp
type Plus [2]Exp
type Minus [2]Exp
type And [2]Exp
type Negation [1]Exp
type Lesser [2]Exp
type Equals [2]Exp
type Or [2]Exp
type Var string

/*type Context*/

/////////////////////////
// Stmt instances

// pretty print

// TODO: Vars, Decl, Context, Block, while, print, if & else, Lesser, Equal,

// Optional Todo for parser: Grouping!

func (stmt Seq) pretty() string {
	return stmt[0].pretty() + "; " + stmt[1].pretty()
}

func (decl Decl) pretty() string {
	return decl.lhs + " := " + decl.rhs.pretty()
}

func (a Assign) pretty() string {
	return a.lhs + " = " + a.rhs.pretty()
}

func (ite IfThenElse) pretty() string {
	var x string
	x = "if "
	x += ite.cond.pretty()
	x += ite.thenBlock.pretty()
	x += "else "
	x += ite.elseBlock.pretty()
	return x
}

func (while While) pretty() string {
	var x string
	x = "while "
	x += while.cond.pretty()
	x += while.body.pretty()
	return x
}

func (print Print) pretty() string {
	var x string
	x = "print "
	x += print[0].pretty()
	return x
}

// eval

func (stmt Seq) eval(s ValState) {
	stmt[0].eval(s)
	stmt[1].eval(s)
}

func (ite IfThenElse) eval(s ValState) {
	v := ite.cond.eval(s)
	if v.flag == ValueBool {
		if v.valB {
			ite.thenBlock.eval(s)
		} else {
			ite.elseBlock.eval(s)
		}
	} else {
		fmt.Printf("if-then-else eval fail")
	}
}

func (while While) eval(s ValState) {
	for while.cond.eval(s).valB {
		while.body.eval(s)
	}
}

// Maps are represented via points.
// Hence, maps are passed by "reference" and the update is visible for the caller as well.
func (decl Decl) eval(s ValState) {
	v := decl.rhs.eval(s)
	x := (string)(decl.lhs)
	s[x] = v
}

func (a Assign) eval(s ValState) {
	v := a.rhs.eval(s)
	x := (string)(a.lhs)
	sVal := s[x]
	if sVal.flag == v.flag {
		s[x] = v
	} else {
		s[x] = mkUndefined()
	}
}

func (x Var) eval(s ValState) Val {
	y := (string)(x)
	vy, ok := s[y]
	if ok {
		switch vy.flag {
		case ValueInt:
			return mkInt(vy.valI)
		case ValueBool:
			return mkBool(vy.valB)
		case Undefined:
			return mkUndefined()
		}
	}
	return mkUndefined()
}

// type check

func (stmt Seq) check(t TyState) bool {
	if !stmt[0].check(t) {
		return false
	}
	return stmt[1].check(t)
}

func (print Print) check(t TyState) bool {
	return true
}

func (decl Decl) check(t TyState) bool {
	ty := decl.rhs.infer(t)
	if ty == TyIllTyped {
		return false
	}

	x := (string)(decl.lhs)
	t[x] = ty
	return true
}

func (ite IfThenElse) check(t TyState) bool {
	return ite.cond.infer(t) == TyBool && ite.thenBlock.check(t) && ite.elseBlock.check(t)
}

func (while While) check(t TyState) bool {
	return while.cond.infer(t) == TyBool && while.body.check(t)
}

func (a Assign) check(t TyState) bool {
	x := (string)(a.lhs)
	return t[x] == a.rhs.infer(t)
}

func (b Block) check(t TyState) bool {
	tInner := make(map[string]Type)
	for key, value := range t {
		tInner[key] = value
	}
	return b[0].check(tInner)
}

/////////////////////////
// Exp instances

// pretty print

func (b Block) pretty() string {
	var x string
	x = "{\n"
	x += b[0].pretty()
	x += "\n}"
	return x
}

func (x Var) pretty() string {
	return (string)(x)
}

func (x Bool) pretty() string {
	if x {
		return "true"
	} else {
		return "false"
	}

}

func (x Num) pretty() string {
	return strconv.Itoa(int(x))
}

func (e Mult) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "*"
	x += e[1].pretty()
	x += ")"

	return x
}

func (e Minus) pretty() string {
	var x string
	x = "("
	x += e[0].pretty()
	x += "-"
	x += e[1].pretty()
	x += ")"

	return x
}

func (e Plus) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "+"
	x += e[1].pretty()
	x += ")"

	return x
}

func (e And) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "&&"
	x += e[1].pretty()
	x += ")"

	return x
}

func (e Negation) pretty() string {

	var x string
	x = "(!"
	x += e[0].pretty()
	x += ")"

	return x
}

func (e Lesser) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "<"
	x += e[1].pretty()
	x += ")"

	return x
}

func (e Equals) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "=="
	x += e[1].pretty()
	x += ")"

	return x
}

func (e Or) pretty() string {

	var x string
	x = "("
	x += e[0].pretty()
	x += "||"
	x += e[1].pretty()
	x += ")"

	return x
}

// Evaluator

func (b Block) eval(s ValState) {
	sInner := make(map[string]Val)
	for key, value := range s {
		sInner[key] = value
	}
	b[0].eval(sInner)

	for key, value := range s {
		if sInner[key].flag == value.flag {
			s[key] = sInner[key]
		}
	}
}

func (print Print) eval(s ValState) {
	n := print[0].eval(s)
	fmt.Printf("\n%s", showVal(n))
}

func (x Bool) eval(s ValState) Val {
	return mkBool((bool)(x))
}

func (x Num) eval(s ValState) Val {
	return mkInt((int)(x))
}

func (e Mult) eval(s ValState) Val {
	n1 := e[0].eval(s)
	n2 := e[1].eval(s)
	if n1.flag == ValueInt && n2.flag == ValueInt {
		return mkInt(n1.valI * n2.valI)
	}
	return mkUndefined()
}

func (e Plus) eval(s ValState) Val {
	n1 := e[0].eval(s)
	n2 := e[1].eval(s)
	if n1.flag == ValueInt && n2.flag == ValueInt {
		return mkInt(n1.valI + n2.valI)
	}
	return mkUndefined()
}

func (e Minus) eval(s ValState) Val {
	n1 := e[0].eval(s)
	n2 := e[1].eval(s)
	if n1.flag == ValueInt && n2.flag == ValueInt {
		return mkInt(n1.valI - n2.valI)
	}
	return mkUndefined()
}

func (e Negation) eval(s ValState) Val {
	b1 := e[0].eval(s)
	if b1.flag == ValueBool {
		return mkBool(!b1.valB)
	}
	return mkUndefined()
}

func (e Lesser) eval(s ValState) Val {
	b1 := e[0].eval(s)
	b2 := e[1].eval(s)
	if b1.flag == ValueInt && b2.flag == ValueInt {
		return mkBool(b1.valI < b2.valI)
	}
	return mkUndefined()
}

func (e Equals) eval(s ValState) Val {
	b1 := e[0].eval(s)
	b2 := e[1].eval(s)
	switch {
	case b1.flag == ValueInt && b2.flag == ValueInt:
		return mkBool(b1.valI == b2.valI)
	case b1.flag == ValueBool && b2.flag == ValueBool:
		return mkBool(b1.valB == b2.valB)
	}
	return mkUndefined()
}

func (e And) eval(s ValState) Val {
	b1 := e[0].eval(s)
	b2 := e[1].eval(s)
	switch {
	case b1.flag == ValueBool && b1.valB == false:
		return mkBool(false)
	case b1.flag == ValueBool && b2.flag == ValueBool:
		return mkBool(b1.valB && b2.valB)
	}
	return mkUndefined()
}

func (e Or) eval(s ValState) Val {
	b1 := e[0].eval(s)
	b2 := e[1].eval(s)
	switch {
	case b1.flag == ValueBool && b1.valB == true:
		return mkBool(true)
	case b1.flag == ValueBool && b2.flag == ValueBool:
		return mkBool(b1.valB || b2.valB)
	}
	return mkUndefined()
}

// Type inferencer/checker

func (x Var) infer(t TyState) Type {
	y := (string)(x)
	ty, ok := t[y]
	if ok {
		return ty
	} else {
		return TyIllTyped // variable does not exist yields illtyped
	}

}

func (x Bool) infer(t TyState) Type {
	return TyBool
}

func (x Num) infer(t TyState) Type {
	return TyInt
}

func (e Mult) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyInt && t2 == TyInt {
		return TyInt
	}
	return TyIllTyped
}

func (e Plus) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyInt && t2 == TyInt {
		return TyInt
	}
	return TyIllTyped
}

func (e Minus) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyInt && t2 == TyInt {
		return TyInt
	}
	return TyIllTyped
}

func (e And) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyBool && t2 == TyBool {
		return TyBool
	}
	return TyIllTyped
}

func (e Negation) infer(t TyState) Type {
	t1 := e[0].infer(t)
	if t1 == TyBool {
		return TyBool
	}
	return TyIllTyped
}

func (e Equals) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyBool && t2 == TyBool || t1 == TyInt && t2 == TyInt {
		return TyBool
	}
	return TyIllTyped
}

func (e Lesser) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyInt && t2 == TyInt {
		return TyBool
	}
	return TyIllTyped
}

func (e Or) infer(t TyState) Type {
	t1 := e[0].infer(t)
	t2 := e[1].infer(t)
	if t1 == TyBool && t2 == TyBool {
		return TyBool
	}
	return TyIllTyped
}

// Helper functions to build ASTs by hand

func number(x int) Exp {
	return Num(x)
}

func boolean(x bool) Exp {
	return Bool(x)
}

func plus(x, y Exp) Exp {
	return (Plus)([2]Exp{x, y})

	// The type Plus is defined as the two element array consisting of Exp elements.
	// Plus and [2]Exp are isomorphic but different types.
	// We first build the AST value [2]Exp{x,y}.
	// Then cast this value (of type [2]Exp) into a value of type Plus.

}

func minus(x, y Exp) Exp {
	return (Minus)([2]Exp{x, y})
}

func mult(x, y Exp) Exp {
	return (Mult)([2]Exp{x, y})
}

func and(x, y Exp) Exp {
	return (And)([2]Exp{x, y})
}

func or(x, y Exp) Exp {
	return (Or)([2]Exp{x, y})
}

func negate(x Exp) Exp {
	return (Negation)([1]Exp{x})
}

func less(x, y Exp) Exp {
	return (Lesser)([2]Exp{x, y})
}

func equal(x, y Exp) Exp {
	return (Equals)([2]Exp{x, y})
}

func assign(x string, y Exp) Stmt {
	assignment := Assign{
		lhs: x,
		rhs: y,
	}
	return assignment
}

func print(x Exp) Stmt {
	return (Print)([1]Exp{x})
}

func declare(x string, y Exp) Stmt {
	declaration := Decl{
		lhs: x,
		rhs: y,
	}
	return declaration
}

func block(x Stmt) Block {
	return (Block)([1]Stmt{x})
}

func ifThenElse(x Exp, y, z Block) Stmt {
	ifThenElse := IfThenElse{
		cond:      x,
		thenBlock: y,
		elseBlock: z,
	}
	return ifThenElse
}

func while(x Exp, y Block) Stmt {
	while := While{
		cond: x,
		body: y,
	}
	return while
}

func seq(x, y Stmt) Stmt {
	return (Seq)([2]Stmt{x, y})
}

func vars(x string) Exp {
	return Var(x)
}

// Examples

func run(e Exp) {
	s := make(map[string]Val)
	t := make(map[string]Type)
	fmt.Printf("\n ******* ")
	fmt.Printf("\n %s", e.pretty())
	fmt.Printf("\n %s", showVal(e.eval(s)))
	fmt.Printf("\n %s", showType(e.infer(t)))
}

func printProg(block Stmt) {
	t := make(map[string]Type)
	fmt.Printf("\n******* ")
	fmt.Printf("\n%t", block.check(t))
	fmt.Printf("\n%s", block.pretty())
}

func runProg(block Stmt) {
	s := make(map[string]Val)
	t := make(map[string]Type)
	fmt.Printf("\n******* ")
	if block.check(t) {
		block.eval(s)
	} else {
		fmt.Printf("Code Error, please check your code")
	}
}

func ex1() {
	ast := plus(mult(number(1), number(2)), number(0))

	run(ast)
}

func ex2() {
	ast := and(boolean(false), number(0))
	run(ast)
}

func ex3() {
	ast := or(boolean(false), number(0))
	run(ast)
}

func ex4() {
	ast := minus(mult(number(1), number(2)), number(5))

	run(ast)
}

func ex5() {
	ast := negate(or(boolean(true), boolean(true)))
	run(ast)
}

func ex6() {
	ast := negate(less(number(3), number(20)))
	run(ast)
}

func ex7() {
	prog := block(block(seq(declare("x", boolean(true)), assign("x", negate(less(number(3), number(20)))))))
	runProg(prog)
}

func ex8() {
	prog := block(print(vars("x2")))
	runProg(prog)
}

func ex9() {
	prog := block(seq(declare("x", number(1)), while(less(vars("x"), number(10)), block(seq(assign("x", plus(vars("x"), number(1))), print(vars("x")))))))
	//	//
	runProg(prog)
}

func ex10() {
	// this checks ifThenElse and to see whether re-declaring in an inner block will overwrite outer variables or not
	prog := block(seq(declare("x", number(1)), seq(ifThenElse(equal(vars("x"), number(1)), block(declare("x", boolean(true))), block(assign("x", number(3)))),
		print(vars("x")))))
	// this checks ifThenElse for the else block
	prog2 := block(seq(declare("x", number(1)), seq(ifThenElse(equal(vars("x"), number(2)), block(declare("x", boolean(true))), block(assign("x", number(3)))),
		print(vars("x")))))
	runProg(prog)
	runProg(prog2)
}

func ex11() {
	// checks whether shadowing works without overwriting the outer-scope type
	prog := block(seq(declare("x", number(1)), seq(block(seq(declare("x", boolean(true)), print(vars("x")))), print(vars("x")))))
	runProg(prog)
}

func ex12() {
	// checks that inner-block value changes get applied in the outer scope if the type is the same
	prog := block(seq(declare("x", number(1)), seq(block(seq(declare("x", number(2)), print(vars("x")))), print(vars("x")))))
	runProg(prog)
}

func main() {

	fmt.Printf("\n")

	ex12()
}

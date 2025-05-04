use std::collections::HashMap;
use std::env;
use std::fs;
use std::iter::Peekable;
use std::slice::Iter;

/// Represents lexical tokens in a minimal C-like language.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Int,
    Return,
    Identifier(String),
    Number(i64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Plus,
    Star,
    Minus,
    Divide,
    Mod,
    Equal,
    Less,
    Greater,
    If,
    Else,
    While,
    Assign,
    Comma,
    Unknown(char),
}

/// Tokenizer that converts source code into a list of tokens.
pub fn tokenizer(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\n' | '\r' | '\t' => {
                chars.next();
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '{' => {
                chars.next();
                tokens.push(Token::LBrace);
            }
            '}' => {
                chars.next();
                tokens.push(Token::RBrace);
            }
            ';' => {
                chars.next();
                tokens.push(Token::Semicolon);
            }
            '0'..='9' => {
                let mut num = 0;
                while let Some(c) = chars.peek() {
                    if c.is_digit(10) {
                        num = num * 10 + c.to_digit(10).unwrap() as i64;
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num));
            }
            '+' => {
                chars.next();
                tokens.push(Token::Plus);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Star);
            }
            '-' => {
                chars.next();
                tokens.push(Token::Minus);
            }
            '/' => {
                chars.next();
                tokens.push(Token::Divide);
            }
            '%' => {
                chars.next();
                tokens.push(Token::Mod);
            }
            '=' => {
                chars.next();
                if let Some('=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Equal); // '=='
                } else {
                    tokens.push(Token::Assign); // '='
                }
            }
            '<' => {
                chars.next();
                tokens.push(Token::Less);
            }
            '>' => {
                chars.next();
                tokens.push(Token::Greater);
            }
            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(c) = chars.peek() {
                    if c.is_alphanumeric() || *c == '_' {
                        ident.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                match ident.as_str() {
                    "int" => tokens.push(Token::Int),
                    "return"
                }
            }
        }
    }
}
/// Parses tokens into an AST for a basic C-like function, assuming int main(...) { ... } format.
pub fn parse_token(tokens: &[Token]) -> ASTNode {
    let mut iter = tokens.iter().peekable();

    // Parsing for: int main(...) {
    match (iter.next(), iter.next(), iter.next()) {
        (Some(Token::Int), Some(Token::Identifier(_)), Some(Token::LParen)) => {
            while let Some(token) = iter.next() {
                if *token == Token::LBrace {
                    break;
                }
            }
        }
        _ => panic!("Syntax error in function declaration"),
    }

    let mut statements = Vec::new();

    while let Some(token) = iter.peek() {
        match token {
            Token::Return | Token::If | Token::While | Token::LBrace | Token::Int | Token::Identifier(_) => {
                statements.push(parse_stmt(&mut iter));
            }
            Token::RBrace => {
                iter.next();
                break;
            }
            _ => panic!("Unexpected token inside block: {:?}", token),
        }
    }

    ASTNode::Sequence(statements)
}

// Parses a variable declaration
fn parse_declaration(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    let name = match iter.next() {
        Some(Token::Identifier(name)) => name.clone(),
        _ => panic!("Expected variable name"),
    };

    ex_token(iter, Token::Assign);
    let expr = parser_expression(iter);
    ex_token(iter, Token::Semicolon);

    ASTNode::Declaration(name, expr)
}

// Parses an assignment statement
fn parser_state(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    let name = match iter.next() {
        Some(Token::Identifier(name)) => name.clone(),
        _ => panic!("Expected variable"),
    };

    ex_token(iter, Token::Assign);
    let expr = parser_expression(iter);
    ex_token(iter, Token::Semicolon);

    ASTNode::Assignment(name, expr)
}

// Parses an individual statement
fn parse_stmt(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    match iter.peek() {
        Some(Token::Return) => {
            iter.next();
            let expr = parser_expression(iter);
            ex_token(iter, Token::Semicolon);
            ASTNode::Return(expr)
        }
        Some(Token::If) => {
            iter.next();
            parser_if_state(iter)
        }
        Some(Token::LBrace) => {
            parser_block(iter)
        }
        Some(Token::While) => {
            iter.next();
            parser_while_loop(iter)
        }
        Some(Token::Int) => {
            iter.next();
            parse_declaration(iter)
        }
        Some(Token::Identifier(_)) => parser_state(iter),
        _ => panic!("Expected condition"),
    }
}

// Parses a while loop
fn parser_while_loop(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    ex_token(iter, Token::LParen);
    let condition = parser_expression(iter);
    ex_token(iter, Token::RParen);

    let body = parse_stmt(iter);
    ASTNode::While {
        condition,
        body: Box::new(body),
    }
}

// Parses a block of statements
fn parser_block(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    ex_token(iter, Token::LBrace);
    let mut stmts = Vec::new();

    while let Some(token) = iter.peek() {
        match token {
            Token::RBrace => {
                iter.next();
                break;
            }
            Token::Return | Token::If | Token::While | Token::LBrace => {
                stmts.push(parse_stmt(iter));
            }
            t => panic!("Unexpected token inside block: {:?}", t),
        }
    }

    ASTNode::Sequence(stmts)
}

// Parses an if statement
fn parser_if_state(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    ex_token(iter, Token::LParen);
    let condition = parser_expression(iter);
    ex_token(iter, Token::RParen);

    let then_branch = parse_stmt(iter);

    let else_branch = if let Some(Token::Else) = iter.peek() {
        iter.next();
        Some(Box::new(parse_stmt(iter)))
    } else {
        None
    };

    ASTNode::If {
        condition,
        then_branch: Box::new(then_branch),
        else_branch,
    }
}

// Ensures the next token matches the expected token
fn ex_token(iter: &mut Peekable<Iter<Token>>, expected: Token) {
    match iter.next() {
        Some(t) if *t == expected => {}
        other => panic!("Expected {:?}, got {:?}", expected, other),
    }
}

// Parses an expression
fn parser_expression(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    parser_difference(iter)
}

// Parses a comparison expression
fn parser_difference(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    let mut left = parse_add_sub(iter);

    while let Some(token) = iter.peek() {
        match token {
            Token::Equal => {
                iter.next();
                let right = parse_add_sub(iter);
                left = Box::new(Expr::Equal(left, right));
            }
            Token::Less => {
                iter.next();
                let right = parse_add_sub(iter);
                left = Box::new(Expr::Less(left, right));
            }
            Token::Greater => {
                iter.next();
                let right = parse_add_sub(iter);
                left = Box::new(Expr::Greater(left, right));
            }
            _ => break,
        }
    }

    left
}

// Parses an addition or subtraction expression
fn parse_add_sub(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    let mut left = parse_mul_div(iter);

    while let Some(token) = iter.peek() {
        match token {
            Token::Plus => {
                iter.next();
                let right = parse_mul_div(iter);
                left = Box::new(Expr::Add(left, right));
            }
            Token::Minus => {
                iter.next();
                let right = parse_mul_div(iter);
                left = Box::new(Expr::Sub(left, right));
            }
            _ => break,
        }
    }

    left
}
// Parses a multiplication or division expression
fn parse_mul_div(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    let mut left = parse_primary(iter);

    while let Some(token) = iter.peek() {
        match token {
            Token::Star => {
                iter.next();
                let right = parse_primary(iter);
                left = Box::new(Expr::Mul(left, right));
            }
            Token::Divide => {
                iter.next();
                let right = parse_primary(iter);
                left = Box::new(Expr::Div(left, right));
            }
            Token::Mod => {
                iter.next();
                let right = parse_primary(iter);
                left = Box::new(Expr::Mod(left, right));
            }
            _ => break,
        }
    }

    left
}

// Parses a primary expression
fn parse_primary(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    match iter.next() {
        Some(Token::Number(n)) => Box::new(Expr::Number(*n)),
        Some(Token::Identifier(name)) => {
            let name = name.clone();
            if let Some(Token::LParen) = iter.peek() {
                iter.next();
                let mut args = Vec::new();
                while let Some(token) = iter.peek() {
                    if let Token::RParen = token {
                        break;
                    }
                    let arg = parser_expression(iter);
                    args.push(*arg);
                    if let Some(Token::Comma) = iter.peek() {
                        iter.next();
                    } else {
                        break;
                    }
                }
                ex_token(iter, Token::RParen);
                Box::new(Expr::Call(name, args))
            } else {
                Box::new(Expr::Var(name))
            }
        }
        Some(Token::LParen) => {
            let expr = parser_expression(iter);
            match iter.next() {
                Some(Token::RParen) => expr,
                _ => panic!("Expected closing parenthesis"),
            }
        }
        other => panic!("Expected number, variable, or '(', got {:?}", other),
    }
}

/// Enum representing the virtual machine's instruction set for executing compiled code.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instruction {
    IMM(i64),
    PSH,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    JMP(usize),
    BZ(usize),
    BNZ(usize),
    JSR(usize),
    ENT(usize),
    ADJ(usize),
    LEV,
    LEA(usize),
    LI,
    LC,
    SI,
    SC,
    EXIT,
    PRTF,
    MALC,
    FREE,
    MSET,
    MCMP,
    OPEN,
    READ,
    CLOS,
    EQ,
    LT,
    GT,
}
// Stack-based virtual machine
pub struct VM {
    pub stack: Vec<i64>,
    pub pc: usize,
    pub bp: usize,
    pub program: Vec<Instruction>,
    pub running: bool,
}

/// A stack-based virtual machine for executing C4-style bytecode instructions.
impl VM {
    pub fn new(program: Vec<Instruction>) -> Self {
        VM {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            program,
            running: true,
        }
    }

    pub fn run(&mut self) {
        while self.running {
            if self.pc >= self.program.len() {
                panic!("Program counter out of bounds");
            }

            match self.program[self.pc] {
                Instruction::IMM(val) => {
                    self.stack.push(val);
                }
                Instruction::PSH => {
                    if let Some(&top) = self.stack.last() {
                        self.stack.push(top);
                    } else {
                        panic!("PSH failed: stack is empty");
                    }
                }
                Instruction::ADD => {
                    let b = self.stack.pop().expect("ADD: missing operand B");
                    let a = self.stack.pop().expect("ADD: missing operand A");
                    self.stack.push(a + b);
                }
                Instruction::SUB => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }
                Instruction::MUL => {
                    let b = self.stack.pop().expect("MUL: missing operand B");
                    let a = self.stack.pop().expect("MUL: missing operand A");
                    self.stack.push(a * b);
                }
                Instruction::DIV => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a / b);
                }
                Instruction::MOD => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a % b);
                }
                Instruction::JMP(target) => {
                    self.pc = target;
                    continue;
                }
                Instruction::BZ(target) => {
                    let cond = self.stack.pop().unwrap();
                    if cond == 0 {
                        self.pc = target;
                        continue;
                    }
                }
                Instruction::BNZ(target) => {
                    let cond = self.stack.pop().unwrap();
                    if cond != 0 {
                        self.pc = target;
                        continue;
                    }
                }
                Instruction::JSR(target) => {
                    self.stack.push((self.pc + 1) as i64);
                    self.pc = target;
                    continue;
                }
                Instruction::ENT(size) => {
                    self.stack.push(self.bp as i64);
                    self.bp = self.stack.len();
                    self.stack.resize(self.stack.len() + size, 0);
                }
                Instruction::ADJ(n) => {
                    for _ in 0..n {
                        self.stack.pop();
                    }
                }
                Instruction::LEV => {
                    let old_bp = self.stack[self.bp - 1];
                    self.stack.truncate(self.bp - 1);
                    self.bp = old_bp as usize;
                    self.pc = self.stack.pop().unwrap() as usize;
                    continue;
                }
                Instruction::LEA(offset) => {
                    let addr = self.bp + offset;
                    self.stack.push(addr as i64);
                }
                Instruction::LI => {
                    let addr = self.stack.pop().unwrap() as usize;
                    let val = self.stack[addr];
                    self.stack.push(val);
                }
                Instruction::LC => {
                    let addr = self.stack.pop().unwrap() as usize;
                    let val = self.stack[addr] & 0xFF;
                    self.stack.push(val);
                }
                Instruction::SI => {
                    let val = self.stack.pop().unwrap();
                    let addr = self.stack.pop().unwrap() as usize;
                    self.stack[addr] = val;
                }
                Instruction::SC => {
                    let val = self.stack.pop().unwrap() & 0xFF;
                    let addr = self.stack.pop().unwrap() as usize;
                    self.stack[addr] = val;
                }
                Instruction::EXIT => {
                    if let Some(&result) = self.stack.last() {
                        println!("output {}", result);
                    } else {
                        println!("Program got exit without a return value");
                    }
                    self.running = false;
                }
                Instruction::PRTF => {
                    let _arg_count = self.stack.pop().unwrap();
                    let _fmt_addr = self.stack.pop().unwrap();
                    println!("[PRTF] Simulated printf");
                    self.stack.push(0);
                }
                Instruction::MALC => {
                    self.stack.push(0x1000);
                }
                Instruction::FREE => {
                    let _ = self.stack.pop();
                }
                Instruction::MSET => {
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                }
                Instruction::MCMP => {
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    self.stack.push(0);
                }
                Instruction::OPEN => {
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    self.stack.push(3);
                }
                Instruction::READ => {
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    let _ = self.stack.pop();
                    self.stack.push(10);
                }
                Instruction::CLOS => {
                    let _ = self.stack.pop();
                    self.stack.push(0);
                }
                Instruction::EQ => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push((a == b) as i64);
                }
                Instruction::LT => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push((a < b) as i64);
                }
                Instruction::GT => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push((a > b) as i64);
                }
            }
            self.pc += 1;
        }
    }
}

// Generate VM instructions from parsed AST
pub fn get_vm(ast: &ASTNode) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    let mut symbol_table = HashMap::new();
    let mut next_offset = 0;
    let mut patches: Vec<(usize, String)> = Vec::new();

    instructions.push(Instruction::ENT(0));

    get_vm_inner(ast, &mut instructions, &mut symbol_table, &mut next_offset, &mut patches);

    let mut function_addresses = HashMap::new();
    if let ASTNode::Sequence(stmts) = ast {
        for (i, stmt) in stmts.iter().enumerate() {
            if let ASTNode::FunctionDef { name, .. } = stmt {
                function_addresses.insert(name.clone(), i);
            }
        }
    }
for (index, func_name) in patches {
    if let Some(&addr) = function_addresses.get(&func_name) {
        instructions[index] = Instruction::JSR(addr);
    } else {
        panic!("Unresolved function call: {}", func_name);
    }
}

instructions[0] = Instruction::ENT(next_offset);

instructions
}

// Recursively generates instructions from the AST
fn get_vm_inner(
    ast: &ASTNode,
    instructions: &mut Vec<Instruction>,
    symbol_table: &mut HashMap<String, usize>,
    next_offset: &mut usize,
    patches: &mut Vec<(usize, String)>,
) {
    match ast {
        // Handles: return <expr>;
        ASTNode::Return(expr) => {
            emit_expr(expr, instructions, symbol_table, patches);
            instructions.push(Instruction::PSH);
            instructions.push(Instruction::EXIT);
        }
        // Handles: if (<condition>) { ... } else { ... }
        ASTNode::If { condition, then_branch, else_branch } => {
            emit_expr(condition, instructions, symbol_table, patches);
            let jump_false_index = instructions.len();
            instructions.push(Instruction::BZ(9999)); // to patch
            get_vm_inner(then_branch, instructions, symbol_table, next_offset, patches);
            if let Some(else_branch) = else_branch {
                let jump_over_else_index = instructions.len();
                instructions.push(Instruction::JMP(9999));
                let else_start = instructions.len();
                get_vm_inner(else_branch, instructions, symbol_table, next_offset, patches);
                let after_else = instructions.len();
                instructions[jump_false_index] = Instruction::BZ(else_start);
                instructions[jump_over_else_index] = Instruction::JMP(after_else);
            } else {
                let after_then = instructions.len();
                instructions[jump_false_index] = Instruction::BZ(after_then);
            }
        }
        // Handles: while (<condition>) { ... }
        ASTNode::While { condition, body } => {
            let loop_start = instructions.len();
            emit_expr(condition, instructions, symbol_table, patches);
            let jump_if_false_index = instructions.len();
            instructions.push(Instruction::BZ(9999));
            get_vm_inner(body, instructions, symbol_table, next_offset, patches);
            instructions.push(Instruction::JMP(loop_start));
            let loop_end = instructions.len();
            instructions[jump_if_false_index] = Instruction::BZ(loop_end);
        }
        // Handles: { stmt1; stmt2; ... }
        ASTNode::Sequence(statements) => {
            for stmt in statements {
                get_vm_inner(stmt, instructions, symbol_table, next_offset, patches);
            }
        }
        // Handles: int x = <expr>;
        ASTNode::Declaration(name, expr) => {
            let offset = *next_offset;
            *next_offset += 1;
            symbol_table.insert(name.clone(), offset);
            instructions.push(Instruction::LEA(offset));
            emit_expr(expr, instructions, symbol_table, patches);
            instructions.push(Instruction::SI);
        }
        // Handles: x = <expr>;
        ASTNode::Assignment(name, expr) => {
            if let Some(&offset) = symbol_table.get(name) {
                instructions.push(Instruction::LEA(offset));
                emit_expr(expr, instructions, symbol_table, patches);
                instructions.push(Instruction::SI);
            } else {
                panic!("Assignment to undeclared variable: {}", name);
            }
        }
        // Handles: int foo(...) { ... }
        ASTNode::FunctionDef { name: _, params, body } => {
            symbol_table.clear();
            *next_offset = params.len();
            for (i, param) in params.iter().enumerate() {
                symbol_table.insert(param.clone(), i);
            }
            get_vm_inner(body, instructions, symbol_table, next_offset, patches);
        }
    }
}
// Translates a high-level expression (AST) into low-level virtual machine instructions.
fn emit_expr(
    expr: &Expr,
    instructions: &mut Vec<Instruction>,
    symbol_table: &HashMap<String, usize>,
    patches: &mut Vec<(usize, String)>,
) {
    match expr {
        Expr::Number(n) => {
            instructions.push(Instruction::IMM(*n));
        }
        Expr::Add(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::ADD);
        }
        Expr::Sub(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::SUB);
        }
        Expr::Mul(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::MUL);
        }
        Expr::Div(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::DIV);
        }
        Expr::Mod(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::MOD);
        }
        Expr::Equal(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::EQ);
        }
        Expr::Less(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::LT);
        }
        Expr::Greater(lhs, rhs) => {
            emit_expr(lhs, instructions, symbol_table, patches);
            emit_expr(rhs, instructions, symbol_table, patches);
            instructions.push(Instruction::GT);
        }
        Expr::Variable(name) | Expr::Var(name) => {
            if let Some(&offset) = symbol_table.get(name) {
                instructions.push(Instruction::LEA(offset));
                instructions.push(Instruction::LI);
            } else {
                panic!("Use of undeclared variable: {}", name);
            }
        }
        Expr::Call(func_name, args) => {
            for arg in args {
                emit_expr(arg, instructions, symbol_table, patches);
            }
            let placeholder_index = instructions.len();
            instructions.push(Instruction::JSR(9999)); // Patch later
            patches.push((placeholder_index, func_name.clone()));
        }
    }
}

// Entry point for the compiler and virtual machine.
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input.c>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let source = fs::read_to_string(filename).expect("Failed to read source file");

    let tokens = tokenizer(&source);
    let ast = parse_token(&tokens);
    let program = get_vm(&ast);
    let mut vm = VM::new(program);
    vm.run();
}
// Tests for the compiler
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_basic_function() {
        let src = "int main() { return 42; }";
        let tokens = tokenizer(src);
        assert_eq!(
            tokens,
            vec![
                Token::Int,
                Token::Identifier("main".into()),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::Return,
                Token::Number(42),
                Token::Semicolon,
                Token::RBrace
            ]
        );
    }

    #[test]
    fn virtual_machine_addition() {
        let prog = vec![
            Instruction::IMM(4),
            Instruction::IMM(6),
            Instruction::ADD,
            Instruction::EXIT,
        ];
        let mut vm = VM::new(prog);
        vm.run();
        assert_eq!(vm.stack, vec![10]);
    }

    #[test]
    fn vm_branch_on_zero_skips_add() {
        let prog = vec![
            Instruction::IMM(0),
            Instruction::BZ(5),
            Instruction::IMM(100),
            Instruction::IMM(200),
            Instruction::ADD,
            Instruction::IMM(9),
            Instruction::EXIT,
        ];
        let mut vm = VM::new(prog);
        vm.run();
        assert_eq!(vm.stack, vec![9]);
    }

    #[test]
    fn vm_branch_on_nonzero_executes_add() {
        let prog = vec![
            Instruction::IMM(1),
            Instruction::BNZ(5),
            Instruction::IMM(10),
            Instruction::IMM(20),
            Instruction::ADD,
            Instruction::IMM(7),
            Instruction::EXIT,
        ];
        let mut vm = VM::new(prog);
        vm.run();
        assert_eq!(vm.stack, vec![7]);
    }

    #[test]
    fn vm_simple_function_call() {
        let prog = vec![
            Instruction::JSR(4),
            Instruction::IMM(123),
            Instruction::PSH,
            Instruction::EXIT,
            Instruction::ENT(0),
            Instruction::LEV,
        ];
        let mut vm = VM::new(prog);
        vm.run();
        assert_eq!(vm.stack.last(), Some(&123));
    }

    #[test]
    fn vm_memory_store_and_load() {
        let prog = vec![
            Instruction::ENT(1),
            Instruction::LEA(0),
            Instruction::IMM(888),
            Instruction::SI,
            Instruction::LEA(0),
            Instruction::LI,
            Instruction::EXIT,
        ];
        let mut vm = VM::new(prog);
        vm.run();
        assert_eq!(vm.stack.last(), Some(&888));
    }
}
#[test]
fn parse_return_sum_expression() {
    let tokens = tokenizer("int main() { return 8 + 9; }");
    let ast = parse_token(&tokens);
    assert_eq!(
        ast,
        ASTNode::Sequence(vec![ASTNode::Return(Box::new(Expr::Add(
            Box::new(Expr::Number(8)),
            Box::new(Expr::Number(9)),
        )))]),
    );
}

#[test]
fn parse_nested_expression_with_multiplication() {
    let tokens = tokenizer("int main() { return (2 + 3) * 4; }");
    let ast = parse_token(&tokens);
    assert_eq!(
        ast,
        ASTNode::Sequence(vec![ASTNode::Return(Box::new(Expr::Mul(
            Box::new(Expr::Add(
                Box::new(Expr::Number(2)),
                Box::new(Expr::Number(3))
            )),
            Box::new(Expr::Number(4))
        )))]),
    );
}

#[test]
fn parse_if_else_flow() {
    let src = "int main() { if (3 < 4) { return 1; } else { return 0; } }";
    let tokens = tokenizer(src);
    let ast = parse_token(&tokens);
    assert_eq!(
        ast,
        ASTNode::Sequence(vec![ASTNode::If {
            condition: Box::new(Expr::Less(
                Box::new(Expr::Number(3)),
                Box::new(Expr::Number(4))
            )),
            then_branch: Box::new(ASTNode::Sequence(vec![
                ASTNode::Return(Box::new(Expr::Number(1)))
            ])),
            else_branch: Some(Box::new(ASTNode::Sequence(vec![
                ASTNode::Return(Box::new(Expr::Number(0)))
            ])))
        }]),
    );
}

#[test]
fn parse_while_loop_execution() {
    let src = "int main() { while (0 < 1) { return 10; } }";
    let tokens = tokenizer(src);
    let ast = parse_token(&tokens);
    assert_eq!(
        ast,
        ASTNode::Sequence(vec![ASTNode::While {
            condition: Box::new(Expr::Less(
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(1))
            )),
            body: Box::new(ASTNode::Sequence(vec![
                ASTNode::Return(Box::new(Expr::Number(10)))
            ]))
        }])
    );
}

#[test]
fn parse_variable_assignment_and_check() {
    let tokens = tokenizer("int x = 10; if (x == 10) { return x; }");
    let expected = vec![
        Token::Int,
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Number(10),
        Token::Semicolon,
        Token::If,
        Token::LParen,
        Token::Identifier("x".to_string()),
        Token::Equal,
        Token::Number(10),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::Identifier("x".to_string()),
        Token::Semicolon,
        Token::RBrace,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn vm_function_with_variable_return() {
    let tokens = tokenizer("int main() { int y = 7; return y; }");
    let ast = parse_token(&tokens);
    let instructions = get_vm(&ast);
    let mut vm = VM::new(instructions);
    vm.run();
    assert_eq!(vm.stack.last(), Some(&7));
}
#[test]
fn parse_and_codegen_function_call() {
    let ast = ASTNode::Sequence(vec![
        ASTNode::FunctionDef {
            name: "mul".into(),
            params: vec!["x".into(), "y".into()],
            body: Box::new(ASTNode::Return(Box::new(Expr::Mul(
                Box::new(Expr::Variable("x".into())),
                Box::new(Expr::Variable("y".into())),
            )))),
        },
        ASTNode::Return(Box::new(Expr::Call("mul".into(), vec![
            Expr::Number(3),
            Expr::Number(4),
        ]))),
    ]);
    let instructions = get_vm(&ast);
    assert_eq!(
        instructions,
        vec![
            Instruction::ENT(2),
            Instruction::LEA(0),
            Instruction::LI,
            Instruction::LEA(1),
            Instruction::LI,
            Instruction::MUL,
            Instruction::PSH,
            Instruction::EXIT,
            Instruction::IMM(3),
            Instruction::IMM(4),
            Instruction::JSR(0),
            Instruction::PSH,
            Instruction::EXIT,
        ]
    );
}

#[test]
fn tokenizer_handles_empty_string() {
    let tokens = tokenizer("");
    assert!(tokens.is_empty());
}

#[test]
fn parse_logical_expression() {
    let tokens = tokenizer("int main() { return 1 < 2 == 1; }");
    let ast = parse_token(&tokens);
    match ast {
        ASTNode::Sequence(v) => match &v[0] {
            ASTNode::Return(expr) => match &**expr {
                Expr::Equal(left, right) => {
                    assert!(matches!(**left, Expr::Less(_, _)));
                    assert!(matches!(**right, Expr::Number(1)));
                }
                _ => panic!("Expected equality expression"),
            },
            _ => panic!("Expected return node"),
        },
        _ => panic!("Expected sequence node"),
    }
}

#[test]
fn vm_syscall_stub_behavior() {
    let program = vec![
        Instruction::IMM(1),
        Instruction::IMM(123),
        Instruction::PRTF,
        Instruction::MALC,
        Instruction::IMM(3),
        Instruction::CLOS,
        Instruction::EXIT,
    ];
    let mut vm = VM::new(program);
    vm.run();
    assert_eq!(vm.stack.len(), 3); // Dummy syscall behavior
}
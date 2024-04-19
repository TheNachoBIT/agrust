use crate::lexer;
use crate::ast;
use crate::ast::RType;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs;

pub struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<ast::Expression>,
    all_registered_variables: Vec<ast::VariableInfo>,
    variable_id: usize,
    call_id: usize,
    is_current_fn_real: bool,
    binary_precedence: HashMap<String, i64>,
}

pub fn init_binary_precedence() -> HashMap<String, i64> {

    let mut h = HashMap::new();

    h.insert(String::from(";"), -1);
    h.insert(String::from("{"), -1);
    h.insert(String::from("}"), -1);

    h.insert(String::from("="), 2);
    h.insert(String::from("+="), 3);
    h.insert(String::from("-="), 3);

    h.insert(String::from("<"), 10);
    h.insert(String::from(">"), 10);
    h.insert(String::from("+"), 20);
    h.insert(String::from("-"), 20);
    h.insert(String::from("*"), 40);

    return h;
}

// This is probably going to be here temporarily, because i have no traits yet.
pub fn derives_copy_trait(ty: &ast::RType) -> bool {
    match ty {
        ast::RType::IntSize | ast::RType::Int64 | ast::RType::Int32 | ast::RType::Int16 | ast::RType::Int8 | ast::RType::Bool => true,
        _ => false,
    }
}

impl Parser {

    pub fn new(get_lex: lexer::Lexer) -> Self {
        Self {
            lex: get_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new(),
            variable_id: 0,
            call_id: 0,
            is_current_fn_real: false,
            binary_precedence: init_binary_precedence(),
        }
    }

    pub fn check_if_moved(&mut self, expr: &ast::Expression) {

        for v in &self.all_registered_variables {
            if expr.get_name() == v.name {

                if derives_copy_trait(&v.ty) {
                    continue;
                }

                if v.moved {
                    panic!("{} is already moved!", v.name);
                }
            }
        }
    }

    pub fn set_as_moved(&mut self, expr: &ast::Expression) {

        for v in &mut self.all_registered_variables {
            if expr.get_name() == v.name {
                v.moved = true;

                if derives_copy_trait(&v.ty) {
                    v.moved = false;
                }
            }
        }
    }

    pub fn parse_as(&mut self, expr: ast::Expression, get_target: Option<ast::Expression>) -> ast::Expression {

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        self.check_if_moved(&expr);

        self.set_as_moved(&expr);

        let final_target = if let Some(t) = get_target { t } else { expr.clone() };

        return ast::Expression::RAs { target: Box::new(final_target), val: Box::new(expr), ty: get_type };
    }

    pub fn parse_expression(&mut self, target: Option<ast::Expression>) -> ast::Expression {
        let expr = self.parse_primary(target.clone());

        let final_target = match target.clone() {
            Some(_t) => target,
            None => Some(expr.clone())
        };

        return self.parse_binary_operator(0, expr, final_target);
    }

    pub fn parse_number(&mut self) -> ast::Expression {

        let numb: f64 = self.lex.string_buffer.parse::<f64>().unwrap();

        self.lex.get_next_token();

        return ast::Expression::RNumber { val: numb };
    }

    pub fn parse_return(&mut self) -> ast::Expression {

        self.lex.get_next_token();

        let ret_val = self.parse_expression(None);

        return ast::Expression::RRealReturn { ret: Box::new(ret_val) };
    }

    pub fn parse_if(&mut self) -> ast::Expression {

        self.lex.get_next_token();

        let cond = self.parse_expression(None);

        if self.lex.current_token != lexer::LexerToken::Char('{') {
            panic!("Expected '{{' at beginning of 'if', Found {:#?}", &self.lex.current_token);
        }

        self.lex.get_next_token();

        let mut ifb: Vec<ast::Expression> = Vec::new();
        let mut elseb: Vec<ast::Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char('}') && self.lex.current_token != lexer::LexerToken::EndOfFile {

            let expr: ast::Expression = self.parse_expression(None);

            if self.lex.current_token != lexer::LexerToken::Char(';') {
                panic!("Expected ';' inside 'if'. Found {:#?}", &self.lex.current_token);
            }

            ifb.push(expr);

            self.lex.get_next_token();
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Else {
            return ast::Expression::RIf { condition: Box::new(cond), if_body: ifb, else_body: elseb }
        }

        self.lex.get_next_token();

        if self.lex.current_token == lexer::LexerToken::If {
            let expr = self.parse_if();
            elseb.push(expr);
            return ast::Expression::RIf { condition: Box::new(cond), if_body: ifb, else_body: elseb }
        }

        if self.lex.current_token != lexer::LexerToken::Char('{') {
            panic!("Expected '{{' at beginning of 'else', Found {:#?}", &self.lex.current_token);
        }

        self.lex.get_next_token();

        while self.lex.current_token != lexer::LexerToken::Char('}') && self.lex.current_token != lexer::LexerToken::EndOfFile {

            let expr: ast::Expression = self.parse_expression(None);

            if self.lex.current_token != lexer::LexerToken::Char(';') {
                panic!("Expected ';' inside 'else'. Found {:#?}", &self.lex.current_token);
            }

            elseb.push(expr);

            self.lex.get_next_token();
        }

        self.lex.get_next_token();

        return ast::Expression::RIf { condition: Box::new(cond), if_body: ifb, else_body: elseb }
    }

    pub fn parse_primary(&mut self, target: Option<ast::Expression>) -> ast::Expression {

        //dbg!(&self.lex.current_token);

        let mut res = match &self.lex.current_token {
            lexer::LexerToken::Let => self.parse_let(),
            lexer::LexerToken::Identifier(ident) => self.parse_identifier(ident.clone()),
            lexer::LexerToken::Number => self.parse_number(),
            lexer::LexerToken::Return => self.parse_return(),
            lexer::LexerToken::If => self.parse_if(),
            _ => panic!("Unknown primary identifier, found {:#?}", &self.lex.current_token)
        };

        if self.lex.current_token == lexer::LexerToken::As {
            res = self.parse_as(res, target.clone());
        }

        res
    }

    pub fn parse_call(&mut self, ident: String) -> ast::Expression {

        self.lex.get_next_token();

        let mut call_arguments: Vec<ast::Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char(')') {
            let expr = self.parse_expression(None);

            dbg!(&expr);

            call_arguments.push(expr);

            if self.lex.current_token != lexer::LexerToken::Char(',') && self.lex.current_token != lexer::LexerToken::Char(')') {
                panic!("Expected ',' or ')', Found {:#?}", &self.lex.current_token);
            }

            if self.lex.current_token == lexer::LexerToken::Char(',') {
                self.lex.get_next_token();
            }
        }

        if self.lex.current_token != lexer::LexerToken::Char(')') {
            panic!("Expected ')'");
        }

        self.lex.get_next_token();

        let get_call_id = self.call_id;
        self.call_id += 1;

        return ast::Expression::RCall { 
            name: ident, 
            id: get_call_id,  
            call_args: call_arguments
        };
    }

    pub fn parse_identifier(&mut self, ident: String) -> ast::Expression {

        self.lex.get_next_token();

        if self.lex.current_token == lexer::LexerToken::Char('(') {
            return self.parse_call(ident);
        }

        if self.lex.current_token == lexer::LexerToken::Char(':') {
            self.lex.get_next_token();

            if self.lex.current_token != lexer::LexerToken::Char(':') {
                self.lex.go_back();
            }
            else {
                self.lex.get_next_token();

                let old_ident = ident;

                if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
                    let new_ident = old_ident + "::" + &ident.clone();
                    return self.parse_identifier(new_ident);
                }
                else {
                    panic!("Expected identifier! Found {:#?}", &self.lex.current_token);
                }
            }
        }

        return ast::Expression::RVariable { name: ident };
    }

    pub fn parse_equals(&mut self, lv: ast::Expression, rv: ast::Expression) -> ast::Expression {

        self.check_if_moved(&rv);
        self.set_as_moved(&rv);

        return ast::Expression::REquals { lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_add(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        self.check_if_moved(&lv);
        self.set_as_moved(&lv);

        self.check_if_moved(&rv);
        self.set_as_moved(&rv);

        return ast::Expression::RAdd { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_sub(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        self.check_if_moved(&lv);
        self.set_as_moved(&lv);

        self.check_if_moved(&rv);
        self.set_as_moved(&rv);

        return ast::Expression::RSub { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_mul(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        self.check_if_moved(&lv);
        self.set_as_moved(&lv);

        self.check_if_moved(&rv);
        self.set_as_moved(&rv);

        return ast::Expression::RMul { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn get_token_precedence(&self, tok: String) -> i64 {
        return self.binary_precedence.get(&tok).copied().unwrap_or(-1);
    }

    pub fn create_binary(&mut self, op: String, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        match op.as_str() {
            "=" => self.parse_equals(lv, rv),
            "+" => self.parse_add(lv, rv, target),
            "-" => self.parse_sub(lv, rv, target),
            "*" => self.parse_mul(lv, rv, target),
            ";" | "{" => lv,
            ">" | "<" => self.parse_compare(lv, rv, op, target),
            _ => panic!("Unknown binary operator, found {:#?}", &op)
        }
    }

    pub fn parse_compare(&mut self, lv: ast::Expression, rv: ast::Expression, op: String, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        self.check_if_moved(&lv);
        self.set_as_moved(&lv);

        self.check_if_moved(&rv);
        self.set_as_moved(&rv);

        return ast::Expression::RCompare { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv), operator: op.to_string() };
    }

    pub fn is_an_end_character(&self, s: &String) -> bool {
        return s == ";" || s == "{" || s == "}" || s == "(" || s == ")" || s == "," || s == "";
    }

    pub fn parse_binary_operator(&mut self, expr_precedence: i64, lv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let mut grab_lv = lv;

        loop {

            let mut left_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            if self.is_an_end_character(&left_tok_string) { return grab_lv; }

            self.lex.get_next_token();

            if let lexer::LexerToken::Char(c) = self.lex.current_token {
                left_tok_string.write_char(c).unwrap();
                self.lex.get_next_token();
            }

            let precedence = self.get_token_precedence(left_tok_string.clone());

            if precedence < expr_precedence { return grab_lv; }

            let mut grab_rv = self.parse_primary(target.clone());

            let right_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            let next_precedence = self.get_token_precedence(right_tok_string);

            if precedence < next_precedence {
                grab_rv = self.parse_binary_operator(precedence + 1, grab_rv, target.clone());
            }

            grab_lv = self.create_binary(left_tok_string, grab_lv, grab_rv, target.clone());
        }
    }

    pub fn parse_type(&mut self, ident: String) -> RType {

        match ident.as_str() {
            "isize" => RType::IntSize,
            "i64" => RType::Int64,
            "i32" => RType::Int32,
            "i16" => RType::Int16,
            "i8" => RType::Int8,
            "bool" => RType::Bool,
            "f32" => RType::Float32,
            "f64" => RType::Float64,
            _ => panic!("Type \"{}\" does not exist", ident)
        }
    }

    pub fn parse_let(&mut self) -> ast::Expression {
        self.lex.get_next_token();

        let get_name = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            ident.clone()
        }
        else {
            panic!("Expected identifier!") 
        };

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char(':') {
            panic!("Expected ':'");
        }

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        let new_var_info = ast::VariableInfo::new(get_name.clone(), get_type.clone(), self.variable_id);

        self.variable_id += 1;

        self.all_registered_variables.push(new_var_info);

        return ast::Expression::RLet { name: get_name.to_string(), is_mutable: false, ty: get_type };
    }

    pub fn parse_function(&mut self, is_public: bool) -> ast::Expression {

        self.all_registered_variables.clear();
        self.variable_id = 0;

        self.lex.get_next_token();

        let get_name: String;
        if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            get_name = ident.clone();
        }
        else {
            panic!("Expected identifier");
        }

        self.is_current_fn_real = true;

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char('(') {
            panic!("Expected '('");
        }

        self.lex.get_next_token();

        // =======================[ARGUMENTS]========================

        let mut get_arguments: Vec<ast::Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char(')') {
            let get_name: String;
            if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
                get_name = ident.clone();
            }
            else {
                panic!("Expected identifier");
            }

            self.lex.get_next_token();

            if self.lex.current_token != lexer::LexerToken::Char(':') {
                panic!("Expected ':'");
            }

            self.lex.get_next_token();

            let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
                self.parse_type(ident.to_string()) 
            } 
            else { 
                panic!("Expected identifier for type!") 
            };

            self.lex.get_next_token();

            let new_var_info = ast::VariableInfo::new(get_name.clone(), get_type.clone(), self.variable_id);

            self.variable_id += 1;

            self.all_registered_variables.push(new_var_info);

            get_arguments.push(ast::Expression::RLet { name: get_name.to_string(), is_mutable: false, ty: get_type });

            if self.lex.current_token == lexer::LexerToken::Char(',') {
                self.lex.get_next_token();
            }
            else if self.lex.current_token == lexer::LexerToken::Char(')') {
                break;
            }
        }

        // ==========================================================

        if self.lex.current_token != lexer::LexerToken::Char(')') {
            panic!("Expected ')'");
        }

        self.lex.get_next_token();

        // =======================[TYPE]========================

        if self.lex.current_token != lexer::LexerToken::Char('-') {
            panic!("Expected '->'");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char('>') {
            panic!("Expected '->'");
        }

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        // =====================================================

        if self.lex.current_token != lexer::LexerToken::Char('{') {
            panic!("Expected '{{'");
        }

        self.lex.get_next_token();

         // =======================[BODY]========================

        let mut all_inst: Vec<ast::Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char('}') && self.lex.current_token != lexer::LexerToken::EndOfFile {

            let expr: ast::Expression = self.parse_expression(None);

            if expr.is_semicolon_free() {
                if self.lex.current_token == lexer::LexerToken::Char(';') {
                    panic!("The use of ';' is not allowed here.");
                }
            }
            else {

                if self.lex.current_token != lexer::LexerToken::Char(';') {
                    panic!("Expected ';'. Found {:#?}", &self.lex.current_token);
                }

                self.lex.get_next_token();
            }

            all_inst.push(expr);
        }

        // ======================================================

        if self.lex.current_token != lexer::LexerToken::Char('}') {
            panic!("Expected '}}'. Found {:#?}", &self.lex.current_token);
        }

        self.lex.get_next_token();

        let mut is_pub = is_public;
        if get_name == "main" {
            is_pub = true;
        }

        return ast::Expression::RFunction { name: get_name, is_fn_public: is_pub, ty: get_type, arguments: get_arguments, instructions: all_inst };
    }

    pub fn get_contents_of_rs_file(&mut self, name: String) -> Vec<ast::Expression> {

        let file_name: String = name.to_string() + ".rs";

        let lex: lexer::Lexer = lexer::Lexer::new(fs::read_to_string(&file_name).expect("Cannot open test.rs"));

        let mut par: Parser = Parser::new(lex);

        let _final_codegen = par.start();

        return par.all_instructions;
    }

    pub fn parse_mod(&mut self) -> ast::Expression {

        self.lex.get_next_token();

        let get_name: String;
        if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            get_name = ident.clone();
        }
        else {
            panic!("Expected identifier");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char(';') {
            panic!("Expected ';'");
        }

        self.lex.get_next_token();

        let get_all_contents = self.get_contents_of_rs_file(get_name.clone());

        return ast::Expression::RMod { name: get_name, content: get_all_contents };
    }

    pub fn parse_pub(&mut self) -> ast::Expression {
        self.lex.get_next_token();

        match self.lex.current_token {
            lexer::LexerToken::Function => { 
                self.parse_function(true)
            },
            _ => panic!("Expected function! Found {:#?}", &self.lex.current_token),
        }
    }

    pub fn start(&mut self) -> String {

        let mut final_result: String = String::new();

        self.lex.get_next_token();

        while self.lex.current_token != lexer::LexerToken::EndOfFile {

            match self.lex.current_token {
                lexer::LexerToken::Pub => {
                    let p = self.parse_pub();
                    self.all_instructions.push(p);
                },
                lexer::LexerToken::Function => { 
                    let func = self.parse_function(false);
                    self.all_instructions.push(func);
                },
                lexer::LexerToken::Mod => {
                    let get_mod = self.parse_mod();
                    self.all_instructions.push(get_mod);
                }
                _ => break
            };
        }

        final_result += "#![no_main]\n";

        for inst in &mut self.all_instructions {

            let res_cg = inst.codegen(0, false);

            if res_cg != "" {
                final_result += &(res_cg + "\n");
            }
        }

        final_result
    }
}
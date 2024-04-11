#[derive(Clone, PartialEq, Debug, PartialOrd, Eq, Ord)]
pub enum RType {
    Void,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    IntSize,
    Float32,
    Float64,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {

    RLet {
        name: String,
        ty: RType
    },
    RNumber {
        val: f64
    },
    RFunction {
        name: String,
        ty: RType,
        arguments: Vec<Expression>,
        instructions: Vec<Expression>,
        generate_real: bool,
    },
    RVariable {
        name: String,
    },
    REquals {
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RAdd {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RSub {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RMul {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RAs {
        target: Box<Expression>,
        val: Box<Expression>,
        ty: RType,
    },
    RRealReturn {
        ret: Box<Expression>,
        is_implicit: bool,
    },
    RCompare {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>,
        operator: String,
    },
    RCall {
        name: String,
        parent_fn_name: String,
        is_parent_real: bool,
        id: usize,
        fn_args: Vec<Expression>,
        call_args: Vec<Expression>,
        content: Vec<Expression>,
    },
    RIf {
        condition: Box<Expression>,
        if_body: Vec<Expression>,
        else_body: Vec<Expression>,
    },
    RNothing,
}

pub fn is_float(ty: &RType) -> bool {
    match ty {
        RType::Float32 | RType::Float64 => true,
        _ => false
    }
}

impl RType {
    pub fn codegen(&self) -> String {

        match self {
            RType::Bool => "bool".to_string(),
            RType::Int8 => "i8".to_string(),
            RType::Int16 => "i16".to_string(),
            RType::Int32 => "i32".to_string(),
            RType::Int64 => "i64".to_string(),
            RType::IntSize => "isize".to_string(),
            RType::Float32 => "f32".to_string(),
            RType::Float64 => "f64".to_string(),
            _ => todo!(),
        }
    }
}

fn tabs_to_string(tabs: isize) -> String {

    let mut res: String = String::new();

    for _i in 0..tabs {
        res += "\t";
    }

    res
}

impl Expression {

    pub fn is_semicolon_free(&self) -> bool {
        match self {
            Expression::RIf { .. } => true,
            _ => false,
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            Expression::RLet { name, .. } | Expression::RFunction { name, .. } | Expression::RVariable { name } => {
                name.to_string()
            },
            _ => "".to_string()
        }
    }

    pub fn add_parent_name(&mut self, parent_name: String) {

        match self {
            Expression::RLet { name, .. } | Expression::RVariable { name } => {
                let get_name = name.to_string();
                *name = parent_name.to_string() + "_" + &get_name;
            },

            Expression::RFunction { instructions, .. } => {
                for inst in instructions {
                    inst.add_parent_name(parent_name.to_string());
                }
            },
            Expression::REquals { lvalue, rvalue } |
            Expression::RAdd { lvalue, rvalue, .. } |
            Expression::RSub { lvalue, rvalue, .. } |
            Expression::RMul { lvalue, rvalue, .. } => {
                lvalue.add_parent_name(parent_name.to_string());
                rvalue.add_parent_name(parent_name.to_string());
            },
            Expression::RAs { val, .. } => {
                val.add_parent_name(parent_name.to_string());
            },
            Expression::RRealReturn { ret, .. } => {
                ret.add_parent_name(parent_name.to_string());
            },
            Expression::RCall { name, content, id, .. } => {
                //for c in content {
                //    c.add_parent_name(parent_name.to_string());
                //}
            },
            Expression::RIf { condition, if_body, else_body } => {
                condition.add_parent_name(parent_name.to_string());

                for i in if_body {
                    i.add_parent_name(parent_name.to_string());
                }

                for i in else_body {
                    i.add_parent_name(parent_name.to_string());
                }
            },
            _ => {}
        }
    }

    pub fn add_content_to_calls(&mut self, fn_list: &[Expression], fn_name: &String) {
        match self {
            Expression::RFunction { instructions, name, .. } => {
                for inst in instructions {
                    inst.add_content_to_calls(fn_list, &name);
                }
            },
            Expression::REquals { lvalue, rvalue } |
            Expression::RAdd { lvalue, rvalue, .. } |
            Expression::RSub { lvalue, rvalue, .. } |
            Expression::RMul { lvalue, rvalue, .. } => {
                lvalue.add_content_to_calls(fn_list, fn_name);
                rvalue.add_content_to_calls(fn_list, fn_name);
            },
            Expression::RAs { val, .. } => {
                val.add_content_to_calls(fn_list, fn_name);
            },
            Expression::RRealReturn { ret, .. } => {
                ret.add_content_to_calls(fn_list, fn_name);
            },
            Expression::RCall { name, parent_fn_name, content, fn_args, id, .. } => {
                // This is where the magic happens 8)
                let call_name = name;
                for function in fn_list {
                    if let Expression::RFunction { name, instructions, arguments, .. } = function {
                        if call_name == name {

                            *parent_fn_name = fn_name.to_string();

                            for arg in arguments {
                                fn_args.push(arg.clone());
                            }

                            for inst in instructions {
                                content.push(inst.clone());
                                let get_last_index = content.len() - 1;
                                content[get_last_index].add_content_to_calls(fn_list, &(name.to_owned() + &id.to_string()));
                            }
                            break;
                        }
                    }
                }
            },
            Expression::RIf { condition, if_body, else_body } => {
                condition.add_content_to_calls(fn_list, fn_name);

                for i in if_body {
                    i.add_content_to_calls(fn_list, fn_name);
                }

                for i in else_body {
                    i.add_content_to_calls(fn_list, fn_name);
                }
            }
            _ => {}
        }
    }

    pub fn can_generate_semicolon(&self) -> bool {

        match self {
            Expression::RRealReturn { is_implicit, .. } => {
                if *is_implicit { false } else { true }
            },
            Expression::RIf { .. } => false,
            _ => true
        }
    }

    pub fn set_implicit(&mut self, b: bool) {
        match self {
            Expression::RRealReturn { is_implicit, .. } => {
                *is_implicit = b;
            },
            _ => {}
        }
    }

    pub fn codegen(&mut self, tabs: isize) -> String {

        let final_res = match self {

            Expression::RLet { name, ty } => {
                "let".to_string() + " " + &name + ": " + &ty.codegen()
            },
            Expression::RNumber { val } => {
                val.to_string()
            },
            Expression::RFunction { name, ty, instructions, arguments, generate_real } => {

                if *generate_real == false {
                    return "".to_string();
                }

                let mut result: String = if name == "main" {
                    tabs_to_string(tabs) + "#[no_mangle]\n" +
                    &tabs_to_string(tabs) + "pub extern \"C\" "
                }
                else {
                    "".to_string()
                };

                result += &(tabs_to_string(tabs) + "fn " + &name + "(");
                
                let temp_args = if name == "main" {
                    "_: isize, _: *const *const u8".to_string()
                }
                else {
                    let mut count: isize = 0;
                    let arg_count: usize = arguments.len();
                    for a in arguments {
                        match a {
                            Expression::RLet { name, ty } => {
                                result += &(name.to_string() + ": " + &ty.codegen());
                                count += 1;
                                if count < (arg_count - 1).try_into().unwrap() {
                                    result += ", ";
                                }
                            },
                            _ => { panic!("What") }
                        }
                    }

                    "".to_string()
                };

                result += &(temp_args.to_string() + ") -> " + &ty.codegen() + " {\n");

                for i in instructions {
                    result += &i.codegen(tabs + 1);

                    if i.can_generate_semicolon() {
                        result += ";";
                    }

                    result += "\n";
                }

                result += "}\n";

                result
            },
            Expression::RVariable { name } => name.to_string(),
            Expression::REquals { lvalue, rvalue } => {
                lvalue.codegen(0) + " = " + &rvalue.codegen(0)
            },
            Expression::RAdd { lvalue, rvalue, .. } => {
                lvalue.codegen(0) + " + " + &rvalue.codegen(0)
            },
            Expression::RSub { lvalue, rvalue, .. } => {
                lvalue.codegen(0) + " - " + &rvalue.codegen(0)
            },
            Expression::RMul { lvalue, rvalue, .. } => {
                lvalue.codegen(0) + " * " + &rvalue.codegen(0)
            },
            Expression::RAs { val, ty, .. } => {
                val.codegen(0) + " as " + &ty.codegen()
            },
            Expression::RCompare { lvalue, rvalue, operator, .. } => {
                lvalue.codegen(0) + " " + &operator + " " + &rvalue.codegen(0)
            }
            Expression::RRealReturn { ret, is_implicit } => {
                if !*is_implicit {
                    "return ".to_string() + &ret.codegen(0)
                }
                else {
                    ret.codegen(0)
                }
            },
            Expression::RCall { name, parent_fn_name, is_parent_real, ref mut content, ref mut fn_args, ref mut call_args, id } => {
                let mut res = "({\n".to_string();

                let mut final_args: Vec<Expression> = Vec::new();

                for i in 0..fn_args.len() {
                    let new_eq = Expression::REquals {
                        lvalue: Box::new(fn_args[i].clone()),
                        rvalue: Box::new(call_args[i].clone())
                    };

                    final_args.push(new_eq);
                }

                // Arguments
                for a in &mut *final_args {
                    match a {
                        Expression::REquals { lvalue, rvalue } => {
                            lvalue.add_parent_name(name.to_string() + &id.to_string());

                            if !*is_parent_real {
                                rvalue.add_parent_name(parent_fn_name.to_string());
                            }
                        },
                        _ => { panic!("What") }
                    }
                }

                for a in &mut final_args {

                    a.set_implicit(true);

                    res += &(tabs_to_string(tabs + 2) + &a.codegen(0));

                    if a.can_generate_semicolon() {
                        res += ";";
                    }

                    res += "\n";
                }

                res += &(tabs_to_string(tabs + 2) + "{\n");

                dbg!(name.to_string());

                // Content
                for c in &mut *content {
                    c.add_parent_name(name.to_string() + &id.to_string());
                }

                for c in content {

                    c.set_implicit(true);

                    res += &(tabs_to_string(tabs + 3) + &c.codegen(0));

                    if c.can_generate_semicolon() {
                        res += ";";
                    }

                    res += "\n";
                }

                res += &(tabs_to_string(tabs + 2) + "}\n");
                res += &(tabs_to_string(tabs + 1) + "})");

                res
            },
            Expression::RIf { condition, if_body, else_body } => {

                let mut res = "if ".to_string() + &condition.codegen(0) + " {\n";

                for c in if_body {
                    res += &(tabs_to_string(tabs + 1) + &c.codegen(0));

                    if c.can_generate_semicolon() {
                        res += ";";
                    }

                    res += "\n"; 
                }

                res += &(tabs_to_string(tabs) + "}\n");

                if else_body.len() != 0 {
                    res += &(tabs_to_string(tabs) + "else {\n");

                    for c in else_body {
                        res += &(tabs_to_string(tabs + 1) + &c.codegen(0));

                        if c.can_generate_semicolon() {
                            res += ";";
                        }

                        res += "\n";
                    }

                    res += &(tabs_to_string(tabs) + "}\n");
                }

                res
            }
            _ => todo!(),
        };

        tabs_to_string(tabs) + &final_res
    }
}

#[derive(Clone)]
pub struct VariableInfo {
    pub name: String,
    pub ty: RType,
    pub id: usize,
    pub moved: bool,
}

impl VariableInfo {
    pub fn new(get_name: String, get_type: RType, get_id: usize) -> Self {
        Self {
            name: get_name,
            ty: get_type,
            id: get_id,
            moved: false,
        }
    }
}
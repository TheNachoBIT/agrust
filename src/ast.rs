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
        is_mutable: bool,
        ty: RType
    },
    RNumber {
        val: f64
    },
    RFunction {
        name: String,
        ty: RType,
        is_fn_public: bool,
        arguments: Vec<Expression>,
        instructions: Vec<Expression>,
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
    },
    RCompare {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>,
        operator: String,
    },
    RCall {
        name: String,
        id: usize,
        call_args: Vec<Expression>,
    },
    RIf {
        condition: Box<Expression>,
        if_body: Vec<Expression>,
        else_body: Vec<Expression>,
    },
    RMod {
        name: String,
        content: Vec<Expression>,
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

fn add_internal_main() -> String {
    "
#[no_mangle]
pub extern \"C\" fn __main() {
    // Delete stinky GCC C++ stdlib initialization, saving 115 KBs! (I'm a performance addict -NachoBIT)
}

".to_string()
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
            Expression::RMul { lvalue, rvalue, .. } |
            Expression::RCompare { lvalue, rvalue, .. } => {
                lvalue.add_parent_name(parent_name.to_string());
                rvalue.add_parent_name(parent_name.to_string());
            },
            Expression::RAs { val, .. } => {
                val.add_parent_name(parent_name.to_string());
            },
            Expression::RRealReturn { ret, .. } => {
                ret.add_parent_name(parent_name.to_string());
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

    pub fn can_generate_semicolon(&self) -> bool {

        match self {
            Expression::RIf { .. } => false,
            _ => true
        }
    }

    pub fn codegen(&mut self, tabs: isize, is_right: bool) -> String {

        let final_res = match self {

            Expression::RLet { name, ty, is_mutable } => {
                if !*is_mutable {
                    "let ".to_string() + &name + ": " + &ty.codegen()
                }
                else {
                    "let mut ".to_string() + &name + ": " + &ty.codegen()
                }
            },
            Expression::RNumber { val } => {
                val.to_string()
            },
            Expression::RFunction { name, ty, instructions, arguments, is_fn_public } => {

                let mut result: String = if name == "main" {
                    tabs_to_string(tabs) + &add_internal_main() +
                    &tabs_to_string(tabs) + "#[no_mangle]\n" +
                    &tabs_to_string(tabs)
                }
                else {
                    "".to_string()
                };

                if *is_fn_public {
                    result += "pub "; 
                }

                if name == "main" {
                    result += "extern \"C\" ";
                }

                result += &("fn ".to_owned() + &name.to_string() + "(");
                
                let temp_args = if name == "main" {
                    "_: isize, _: *const *const u8".to_string()
                }
                else {
                    let mut count: isize = 0;
                    let arg_count: usize = arguments.len();
                    for a in arguments {
                        match a {
                            Expression::RLet { name, ty, is_mutable: _ } => {
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

                    result += &i.codegen(tabs + 1, false);

                    if i.can_generate_semicolon() {
                        result += ";";
                    }

                    result += "\n";
                }

                result += &(tabs_to_string(tabs) + "}\n");

                result
            },
            Expression::RVariable { name } => name.to_string(),
            Expression::REquals { lvalue, rvalue } => {
                lvalue.codegen(tabs, true) + " = " + &rvalue.codegen(tabs, true)
            },
            Expression::RAdd { lvalue, rvalue, .. } => {
                lvalue.codegen(tabs, true) + " + " + &rvalue.codegen(tabs, true)
            },
            Expression::RSub { lvalue, rvalue, .. } => {
                lvalue.codegen(tabs, true) + " - " + &rvalue.codegen(tabs, true)
            },
            Expression::RMul { lvalue, rvalue, .. } => {
                lvalue.codegen(tabs, true) + " * " + &rvalue.codegen(tabs, true)
            },
            Expression::RAs { val, ty, .. } => {
                val.codegen(tabs, true) + " as " + &ty.codegen()
            },
            Expression::RCompare { lvalue, rvalue, operator, .. } => {
                lvalue.codegen(tabs, true) + " " + &operator + " " + &rvalue.codegen(tabs, true)
            }
            Expression::RRealReturn { ret } => {
                "return ".to_string() + &ret.codegen(tabs, true)
            },
            Expression::RCall { 
                name, id, call_args 
            } => {
                let mut res = name.to_string();

                res += "(";

                let call_args_length = call_args.len();
                let call_args_count = 0;
                for i in call_args {
                    res += &i.codegen(tabs, true);
                    if call_args_count < call_args_length - 1 {
                        res += ", ";
                    }
                }

                res += ")";
                res
            },
            Expression::RIf { condition, if_body, else_body } => {

                let mut res = "if ".to_string() + &condition.codegen(tabs, true) + " {\n";

                for c in if_body {
                    res += &c.codegen(tabs + 1, false);

                    if c.can_generate_semicolon() {
                        res += ";";
                    }

                    res += "\n"; 
                }

                res += &(tabs_to_string(tabs) + "}\n");

                if else_body.len() != 0 {
                    res += &(tabs_to_string(tabs) + "else {\n");

                    for c in else_body {
                        res += &c.codegen(tabs + 1, false);

                        if c.can_generate_semicolon() {
                            res += ";";
                        }

                        res += "\n";
                    }

                    res += &(tabs_to_string(tabs) + "}\n");
                }

                res
            },
            Expression::RMod { name, content } => {

                let mut res: String = "mod ".to_string() + &name + " {\n";

                for i in content {
                    res += &i.codegen(tabs + 1, false);
                }

                res += &(tabs_to_string(tabs) + "}");

                res
            },
            _ => todo!(),
        };

        if !is_right {
            tabs_to_string(tabs) + &final_res
        }
        else {
            final_res
        }
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
use std::sync::Arc;
use tokio::sync::{Notify, RwLock};

#[derive(Debug, Clone)]
pub struct Program {
    pub rules: Vec<Rule>
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub head: BasicType,
    pub guard: Vec<GuardExpr>,
    pub body: Vec<BodyExpr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GuardExpr {
    
}

#[derive(Debug, Clone)]
pub enum BodyExpr {
    Is {
	left: BasicType,
	right: MathExpr,
    },
    Print {
	msg: BasicType,
    },
    Call(BasicType),
    Assign {
	var: BasicType,
	value: BasicType,
    }
}

#[derive(Debug, Clone)]
pub enum MathExpr {
    Num(BasicType),
    Sum(Box<MathExpr>, Box<MathExpr>),
    Sub(Box<MathExpr>, Box<MathExpr>),
}

#[derive(Debug, Clone)]
pub struct VarValue {
    pub channel: Arc<VarChannel>
}

#[derive(Debug)]
pub struct VarChannel {
    pub notify: Notify,
    pub lock: RwLock<Option<BasicType>>,
}
impl VarValue {
    pub fn new() -> Self {
	VarValue{ channel: Arc::new(VarChannel {
	    notify: Notify::new(),
	    lock: RwLock::new(None)})}
    }
}

#[derive(Debug, Clone)]
pub enum BasicType {
    Number(i64),
    Var {
	name: String,
	value: VarValue,
    },
    Atom(String),
    Str {
	name: String,
	args: Vec<BasicType>,
    }
}

impl std::fmt::Display for BasicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    BasicType::Number(n) => write!(f, "{}", n),
	    BasicType::Var { name, .. } => write!(f, "{}", name),
	    BasicType::Atom(atom) => write!(f, "{}", atom),
	    BasicType::Str { name, args} => {
		let args_str: Vec<String> = args.iter().map(|x| x.to_string()).collect();
		let args = args_str.join(",");
		write!(f, "{}({})", name, args)
	    }
	}
    }
}

use std::collections::HashMap;
use tokio::task::JoinSet;
use async_recursion::async_recursion;

use crate::types::*;

pub fn run_body_expr_spawn(join_set: &mut JoinSet<()>, body: &BodyExpr, matches: &VarSubstitution,  program: &Program) {
    let body = body.clone();
    let matches = matches.clone();
    let program = program.clone();
    join_set.spawn(async move {
	match run_body_expr(&body, &matches).await {
	    State::Stop => (),
	    State::Call(call) => run(call, &program).await,
	}
    });
}

pub async fn run(query: BasicType, program: &Program) {
    let mut query = query;
    let mut join_set = tokio::task::JoinSet::new();
    'outer: loop {
        for rule in program.rules.iter() {
	    if let Some(mut matches) = pattern_match(&query, &rule.head).await {
		if eval_guard(&rule.guard, &matches).await == true {
		    add_vars_not_in_head(&mut matches, &rule.body);
		    for body in &rule.body[1..] {
			run_body_expr_spawn(&mut join_set, body, &matches, program);
		    }
		    match run_body_expr(&rule.body[0], &matches).await {
			State::Stop => break 'outer,
			State::Call(call) => query = call,
		    }
		    break;
		}
	    }
	}
    }

    while let Some(_) = join_set.join_next().await { }
}

fn add_vars_not_in_head(matches: &mut VarSubstitution, body_exprs: &Vec<BodyExpr>) {
    for body in body_exprs {
	match body {
	    BodyExpr::Assign { var, value } => {
		add_vars_not_in_head_basic_type(matches, &var);
		add_vars_not_in_head_basic_type(matches, &value);
	    },
	    BodyExpr::Is { left, right } => {
		if let BasicType::Var { name, value } = left {
		    if let None = matches.get(name) {
			matches.insert(name.clone(), BasicType::Var { name: name.clone(), value: VarValue::new()});
		    }
		}
		add_vars_not_in_head_math_expr(matches, right);
	    },
	    BodyExpr::Call(call) => {
		add_vars_not_in_head_basic_type(matches, &call);
	    },
	    _ => unreachable!()
	}
    }
}

fn add_vars_not_in_head_math_expr(matches: &mut VarSubstitution, math_expr: &MathExpr) {
    match math_expr {
	MathExpr::Sum(a, b) => {
	    add_vars_not_in_head_math_expr(matches, a);
    	    add_vars_not_in_head_math_expr(matches, b);
	},
	MathExpr::Sub(a, b) => {
	    add_vars_not_in_head_math_expr(matches, a);
    	    add_vars_not_in_head_math_expr(matches, b);
	},
	MathExpr::Num(basic_type) => {
	    add_vars_not_in_head_basic_type(matches, basic_type);
	}
    }
}

fn add_vars_not_in_head_basic_type(matches: &mut VarSubstitution, basic_type: &BasicType) {
    match basic_type {
	BasicType::Var { name, value } => {
	    if let None = matches.get(name) {
		matches.insert(name.clone(), BasicType::Var { name: name.clone(), value: VarValue::new()});
	    }
	},
	BasicType::Str { args, .. } => {
	    for arg in args {
		add_vars_not_in_head_basic_type(matches, arg);
	    }
	},
	_ => {}
    }
}

type VarSubstitution = HashMap<String, BasicType>;

enum State {
    Stop,
    Call(BasicType),
}

async fn run_body_expr(body: &BodyExpr, matches: &VarSubstitution) -> State {
    match body {
	BodyExpr::Call(call) => {
	    match call {
		BasicType::Atom(..) => State::Call(call.clone()),
		BasicType::Str { name, args } => {
		    let args = subst_matches(args, matches);
		    if name == "print" {
			match &args[0] {
			    BasicType::Var { value, .. } => {
				println!("{}", value.get().await);
			    },
			    _ => {
				println!("{}", args[0]);
			    }
			}
			return State::Stop;
		    }
		    State::Call(BasicType::Str { name: name.clone(), args })
		}
		_ => unreachable!()
	    }
	}
	BodyExpr::Print { msg } => {
	    println!("{}", msg);
	    State::Stop
	}
	BodyExpr::Assign { var, value } => {
	    let args = subst_matches(&vec![var.clone(), value.clone()], matches);
	    // tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
	    if let BasicType::Var { value: var_value, .. } = &args[0] {
		var_value.set(args[1].clone()).await;
	    }
	    State::Stop
	}
	BodyExpr::Is { left, right } => {
	    let args = subst_matches(&vec![left.clone()], matches);
	    let math_expr = subst_matches_math_expr(right, matches);

	    if let BasicType::Var { value: var_value, name } = &args[0] {		
		let result = eval_math_expr(&math_expr).await;
		var_value.set(BasicType::Number(result)).await;
	    }
	    State::Stop
	}
	_ => unreachable!()
    }
}

fn subst_matches(args: &Vec<BasicType>, matches: &VarSubstitution) -> Vec<BasicType> {
    args.iter().map(|x| {
	match x {
	    BasicType::Var { name, .. } => {
		match matches.get(name) {
		    Some(value) => value.clone(),
		    None => x.clone(),
		}
	    },
	    BasicType::Str { name, args } => {
		BasicType::Str { name: name.clone(), args: subst_matches(args, matches) }
	    }
	    _ => x.clone()
	}
    }).collect()
}

fn subst_matches_math_expr(math_expr: &MathExpr, matches: &VarSubstitution) -> MathExpr {
    match math_expr {
	MathExpr::Num(a) => {
	    let args = subst_matches(&vec![a.clone()], matches);
	    MathExpr::Num(args[0].clone())
	}
	MathExpr::Sum(a, b) => {
	    MathExpr::Sum(Box::new(subst_matches_math_expr(a, matches)), Box::new(subst_matches_math_expr(b, matches)))
	}
	MathExpr::Sub(a, b) => {
	    MathExpr::Sub(Box::new(subst_matches_math_expr(a, matches)), Box::new(subst_matches_math_expr(b, matches)))
	}	
    }
}

#[async_recursion]
async fn pattern_match(data: &BasicType, pattern: &BasicType) -> Option<VarSubstitution> {
    match (data, pattern) {
	(BasicType::Atom(x), BasicType::Atom(y)) => {
	    if x == y {
		Some(HashMap::new())
	    } else {
		None
	    }
	}
	(BasicType::Number(x), BasicType::Number(y)) => {
	    if x == y {
		Some(HashMap::new())
	    } else {
		None
	    }
	}
	(BasicType::Var { name: name1, value: value1}, BasicType::Var { name: name2, .. }) => {
	    Some(HashMap::from([(name2.clone(), BasicType::Var { name: name1.clone(), value: value1.clone()})]))
	}
	(BasicType::Str { name: name1, args: args1 }, BasicType::Str { name: name2, args: args2 }) => {
	    if name1 == name2 && args1.len() == args2.len() {
		let mut subst = HashMap::new();
		for (arg1, arg2) in args1.iter().zip(args2.iter()) {
		    match pattern_match(arg1, arg2).await {
			Some(s) => {
			    for (key, val1) in s.iter() {
				match subst.get(key) {
				    Some(val2) => {
					// TODO: Fix this
					//if val1 != val2 {
					//    return None;
					//}
				    },
				    None => {
					subst.insert(key.clone(), val1.clone());
				    }
				}
			    }
			},
			None => return None,
		    }
		}
		Some(subst)
	    } else {
		None
	    }
	},
	(BasicType::Atom(x), BasicType::Var { name, .. }) => {
	    Some(HashMap::from([(name.clone(), BasicType::Atom(x.clone()))]))
	},
	(BasicType::Number(x), BasicType::Var{ name, .. }) => {
	    Some(HashMap::from([(name.clone(), BasicType::Number(*x))]))
	},
	(BasicType::Str { name, args }, BasicType::Var { name: var_name, .. }) => {
	    Some(HashMap::from([(var_name.clone(), BasicType::Str { name: name.clone(), args: args.clone() })]))
	},
	(BasicType::Var { value, .. }, BasicType::Number(x)) => {
	    let data = value.get().await;
	    if let BasicType::Number(y) = data {
		if *x == y {
		    Some(HashMap::new())
		} else {
		    None
		}
	    } else {
		None
	    }
	},
	(BasicType::Var { value, .. }, BasicType::Atom(x)) => {
	    let data = value.get().await;
	    if let BasicType::Atom(y) = data {
		if x == &y {
		    Some(HashMap::new())
		} else {
		    None
		}
	    } else {
		None
	    }
	},
	(BasicType::Var { value, .. }, BasicType::Str { .. }) => {
	    let data = value.get().await;
	    pattern_match(&data, pattern).await
	},
	(BasicType::Atom(_), BasicType::Number(_)) => None,
	(BasicType::Number(_), BasicType::Atom(_)) => None,
	(BasicType::Atom(_), BasicType::Str { .. }) => None,
	(BasicType::Str { .. }, BasicType::Atom(_)) => None,
	(BasicType::Number(_), BasicType::Str { .. }) => None,
	(BasicType::Str { .. }, BasicType::Number(_)) => None,
    }
}

#[async_recursion]
async fn eval_math_expr(expr: &MathExpr) -> i64 {
    match expr {
	MathExpr::Num(basic_type) => {
	    match basic_type {
		BasicType::Number(x) => *x,
		BasicType::Var { value, .. } => {
		    let data = value.get().await;
		    if let BasicType::Number(x) = data { 
			return x;
		    } else {
			panic!("Not a number");
		    }
		},
		_ => unreachable!()
	    }
	},
	MathExpr::Sum(a, b) => {
	    let (a, b) = tokio::join!(eval_math_expr(a), eval_math_expr(b));
	    a + b
	},
	MathExpr::Sub(a, b) => {
	    let (a, b) = tokio::join!(eval_math_expr(a), eval_math_expr(b));
	    a - b
	}	
    }
}

async fn eval_guard(guards: &Vec<GuardExpr>, matches: &VarSubstitution) -> bool {
    for guard in guards {
	let guard = subst_matches_guard(&guard, matches);
	match guard {
	    GuardExpr::Data(basic_type) => {
		match basic_type {
		    BasicType::Var { value, .. } => {
			value.get().await;
		    },
		    _ => unreachable!(),
		}
	    },
	    GuardExpr::Equal(left, right) => {
		let mut left = left;
		let mut right = right;
		if let BasicType::Var { value, .. } = left {
		    left = value.get().await;
		}
		if let BasicType::Var { value, .. } = right {
		    right = value.get().await;
		}		
		if pattern_match(&left, &right).await.is_none() {
		    return false;
		}
	    }
	}
    }
    true
}

fn subst_matches_guard(guard: &GuardExpr, matches: &VarSubstitution) -> GuardExpr {
    match guard {
	GuardExpr::Data(var) => {
	    if let BasicType::Var { ref name, .. } = var {
		match matches.get(name) {
		    None => GuardExpr::Data(var.clone()),
		    Some(x) => GuardExpr::Data(x.clone()),
		}
	    } else {
		guard.clone()
	    }
	},
	GuardExpr::Equal(left, right) => {
	    let left = if let BasicType::Var { ref name, .. } = left {
		match matches.get(name) {
		    None => left.clone(),
		    Some(x) => x.clone(),
		}
	    } else {
		left.clone()
	    };
	    let right = if let BasicType::Var { ref name, .. } = right {
		match matches.get(name) {
		    None => right.clone(),
		    Some(x) => x.clone(),
		}
	    } else {
		right.clone()
	    };
	    GuardExpr::Equal(left, right)
	}
    }
}

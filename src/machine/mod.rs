use std::collections::HashMap;
use async_recursion::async_recursion;

use crate::types::*;

pub fn run_body_expr_spawn(body: &BodyExpr, matches: &VarSubstitution,  program: &Program) {
    let body = body.clone();
    let matches = matches.clone();
    let program = program.clone();
    tokio::spawn(async move {
	match run_body_expr(&body, &matches).await {
	    State::Stop => (),
	    State::Call(call) => run(call, &program).await,
	}
    });
}

pub async fn run(query: BasicType, program: &Program) {
    let mut query = query;
    'outer: loop {
        for rule in program.rules.iter() {
	    if let Some(mut matches) = pattern_match(&query, &rule.head).await {
		add_vars_not_in_head(&mut matches, &rule.body);
		for body in &rule.body[1..] {
		    run_body_expr_spawn(body, &matches, program);
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

fn add_vars_not_in_head(matches: &mut VarSubstitution, body_exprs: &Vec<BodyExpr>) {
    for body in body_exprs {
	match body {
	    BodyExpr::Assign { var, .. } => {
		if let BasicType::Var { name, value } = var {
		    if let None = matches.get(name) {
			matches.insert(name.clone(), BasicType::Var { name: name.clone(), value: value.clone()});
		    }
		}
	    },
	    BodyExpr::Call(call) => {
		add_vars_not_in_head_basic_type(matches, &call);
	    },
	    _ => unreachable!()
	}
    }
}

fn add_vars_not_in_head_basic_type(matches: &mut VarSubstitution, basic_type: &BasicType) {
    match basic_type {
	BasicType::Var { name, value } => {
	    if let None = matches.get(name) {
		matches.insert(name.clone(), BasicType::Var { name: name.clone(), value: value.clone()});
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
			println!("{}", args[0]);
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
		let mut data = var_value.channel.lock.write().await;
		match *data {
		    None => {
			*data = Some(args[1].clone());
		    },
		    _ => unreachable!()
		}
		var_value.channel.notify.notify_waiters();
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
	    _ => x.clone()
	}
    }).collect()
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
	(BasicType::Var { value, name }, BasicType::Number(x)) => {
	    fn check_data(data: &BasicType, number: i64) -> Option<VarSubstitution> {
		if let BasicType::Number(y) = data {
		    if number == *y {
			return Some(HashMap::new());
		    } else {
			return None;
		    }
		}
		return None;
	    }
	    {
	        let data = value.channel.lock.read().await;
		if let Some(ref y) = *data {
		    return check_data(&y, *x);
		}
	    }
	    value.channel.notify.notified().await;
	    {
	        let data = value.channel.lock.read().await;
		if let Some(ref y) = *data {
		    return check_data(&y, *x);
		}
	    }
	    return None;
	},	
	(BasicType::Atom(_), BasicType::Number(_)) => None,
	(BasicType::Number(_), BasicType::Atom(_)) => None,
	(BasicType::Atom(_), BasicType::Str { .. }) => None,
	(BasicType::Str { .. }, BasicType::Atom(_)) => None,
	(BasicType::Number(_), BasicType::Str { .. }) => None,
	(BasicType::Str { .. }, BasicType::Number(_)) => None,
	_ => unreachable!()
    }
}

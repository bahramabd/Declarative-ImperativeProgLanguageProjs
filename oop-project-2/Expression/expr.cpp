#include "expr.h"

namespace sym 
{
	Expr::Expr(const Const& c) { 
	    expr_=c.eval();
	}
	Expr::Expr(const Var& v) { 
	    expr_=v.eval();
	}
	Expr::Expr(const __expr_t* e) { 
	   expr_ =e->eval();
	}
	Expr::Expr(const __expr_t& e) { 
	    expr_=e.eval();
	}
	Expr::Expr(const Expr& e) { 
	    expr_=e.eval();
	}
		
	Expr::~Expr() { 
	    expr_=NULL;
	}

	__expr_t* Expr::eval(const var_map_t& var_map) const { 
	    return new Expr(expr_->eval());
	}
	__expr_t* Expr::diff(const std::string& v) const { 
	    return new Expr(expr_->diff(v));
	}
	std::ostream& Expr::operator<< (std::ostream &out) const { 
	    out<<*expr_;
	    return out;
	}
	bool Expr::operator==(const Expr& other) const { 
	    
	}
	bool Expr::operator==(const __expr_t& other) const { 
	    
	}
}                                                                  	

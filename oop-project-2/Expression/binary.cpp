#include "binary.h"
#include "nullary.h"
#include "core.h"
#include "type.h"
#include "unary.h"
#include <math.h>

namespace sym 
{
	bool AddOp::is_add() const { return true;}

	__expr_t* AddOp::eval(const var_map_t& vars) const { 
	    return new AddOp(lhs_->eval(),rhs_->eval());
	}

	__expr_t* AddOp::diff(const std::string& v) const { 
	    return new AddOp(lhs_->diff(v),rhs_->diff(v));
	}

	std::ostream& AddOp::operator<< (std::ostream &out) const {
	    if (*lhs_==Const(0)) {out<<*rhs_;
	    delete lhs_; delete rhs_;
	    return out;}
	    
	    out<< *lhs_<< " + "<< *rhs_;
	    delete lhs_; delete rhs_;
	    return out;
	    
	    
	}

	bool AddOp::operator==(const __expr_t& other_) const { 
	    if (*this==other_){delete this; return true;}
	    else{ return false;}
	}
}

namespace sym 
{
	bool MulOp::is_mul() const {return true; }

	__expr_t* MulOp::eval(const var_map_t& vars) const { 
	    return new MulOp(lhs_->eval(),rhs_->eval());
	}

	__expr_t* MulOp::diff(const std::string& v) const { 
	    MulOp(lhs_->diff(v),rhs_),
	    MulOp(lhs_,rhs_->diff(v));
	}

	std::ostream& MulOp::operator<< (std::ostream &out) const { 
	    
	    out << *lhs_<< " * "<< *rhs_;
	    delete lhs_; delete rhs_;
	    return out;
	}

	bool MulOp::operator==(const __expr_t& other_) const { 
	    if (*this==other_){delete this; return true;}
	    else{ return false;}
	}
}

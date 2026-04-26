#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>

namespace sym 
{
	bool NegOp::is_neg() const {return true; }

	__expr_t* NegOp::eval(const var_map_t& vars) const {
	    return new NegOp(operand->eval());
}
	__expr_t* NegOp::diff(const std::string& v) const {
	     
	    return new NegOp(operand->diff(v));
	}

	std::ostream& NegOp::operator<< (std::ostream &out) const {
	    out<<"-"<<(*operand);
	    delete operand;
	    return out;
	}

	bool NegOp::operator==(const __expr_t& other_) const {
	    const NegOp*p=dynamic_cast<const NegOp*> (other_.eval());
	    bool a = operand == p->operand;
	    delete p;
	    return a;
}
}
namespace sym 
{
	bool ExpOp::is_exp() const {return true; }

	__expr_t* ExpOp::eval(const var_map_t& vars) const { 
	    return new ExpOp(operand->eval());
	}

	__expr_t* ExpOp::diff(const std::string& v) const { 
	    return new ExpOp(operand->eval());
	}

	std::ostream& ExpOp::operator<< (std::ostream &out) const { 
	    out<<"e^"<<(*operand);
	    delete operand;
	    return out;
	}

	bool ExpOp::operator==(const __expr_t& other_) const { 
	   const ExpOp*p=dynamic_cast<const ExpOp*> (other_.eval());
	    bool a = operand == p->operand;
	    delete p;
	    return a;
	}
}

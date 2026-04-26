#include "nullary.h"
#include "unary.h"
#include "binary.h"

namespace sym {
	__expr_t& operator-(const __expr_t &op) { 
	}
	__expr_t& exp(const __expr_t &op) { }

	__expr_t& operator+(const __expr_t &lhs, const __expr_t &rhs) { 
	    
	}
	__expr_t& operator+(double lhs, const __expr_t &rhs) { }
	__expr_t& operator+(const __expr_t &lhs, double rhs) { }

	__expr_t& operator*(const __expr_t &lhs, const __expr_t &rhs) { }
	__expr_t& operator*(double lhs, const __expr_t &rhs) { }
	__expr_t& operator*(const __expr_t &lhs, double rhs) { }
}

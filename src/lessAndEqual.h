#ifndef R_R_H
#include <R.h>
#endif

#ifndef R_INTERNALS_H_
#include <Rinternals.h>
#endif

#ifndef R_COMPLEX_H
#include <R_ext/Complex.h>
#endif


template <typename T>
class lessAndEqual {
public:
    /* for general T where operator< and operator== have been implemented; 
     * this is helpful for 
     *  (1) integers where NA's is just a special integer value;
     *  (2) unsigned char, where NA's is converted to 00;
     *  (3) CharSEXP, where NA's are properly handled by operator<.
     */
    bool operator() (const T& lhs, const T& rhs) const { return lhs < rhs;} // less than 
    bool identical (const T& lhs, const T& rhs) const { return lhs == rhs;}
};


template <>
class lessAndEqual<double> {
private:
    bool mutable rhsTest; // to be used by operator() only
public:
    /* Assumptions: NaN < NA_real_ < -Inf < Finite numbers < Inf */
    bool operator() (const double& lhs, const double& rhs) const   // less than 
    {
        if (R_FINITE(lhs) && R_FINITE(rhs)) return lhs< rhs; // probably the most common case
        
        rhsTest = R_IsNaN(rhs);
        if (R_IsNaN(lhs)) return !rhsTest;
        
        rhsTest = rhsTest || ISNA(rhs);
        if (ISNA(lhs)) return !rhsTest;
        
        rhsTest = rhsTest || (rhs == R_NegInf);
        if (lhs == R_NegInf) return !rhsTest;
        
        return !(rhsTest || lhs < rhs); 
        
    }

    bool identical (const double& lhs, const double& rhs) const 
    {return(
        (lhs == rhs) ||
        (ISNA(lhs) && ISNA(rhs)) ||
        (R_IsNaN(lhs) && R_IsNaN(rhs))
    );}
};



template <>
class lessAndEqual<Rcomplex> {
private:
    lessAndEqual<double> le;
public:
    bool operator() (const Rcomplex& lhs, const Rcomplex& rhs) const 
    {
        if (le.identical(lhs.r , rhs.r)) return le(lhs.i , rhs.i);
        return le(lhs.r , rhs.r);
    }

    bool identical (const Rcomplex& lhs, const Rcomplex& rhs) const 
    {
        return le.identical(lhs.r, rhs.r) && le.identical(lhs.i, rhs.i) ;
    }
};


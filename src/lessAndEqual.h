#ifndef R_R_H
#include <R.h>
#endif

#ifndef R_INTERNALS_H_
#include <Rinternals.h>
#endif

#ifndef R_COMPLEX_H
#include <R_ext/Complex.h>
#endif


/* for general T where operator< and operator== have been implemented; 
 * this is helpful for 
 *  (1) integers where NA's is just a special integer value;
 *  (2) unsigned char, where NA's is converted to 00;
 *  (3) CharSEXP, where NA's are properly handled by operator<.
 */
template <typename T>
inline bool lessThan (const T& lhs, const T& rhs) { return lhs < rhs;}
template <typename T>
inline bool equalTo (const T& lhs, const T& rhs) { return lhs == rhs;}


/* double Assumptions: NaN < NA_real_ < -Inf < Finite numbers < Inf */
template <>
inline bool lessThan<double>(const double& lhs, const double& rhs) 
{
    if (R_FINITE(lhs) && R_FINITE(rhs)) return lhs< rhs; // probably the most common case
    
    bool rhsTest = R_IsNaN(rhs);
    if (R_IsNaN(lhs)) return !rhsTest;
    
    rhsTest = rhsTest || ISNA(rhs);
    if (ISNA(lhs)) return !rhsTest;
    
    rhsTest = rhsTest || (rhs == R_NegInf);
    if (lhs == R_NegInf) return !rhsTest;
    
    return !(rhsTest || lhs < rhs); 
    
}
template <>
inline bool equalTo<double> (const double& lhs, const double& rhs) 
{return(
    (lhs == rhs) ||
    (ISNA(lhs) && ISNA(rhs)) ||
    (R_IsNaN(lhs) && R_IsNaN(rhs))
);}




template <>
inline bool lessThan<Rcomplex> (const Rcomplex& lhs, const Rcomplex& rhs) 
{
    if (equalTo<double>(lhs.r , rhs.r)) return lessThan<double>(lhs.i , rhs.i);
    return lessThan<double>(lhs.r , rhs.r);
}
template <>
inline bool equalTo<Rcomplex> (const Rcomplex& lhs, const Rcomplex& rhs) 
{
    return equalTo<double>(lhs.r, rhs.r) && equalTo<double>(lhs.i, rhs.i) ;
}



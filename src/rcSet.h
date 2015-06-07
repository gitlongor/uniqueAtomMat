#include <set>

#include "CharSEXP.h"
#include "lessAndEqual.h"

template <typename T>
class rcVec {		// a row vec or a col vec from a column-major order matrix
public:
    T * x; 		// pointer to the first element
    int len;    // length of vector: ncol for row vec; nrow for col vec
    int eltShift;  // index shift between adjacent elements: nrow for row vec; 1 for col vec
    int vecShift;  // index shift between adjacent vectors: 1 for row vec; nrow for col vec
    int nVec;		// number of vectors: nrow for row vec; ncol for col vec
    
    inline bool operator< (const rcVec& rhs ) const {
        // elementwise comparison of two vectors from the end
        // assuming equalTo<T>(usually operator==) and lessThan<T> (usually operator<) defined for type T 
        T L, R;
        for(int i=len-1; i>=0; i--){
            if ( equalTo<T>(L= *(x+eltShift*i), (R= *(rhs.x+rhs.eltShift*i))) ) continue;
            return lessThan<T>(L , R);
        }
        return false;
    }
    /*
    friend inline bool operator> (const rcVec& lhs, const rcVec& rhs){return rhs < lhs;}
    friend inline bool operator<=(const rcVec& lhs, const rcVec& rhs){return !(lhs > rhs);}
    friend inline bool operator>=(const rcVec& lhs, const rcVec& rhs){return !(lhs < rhs);}
    */
};

template <typename T>
class vecSet {  // a set with key being rcVec type
public:
    rcVec<T> aRC; 
    std::set<rcVec<T> > rcvSet; // using operator< of rcVec<T>
    
    void duplicatedMat    	(const T* x, const int* nrow, const int* ncol, int* const out, bool const byRow=true, bool const fromLast=false);
    void anyDuplicatedMat   (const T* x, const int* nrow, const int* ncol, int* const out, bool const byRow=true, bool const fromLast=false);
};

template <typename T>
void vecSet<T>::duplicatedMat (const T* x, const int* nrow, const int* ncol, int* const out, bool const byRow, bool const fromLast)
{
    /* put a logical vector of duplicated rows of numeric matrix x into out */
    if(byRow){
        aRC.eltShift = aRC.nVec = (int)(*nrow);
        aRC.vecShift = 1;
        aRC.len = (int)(*ncol);
    }else{
        aRC.eltShift = 1;
        aRC.vecShift = aRC.len = (int)(*nrow);
        aRC.nVec = (int)(*ncol);
    }
    // set insert: if not previously inserted, the .second of returned pair is true; otherwise false. the .first is an iterator for the (previously) inserted element, which is not used. 
    if (fromLast) {
        aRC.x=const_cast<T*>(x) + ( byRow ? (*nrow)-1 : ((*ncol)-1)*(*nrow) ); 
        for(int i=aRC.nVec-1; i>=0; aRC.x -= aRC.vecShift)
            out[i--] = (int) !(rcvSet.insert( aRC ).second);
    }else {
        aRC.x=const_cast<T*>(x);
        for(int i=0; i<aRC.nVec; aRC.x += aRC.vecShift) 
            out[i++] = (int) !(rcvSet.insert( aRC ).second);
    }
    rcvSet.clear();
}


template <typename T>
void vecSet<T>::anyDuplicatedMat (const T* x, const int* nrow, const int* ncol, int* const out, bool const byRow, bool const fromLast)
{
    /* put a logical vector of duplicated rows of numeric matrix x into out */
    if(byRow){
        aRC.eltShift = aRC.nVec = (int)(*nrow);
        aRC.vecShift = 1;
        aRC.len = (int)(*ncol);
    }else{
        aRC.eltShift = 1;
        aRC.vecShift = aRC.len = (int)(*nrow);
        aRC.nVec = (int)(*ncol);
    }
    
    out[0] = 0; // result when no duplicates are found
    // set insert: if not previously inserted, the .second of returned pair is true; otherwise false. the .first is an iterator for the (previously) inserted element, which is not used. 
    if (fromLast) {
        aRC.x=const_cast<T*>(x) + ( byRow ? (*nrow)-1 : ((*ncol)-1)*(*nrow) ); 
        for(int i=aRC.nVec-1; i>=0; aRC.x -= aRC.vecShift, --i)
            if( !(rcvSet.insert( aRC ).second) ) {
                out[0] = i + 1;
                break;
            }
    }else {
        aRC.x=const_cast<T*>(x);
        for(int i=0; i<aRC.nVec; aRC.x += aRC.vecShift, ++i) 
            if( !(rcvSet.insert( aRC ).second) ){
                out[0] = i + 1;
                break;
            }
    }
    rcvSet.clear();
}

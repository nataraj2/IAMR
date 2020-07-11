#ifndef _NavierStokesBase_F_H_
#define _NavierStokesBase_F_H_
#include <AMReX_BLFort.H>

#ifdef __cplusplus
extern "C" 
{
#endif

    void state_error(int* tag, AMREX_ARLIM_P(tlo), AMREX_ARLIM_P(thi),
                                     const int* tagval, const int* clearval,
                                     amrex::Real* data, AMREX_ARLIM_P(data_lo), AMREX_ARLIM_P(data_hi),
                                     const int* lo, const int * hi, const int* nvar,
                                     const int* domain_lo, const int* domain_hi,
                                     const amrex::Real* dx, const amrex::Real* xlo,
                                     const amrex::Real* prob_lo, const amrex::Real* time,
                                     const int* level);

#ifdef __cplusplus
}
#endif

#endif

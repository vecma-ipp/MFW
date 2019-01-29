         parameter(nc_p=100,nf_p=300,ncf_p=nc_p+nf_p+1)
         parameter(nnc_p=25,mmc_p=25)

         common/comfit/ rfit(nf_p),zfit(nf_p),rpre(nc_p),zpre(nc_p),
     *                 psifit(nf_p),psipre(nc_p),curref(nc_p),
     *                 w_wght(nf_p),d_wght(nc_p),s_wght,
     *                 Gindk(nc_p,nf_p),Gindp(nc_p,nf_p),
     *                 Gx_r(nc_p),Gx_z(nc_p),rx_p,zx_p,dpsx_r,dpsx_z,
     *                 Ginda(nc_p),Gindx(nc_p),psip_x,psip_a,
     *                 r_ax,z_ax,alph,Lfit,Lpre

         common/comfit_ad/ Gx_rz(nc_p),Gx_zz(nc_p),Gx_rr(nc_p),
     *                     dpsx_rz,dpsx_zz,dpsx_rr
 
         common/comfit_cc/ CxC(nnc_p,mmc_p),Cpir(nnc_p,mmc_p),
     *                     g_wght(nnc_p),c_wght,n_cc,m_cc

         common/comfit_ad2x/ Gx2_r(nc_p),Gx2_z(nc_p),psip_x2,
     *                       rx2_p,zx2_p,dpsx2_r,dpsx2_z




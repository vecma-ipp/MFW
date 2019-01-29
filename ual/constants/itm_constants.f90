
!> Module implementing the ITM physics constants
!> 
!> Source:
!>  based on SOLPS b2mod_constants.F
!>    09/12/2007 xpb : source CODATA 2006 (http://www.nist.gov/)
!>    08/19/2011 xpb : source CODATA 2010 (http://www.nist.gov/)
!>  pulled from ets r100
!> 
!> \author David Coster
!> 
!> \version "$Id: itm_constants.xml 553 2012-07-13 11:21:35Z coster $"
!> 

module itm_constants


	use itm_types

	real(kind=R8), parameter :: ITM_PI	 = 3.141592653589793238462643383280_R8
	real(kind=R8), parameter :: ITM_C	 = 2.99792458e8_R8  !< speed of light [m/s]
	real(kind=R8), parameter :: ITM_ME	 = 9.10938291e-31_R8  !< electron mass [kg]
	real(kind=R8), parameter :: ITM_MP	 = 1.672621777e-27_R8  !< proton mass [kg]
	real(kind=R8), parameter :: ITM_MN	 = 1.674927351e-27_R8  !< neutron mass [kg]
	real(kind=R8), parameter :: ITM_MD	 = 3.34358348e-27_R8  !< deuteron mass [kg]
	real(kind=R8), parameter :: ITM_MT	 = 5.00735630e-27_R8  !< triton mass [kg]
	real(kind=R8), parameter :: ITM_MA	 = 6.64465675e-27_R8  !< alpha mass [kg]
	real(kind=R8), parameter :: ITM_AMU	 = 1.660538921e-27_R8  !< atomic mass unit [kg]
	real(kind=R8), parameter :: ITM_EV	 = 1.602176565e-19_R8  !< electron volt (eV)
	real(kind=R8), parameter :: ITM_QE	 = ITM_EV   !< elementary charge [coulomb]
	real(kind=R8), parameter :: ITM_MU0	 = 4.0e-7_R8 * ITM_PI   !< vacuum permeability
	real(kind=R8), parameter :: ITM_EPS0	 = 1.0_R8 / (ITM_MU0 * ITM_C * ITM_C) 
	real(kind=R8), parameter :: ITM_AVOGR	 = 6.02214129e23_R8
	real(kind=R8), parameter :: ITM_KBOLT	 = 1.3806488e-23_R8
	real(kind=R8), parameter :: ITM_MASS_H_1	 = 1.00782503207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_2	 = 2.0141017778_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_3	 = 3.0160492777_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_4	 = 4.02781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_5	 = 5.03531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_6	 = 6.04494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_H_7	 = 7.05275_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_3	 = 3.0160293191_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_4	 = 4.00260325415_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_5	 = 5.012220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_6	 = 6.0188891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_7	 = 7.028021_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_8	 = 8.033922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_9	 = 9.043950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_He_10	 = 10.052400_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_3	 = 3.03078_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_4	 = 4.02719_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_5	 = 5.012540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_6	 = 6.015122795_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_7	 = 7.01600455_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_8	 = 8.02248736_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_9	 = 9.0267895_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_10	 = 10.035481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_11	 = 11.043798_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Li_12	 = 12.05378_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_5	 = 5.04079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_6	 = 6.019726_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_7	 = 7.01692983_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_8	 = 8.00530510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_9	 = 9.0121822_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_10	 = 10.0135338_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_11	 = 11.021658_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_12	 = 12.026921_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_13	 = 13.035690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_14	 = 14.04289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_15	 = 15.05346_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Be_16	 = 16.06192_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_6	 = 6.04681_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_7	 = 7.029920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_8	 = 8.0246072_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_9	 = 9.0133288_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_10	 = 10.0129370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_11	 = 11.0093054_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_12	 = 12.0143521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_13	 = 13.0177802_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_14	 = 14.025404_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_15	 = 15.031103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_16	 = 16.039810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_17	 = 17.04699_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_18	 = 18.05617_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_B_19	 = 19.06373_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_8	 = 8.037675_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_9	 = 9.0310367_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_10	 = 10.0168532_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_11	 = 11.0114336_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_12	 = 12.0000000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_13	 = 13.0033548378_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_14	 = 14.003241989_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_15	 = 15.0105993_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_16	 = 16.014701_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_17	 = 17.022586_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_18	 = 18.026760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_19	 = 19.03481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_20	 = 20.04032_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_21	 = 21.04934_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_C_22	 = 22.05720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_10	 = 10.04165_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_11	 = 11.026090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_12	 = 12.0186132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_13	 = 13.00573861_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_14	 = 14.0030740048_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_15	 = 15.0001088982_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_16	 = 16.0061017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_17	 = 17.008450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_18	 = 18.014079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_19	 = 19.017029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_20	 = 20.023370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_21	 = 21.02711_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_22	 = 22.03439_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_23	 = 23.04122_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_24	 = 24.05104_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_N_25	 = 25.06066_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_12	 = 12.034405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_13	 = 13.024812_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_14	 = 14.00859625_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_15	 = 15.0030656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_16	 = 15.99491461956_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_17	 = 16.99913170_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_18	 = 17.9991610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_19	 = 19.003580_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_20	 = 20.0040767_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_21	 = 21.008656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_22	 = 22.009970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_23	 = 23.01569_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_24	 = 24.02047_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_25	 = 25.02946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_26	 = 26.03834_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_27	 = 27.04826_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_O_28	 = 28.05781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_14	 = 14.03506_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_15	 = 15.01801_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_16	 = 16.011466_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_17	 = 17.00209524_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_18	 = 18.0009380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_19	 = 18.99840322_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_20	 = 19.99998132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_21	 = 20.9999490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_22	 = 22.002999_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_23	 = 23.003570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_24	 = 24.008120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_25	 = 25.01210_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_26	 = 26.01962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_27	 = 27.02676_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_28	 = 28.03567_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_29	 = 29.04326_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_30	 = 30.05250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_F_31	 = 31.06043_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_16	 = 16.025761_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_17	 = 17.017672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_18	 = 18.0057082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_19	 = 19.0018802_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_20	 = 19.9924401754_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_21	 = 20.99384668_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_22	 = 21.991385114_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_23	 = 22.99446690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_24	 = 23.9936108_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_25	 = 24.997737_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_26	 = 26.000461_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_27	 = 27.00759_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_28	 = 28.01207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_29	 = 29.01939_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_30	 = 30.02480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_31	 = 31.03311_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_32	 = 32.04002_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_33	 = 33.04938_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ne_34	 = 34.05703_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_18	 = 18.025970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_19	 = 19.013877_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_20	 = 20.007351_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_21	 = 20.9976552_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_22	 = 21.9944364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_23	 = 22.9897692809_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_24	 = 23.99096278_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_25	 = 24.9899540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_26	 = 25.992633_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_27	 = 26.994077_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_28	 = 27.998938_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_29	 = 29.002861_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_30	 = 30.008976_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_31	 = 31.01359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_32	 = 32.02047_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_33	 = 33.02672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_34	 = 34.03517_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_35	 = 35.04249_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_36	 = 36.05148_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Na_37	 = 37.05934_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_19	 = 19.03547_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_20	 = 20.018863_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_21	 = 21.011713_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_22	 = 21.9995738_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_23	 = 22.9941237_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_24	 = 23.985041700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_25	 = 24.98583692_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_26	 = 25.982592929_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_27	 = 26.98434059_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_28	 = 27.9838768_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_29	 = 28.988600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_30	 = 29.990434_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_31	 = 30.996546_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_32	 = 31.998975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_33	 = 33.005254_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_34	 = 34.00946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_35	 = 35.01734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_36	 = 36.02300_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_37	 = 37.03140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_38	 = 38.03757_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_39	 = 39.04677_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mg_40	 = 40.05393_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_21	 = 21.02804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_22	 = 22.01952_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_23	 = 23.007267_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_24	 = 23.9999389_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_25	 = 24.9904281_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_26	 = 25.98689169_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_27	 = 26.98153863_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_28	 = 27.98191031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_29	 = 28.9804450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_30	 = 29.982960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_31	 = 30.983947_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_32	 = 31.988120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_33	 = 32.990840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_34	 = 33.99685_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_35	 = 34.99986_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_36	 = 36.00621_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_37	 = 37.01068_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_38	 = 38.01723_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_39	 = 39.02297_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_40	 = 40.03145_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_41	 = 41.03833_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Al_42	 = 42.04689_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_22	 = 22.03453_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_23	 = 23.02552_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_24	 = 24.011546_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_25	 = 25.004106_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_26	 = 25.992330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_27	 = 26.98670491_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_28	 = 27.9769265325_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_29	 = 28.976494700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_30	 = 29.97377017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_31	 = 30.97536323_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_32	 = 31.97414808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_33	 = 32.978000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_34	 = 33.978576_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_35	 = 34.984580_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_36	 = 35.98660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_37	 = 36.99294_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_38	 = 37.99563_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_39	 = 39.00207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_40	 = 40.00587_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_41	 = 41.01456_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_42	 = 42.01979_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_43	 = 43.02866_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Si_44	 = 44.03526_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_24	 = 24.03435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_25	 = 25.02026_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_26	 = 26.01178_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_27	 = 26.999230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_28	 = 27.992315_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_29	 = 28.9818006_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_30	 = 29.9783138_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_31	 = 30.97376163_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_32	 = 31.97390727_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_33	 = 32.9717255_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_34	 = 33.973636_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_35	 = 34.9733141_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_36	 = 35.978260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_37	 = 36.979610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_38	 = 37.98416_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_39	 = 38.98618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_40	 = 39.99130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_41	 = 40.99434_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_42	 = 42.00101_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_43	 = 43.00619_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_44	 = 44.01299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_45	 = 45.01922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_P_46	 = 46.02738_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_26	 = 26.02788_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_27	 = 27.01883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_28	 = 28.00437_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_29	 = 28.996610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_30	 = 29.984903_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_31	 = 30.9795547_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_32	 = 31.97207100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_33	 = 32.97145876_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_34	 = 33.96786690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_35	 = 34.96903216_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_36	 = 35.96708076_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_37	 = 36.97112557_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_38	 = 37.971163_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_39	 = 38.975130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_40	 = 39.97545_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_41	 = 40.97958_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_42	 = 41.98102_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_43	 = 42.98715_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_44	 = 43.99021_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_45	 = 44.99651_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_46	 = 46.00075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_47	 = 47.00859_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_48	 = 48.01417_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_S_49	 = 49.02362_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_28	 = 28.02851_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_29	 = 29.01411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_30	 = 30.00477_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_31	 = 30.992410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_32	 = 31.985690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_33	 = 32.9774519_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_34	 = 33.97376282_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_35	 = 34.96885268_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_36	 = 35.96830698_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_37	 = 36.96590259_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_38	 = 37.96801043_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_39	 = 38.9680082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_40	 = 39.970420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_41	 = 40.970680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_42	 = 41.97325_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_43	 = 42.97405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_44	 = 43.97828_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_45	 = 44.98029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_46	 = 45.98421_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_47	 = 46.98871_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_48	 = 47.99495_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_49	 = 49.00032_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_50	 = 50.00784_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cl_51	 = 51.01449_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_30	 = 30.02156_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_31	 = 31.01212_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_32	 = 31.9976380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_33	 = 32.9899257_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_34	 = 33.9802712_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_35	 = 34.9752576_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_36	 = 35.967545106_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_37	 = 36.96677632_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_38	 = 37.9627324_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_39	 = 38.964313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_40	 = 39.9623831225_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_41	 = 40.9645006_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_42	 = 41.963046_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_43	 = 42.965636_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_44	 = 43.9649240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_45	 = 44.9680400_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_46	 = 45.968090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_47	 = 46.97219_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_48	 = 47.97454_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_49	 = 48.98052_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_50	 = 49.98443_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_51	 = 50.99163_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_52	 = 51.99678_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ar_53	 = 53.00494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_32	 = 32.02192_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_33	 = 33.00726_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_34	 = 33.99841_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_35	 = 34.988010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_36	 = 35.981292_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_37	 = 36.97337589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_38	 = 37.9690812_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_39	 = 38.96370668_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_40	 = 39.96399848_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_41	 = 40.96182576_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_42	 = 41.96240281_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_43	 = 42.960716_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_44	 = 43.961560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_45	 = 44.960699_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_46	 = 45.961977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_47	 = 46.961678_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_48	 = 47.965514_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_49	 = 48.967450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_50	 = 49.97278_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_51	 = 50.97638_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_52	 = 51.98261_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_53	 = 52.98712_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_54	 = 53.99420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_K_55	 = 54.99971_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_34	 = 34.01412_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_35	 = 35.00494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_36	 = 35.993090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_37	 = 36.985870_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_38	 = 37.976318_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_39	 = 38.9707197_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_40	 = 39.96259098_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_41	 = 40.96227806_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_42	 = 41.95861801_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_43	 = 42.9587666_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_44	 = 43.9554818_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_45	 = 44.9561866_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_46	 = 45.9536926_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_47	 = 46.9545460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_48	 = 47.952534_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_49	 = 48.955674_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_50	 = 49.957519_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_51	 = 50.96150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_52	 = 51.96510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_53	 = 52.97005_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_54	 = 53.97435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_55	 = 54.98055_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_56	 = 55.98557_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ca_57	 = 56.99236_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_36	 = 36.01492_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_37	 = 37.00305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_38	 = 37.99470_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_39	 = 38.984790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_40	 = 39.977967_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_41	 = 40.96925113_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_42	 = 41.96551643_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_43	 = 42.9611507_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_44	 = 43.9594028_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_45	 = 44.9559119_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_46	 = 45.9551719_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_47	 = 46.9524075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_48	 = 47.952231_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_49	 = 48.950024_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_50	 = 49.952188_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_51	 = 50.953603_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_52	 = 51.95668_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_53	 = 52.95961_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_54	 = 53.96326_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_55	 = 54.96824_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_56	 = 55.97287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_57	 = 56.97779_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_58	 = 57.98371_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_59	 = 58.98922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sc_60	 = 59.99571_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_38	 = 38.00977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_39	 = 39.00161_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_40	 = 39.99050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_41	 = 40.98315_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_42	 = 41.973031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_43	 = 42.968522_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_44	 = 43.9596901_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_45	 = 44.9581256_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_46	 = 45.9526316_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_47	 = 46.9517631_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_48	 = 47.9479463_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_49	 = 48.9478700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_50	 = 49.9447912_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_51	 = 50.9466150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_52	 = 51.946897_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_53	 = 52.94973_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_54	 = 53.95105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_55	 = 54.95527_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_56	 = 55.95820_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_57	 = 56.96399_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_58	 = 57.96697_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_59	 = 58.97293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_60	 = 59.97676_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_61	 = 60.98320_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_62	 = 61.98749_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ti_63	 = 62.99442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_40	 = 40.01109_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_41	 = 40.99978_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_42	 = 41.99123_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_43	 = 42.98065_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_44	 = 43.97411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_45	 = 44.965776_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_46	 = 45.9602005_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_47	 = 46.9549089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_48	 = 47.9522537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_49	 = 48.9485161_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_50	 = 49.9471585_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_51	 = 50.9439595_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_52	 = 51.9447755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_53	 = 52.944338_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_54	 = 53.946440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_55	 = 54.94723_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_56	 = 55.95053_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_57	 = 56.95256_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_58	 = 57.95683_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_59	 = 58.96021_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_60	 = 59.96503_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_61	 = 60.96848_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_62	 = 61.97378_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_63	 = 62.97755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_64	 = 63.98347_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_V_65	 = 64.98792_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_42	 = 42.00643_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_43	 = 42.99771_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_44	 = 43.985550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_45	 = 44.97964_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_46	 = 45.968359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_47	 = 46.962900_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_48	 = 47.954032_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_49	 = 48.9513357_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_50	 = 49.9460442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_51	 = 50.9447674_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_52	 = 51.9405075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_53	 = 52.9406494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_54	 = 53.9388804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_55	 = 54.9408397_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_56	 = 55.9406531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_57	 = 56.9436130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_58	 = 57.94435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_59	 = 58.94859_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_60	 = 59.95008_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_61	 = 60.95472_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_62	 = 61.95661_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_63	 = 62.96186_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_64	 = 63.96441_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_65	 = 64.97016_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_66	 = 65.97338_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cr_67	 = 66.97955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_44	 = 44.00687_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_45	 = 44.99451_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_46	 = 45.98672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_47	 = 46.97610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_48	 = 47.96852_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_49	 = 48.959618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_50	 = 49.9542382_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_51	 = 50.9482108_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_52	 = 51.9455655_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_53	 = 52.9412901_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_54	 = 53.9403589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_55	 = 54.9380451_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_56	 = 55.9389049_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_57	 = 56.9382854_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_58	 = 57.939980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_59	 = 58.940440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_60	 = 59.942910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_61	 = 60.94465_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_62	 = 61.94843_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_63	 = 62.95024_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_64	 = 63.95425_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_65	 = 64.95634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_66	 = 65.96108_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_67	 = 66.96414_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_68	 = 67.96930_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mn_69	 = 68.97284_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_45	 = 45.01458_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_46	 = 46.00081_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_47	 = 46.99289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_48	 = 47.980500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_49	 = 48.97361_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_50	 = 49.962990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_51	 = 50.956820_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_52	 = 51.948114_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_53	 = 52.9453079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_54	 = 53.9396105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_55	 = 54.9382934_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_56	 = 55.9349375_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_57	 = 56.9353940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_58	 = 57.9332756_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_59	 = 58.9348755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_60	 = 59.934072_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_61	 = 60.936745_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_62	 = 61.936767_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_63	 = 62.94037_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_64	 = 63.94120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_65	 = 64.94538_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_66	 = 65.94678_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_67	 = 66.95095_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_68	 = 67.95370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_69	 = 68.95878_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_70	 = 69.96146_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_71	 = 70.96672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fe_72	 = 71.96962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_47	 = 47.01149_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_48	 = 48.00176_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_49	 = 48.98972_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_50	 = 49.98154_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_51	 = 50.97072_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_52	 = 51.963590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_53	 = 52.954219_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_54	 = 53.9484596_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_55	 = 54.9419990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_56	 = 55.9398393_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_57	 = 56.9362914_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_58	 = 57.9357528_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_59	 = 58.9331950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_60	 = 59.9338171_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_61	 = 60.9324758_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_62	 = 61.934051_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_63	 = 62.933612_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_64	 = 63.935810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_65	 = 64.936478_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_66	 = 65.93976_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_67	 = 66.94089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_68	 = 67.94487_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_69	 = 68.94632_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_70	 = 69.95100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_71	 = 70.95290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_72	 = 71.95781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_73	 = 72.96024_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_74	 = 73.96538_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Co_75	 = 74.96833_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_48	 = 48.01975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_49	 = 49.00966_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_50	 = 49.99593_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_51	 = 50.98772_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_52	 = 51.975680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_53	 = 52.96847_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_54	 = 53.957910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_55	 = 54.951330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_56	 = 55.942132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_57	 = 56.9397935_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_58	 = 57.9353429_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_59	 = 58.9343467_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_60	 = 59.9307864_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_61	 = 60.9310560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_62	 = 61.9283451_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_63	 = 62.9296694_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_64	 = 63.9279660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_65	 = 64.9300843_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_66	 = 65.9291393_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_67	 = 66.931569_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_68	 = 67.931869_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_69	 = 68.935610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_70	 = 69.93650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_71	 = 70.94074_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_72	 = 71.94209_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_73	 = 72.94647_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_74	 = 73.94807_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_75	 = 74.95287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_76	 = 75.95533_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_77	 = 76.96055_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ni_78	 = 77.96318_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_52	 = 51.99718_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_53	 = 52.98555_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_54	 = 53.97671_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_55	 = 54.96605_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_56	 = 55.95856_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_57	 = 56.949211_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_58	 = 57.9445385_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_59	 = 58.9394980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_60	 = 59.9373650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_61	 = 60.9334578_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_62	 = 61.932584_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_63	 = 62.9295975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_64	 = 63.9297642_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_65	 = 64.9277895_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_66	 = 65.9288688_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_67	 = 66.9277303_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_68	 = 67.9296109_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_69	 = 68.9294293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_70	 = 69.9323923_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_71	 = 70.9326768_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_72	 = 71.9358203_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_73	 = 72.936675_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_74	 = 73.939875_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_75	 = 74.94190_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_76	 = 75.945275_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_77	 = 76.94785_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_78	 = 77.95196_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_79	 = 78.95456_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cu_80	 = 79.96087_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_54	 = 53.99295_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_55	 = 54.98398_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_56	 = 55.97238_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_57	 = 56.96479_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_58	 = 57.954590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_59	 = 58.949260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_60	 = 59.941827_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_61	 = 60.939511_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_62	 = 61.934330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_63	 = 62.9332116_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_64	 = 63.9291422_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_65	 = 64.9292410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_66	 = 65.9260334_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_67	 = 66.9271273_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_68	 = 67.9248442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_69	 = 68.9265503_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_70	 = 69.9253193_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_71	 = 70.927722_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_72	 = 71.926858_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_73	 = 72.929780_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_74	 = 73.929460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_75	 = 74.932940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_76	 = 75.933290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_77	 = 76.93696_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_78	 = 77.93844_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_79	 = 78.94265_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_80	 = 79.94434_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_81	 = 80.95048_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_82	 = 81.95442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zn_83	 = 82.96103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_56	 = 55.99491_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_57	 = 56.98293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_58	 = 57.97425_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_59	 = 58.96337_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_60	 = 59.95706_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_61	 = 60.949450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_62	 = 61.944175_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_63	 = 62.9392942_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_64	 = 63.9368387_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_65	 = 64.9327348_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_66	 = 65.931589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_67	 = 66.9282017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_68	 = 67.9279801_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_69	 = 68.9255736_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_70	 = 69.9260220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_71	 = 70.9247013_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_72	 = 71.9263663_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_73	 = 72.9251747_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_74	 = 73.926946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_75	 = 74.9265002_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_76	 = 75.9288276_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_77	 = 76.9291543_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_78	 = 77.9316082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_79	 = 78.93289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_80	 = 79.93652_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_81	 = 80.93775_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_82	 = 81.94299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_83	 = 82.94698_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_84	 = 83.95265_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_85	 = 84.95700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ga_86	 = 85.96312_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_58	 = 57.99101_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_59	 = 58.98175_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_60	 = 59.97019_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_61	 = 60.96379_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_62	 = 61.95465_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_63	 = 62.94964_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_64	 = 63.941650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_65	 = 64.93944_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_66	 = 65.933840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_67	 = 66.932734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_68	 = 67.928094_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_69	 = 68.9279645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_70	 = 69.9242474_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_71	 = 70.9249510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_72	 = 71.9220758_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_73	 = 72.9234589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_74	 = 73.9211778_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_75	 = 74.9228589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_76	 = 75.9214026_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_77	 = 76.9235486_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_78	 = 77.922853_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_79	 = 78.92540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_80	 = 79.925370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_81	 = 80.92882_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_82	 = 81.92955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_83	 = 82.93462_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_84	 = 83.93747_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_85	 = 84.94303_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_86	 = 85.94649_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_87	 = 86.95251_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_88	 = 87.95691_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ge_89	 = 88.96383_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_60	 = 59.99313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_61	 = 60.98062_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_62	 = 61.97320_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_63	 = 62.96369_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_64	 = 63.95757_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_65	 = 64.94956_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_66	 = 65.94471_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_67	 = 66.93919_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_68	 = 67.936770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_69	 = 68.932270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_70	 = 69.930920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_71	 = 70.927112_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_72	 = 71.926752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_73	 = 72.923825_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_74	 = 73.9239287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_75	 = 74.9215965_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_76	 = 75.9223940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_77	 = 76.9206473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_78	 = 77.921827_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_79	 = 78.920948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_80	 = 79.922534_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_81	 = 80.922132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_82	 = 81.92450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_83	 = 82.92498_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_84	 = 83.92906_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_85	 = 84.93202_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_86	 = 85.93650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_87	 = 86.93990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_88	 = 87.94494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_89	 = 88.94939_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_90	 = 89.95550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_91	 = 90.96043_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_As_92	 = 91.96680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_65	 = 64.96466_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_66	 = 65.95521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_67	 = 66.95009_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_68	 = 67.941800_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_69	 = 68.939560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_70	 = 69.933390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_71	 = 70.932240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_72	 = 71.927112_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_73	 = 72.926765_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_74	 = 73.9224764_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_75	 = 74.9225234_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_76	 = 75.9192136_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_77	 = 76.9199140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_78	 = 77.9173091_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_79	 = 78.9184991_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_80	 = 79.9165213_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_81	 = 80.9179925_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_82	 = 81.9166994_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_83	 = 82.919118_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_84	 = 83.918462_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_85	 = 84.922250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_86	 = 85.924272_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_87	 = 86.928520_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_88	 = 87.931420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_89	 = 88.93645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_90	 = 89.93996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_91	 = 90.94596_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_92	 = 91.94992_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_93	 = 92.95629_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Se_94	 = 93.96049_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_67	 = 66.96479_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_68	 = 67.95852_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_69	 = 68.95011_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_70	 = 69.94479_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_71	 = 70.93874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_72	 = 71.936640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_73	 = 72.931690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_74	 = 73.929891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_75	 = 74.925776_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_76	 = 75.924541_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_77	 = 76.921379_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_78	 = 77.921146_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_79	 = 78.9183371_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_80	 = 79.9185293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_81	 = 80.9162906_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_82	 = 81.9168041_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_83	 = 82.915180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_84	 = 83.916479_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_85	 = 84.915608_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_86	 = 85.918798_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_87	 = 86.920711_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_88	 = 87.924070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_89	 = 88.926390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_90	 = 89.930630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_91	 = 90.933970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_92	 = 91.939260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_93	 = 92.94305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_94	 = 93.94868_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_95	 = 94.95287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_96	 = 95.95853_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Br_97	 = 96.96280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_69	 = 68.96518_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_70	 = 69.95526_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_71	 = 70.94963_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_72	 = 71.942092_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_73	 = 72.939289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_74	 = 73.9330844_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_75	 = 74.930946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_76	 = 75.925910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_77	 = 76.9246700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_78	 = 77.9203648_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_79	 = 78.920082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_80	 = 79.9163790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_81	 = 80.9165920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_82	 = 81.9134836_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_83	 = 82.914136_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_84	 = 83.911507_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_85	 = 84.9125273_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_86	 = 85.91061073_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_87	 = 86.91335486_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_88	 = 87.914447_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_89	 = 88.917630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_90	 = 89.919517_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_91	 = 90.923450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_92	 = 91.926156_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_93	 = 92.93127_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_94	 = 93.93436_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_95	 = 94.93984_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_96	 = 95.94307_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_97	 = 96.94856_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_98	 = 97.95191_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_99	 = 98.95760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Kr_100	 = 99.96114_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_71	 = 70.96532_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_72	 = 71.95908_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_73	 = 72.95056_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_74	 = 73.944265_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_75	 = 74.938570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_76	 = 75.9350722_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_77	 = 76.930408_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_78	 = 77.928141_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_79	 = 78.923989_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_80	 = 79.922519_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_81	 = 80.918996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_82	 = 81.9182086_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_83	 = 82.915110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_84	 = 83.914385_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_85	 = 84.911789738_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_86	 = 85.91116742_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_87	 = 86.909180527_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_88	 = 87.91131559_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_89	 = 88.912278_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_90	 = 89.914802_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_91	 = 90.916537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_92	 = 91.919729_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_93	 = 92.922042_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_94	 = 93.926405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_95	 = 94.929303_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_96	 = 95.934270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_97	 = 96.937350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_98	 = 97.941790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_99	 = 98.94538_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_100	 = 99.94987_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_101	 = 100.95320_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rb_102	 = 101.95887_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_73	 = 72.96597_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_74	 = 73.95631_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_75	 = 74.94995_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_76	 = 75.941770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_77	 = 76.937945_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_78	 = 77.932180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_79	 = 78.929708_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_80	 = 79.924521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_81	 = 80.923212_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_82	 = 81.918402_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_83	 = 82.917557_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_84	 = 83.913425_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_85	 = 84.912933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_86	 = 85.9092602_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_87	 = 86.9088771_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_88	 = 87.9056121_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_89	 = 88.9074507_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_90	 = 89.907738_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_91	 = 90.910203_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_92	 = 91.911038_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_93	 = 92.914026_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_94	 = 93.915361_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_95	 = 94.919359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_96	 = 95.921697_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_97	 = 96.926153_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_98	 = 97.928453_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_99	 = 98.933240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_100	 = 99.93535_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_101	 = 100.94052_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_102	 = 101.94302_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_103	 = 102.94895_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_104	 = 103.95233_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sr_105	 = 104.95858_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_76	 = 75.95845_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_77	 = 76.949650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_78	 = 77.94361_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_79	 = 78.93735_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_80	 = 79.93428_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_81	 = 80.929130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_82	 = 81.92679_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_83	 = 82.922350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_84	 = 83.92039_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_85	 = 84.916433_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_86	 = 85.914886_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_87	 = 86.9108757_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_88	 = 87.9095011_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_89	 = 88.9058483_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_90	 = 89.9071519_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_91	 = 90.907305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_92	 = 91.908949_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_93	 = 92.909583_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_94	 = 93.911595_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_95	 = 94.912821_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_96	 = 95.915891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_97	 = 96.918134_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_98	 = 97.922203_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_99	 = 98.924636_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_100	 = 99.927760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_101	 = 100.93031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_102	 = 101.933560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_103	 = 102.93673_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_104	 = 103.94105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_105	 = 104.94487_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_106	 = 105.94979_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_107	 = 106.95414_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Y_108	 = 107.95948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_78	 = 77.95523_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_79	 = 78.94916_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_80	 = 79.94040_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_81	 = 80.93721_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_82	 = 81.93109_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_83	 = 82.92865_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_84	 = 83.92325_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_85	 = 84.92147_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_86	 = 85.916470_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_87	 = 86.914816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_88	 = 87.910227_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_89	 = 88.908890_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_90	 = 89.9047044_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_91	 = 90.9056458_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_92	 = 91.9050408_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_93	 = 92.9064760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_94	 = 93.9063152_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_95	 = 94.9080426_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_96	 = 95.9082734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_97	 = 96.9109531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_98	 = 97.912735_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_99	 = 98.916512_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_100	 = 99.917760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_101	 = 100.921140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_102	 = 101.922980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_103	 = 102.92660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_104	 = 103.92878_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_105	 = 104.93305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_106	 = 105.93591_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_107	 = 106.94075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_108	 = 107.94396_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_109	 = 108.94924_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Zr_110	 = 109.95287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_81	 = 80.94903_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_82	 = 81.94313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_83	 = 82.93671_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_84	 = 83.93357_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_85	 = 84.92791_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_86	 = 85.925040_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_87	 = 86.920360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_88	 = 87.91833_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_89	 = 88.913418_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_90	 = 89.911265_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_91	 = 90.906996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_92	 = 91.907194_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_93	 = 92.9063781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_94	 = 93.9072839_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_95	 = 94.9068358_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_96	 = 95.908101_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_97	 = 96.9080986_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_98	 = 97.910328_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_99	 = 98.911618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_100	 = 99.914182_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_101	 = 100.915252_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_102	 = 101.918040_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_103	 = 102.919140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_104	 = 103.92246_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_105	 = 104.92394_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_106	 = 105.92797_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_107	 = 106.93031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_108	 = 107.93484_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_109	 = 108.93763_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_110	 = 109.94244_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_111	 = 110.94565_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_112	 = 111.95083_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nb_113	 = 112.95470_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_83	 = 82.94874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_84	 = 83.94009_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_85	 = 84.93655_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_86	 = 85.93070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_87	 = 86.92733_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_88	 = 87.921953_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_89	 = 88.919480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_90	 = 89.913937_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_91	 = 90.911750_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_92	 = 91.906811_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_93	 = 92.906813_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_94	 = 93.9050883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_95	 = 94.9058421_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_96	 = 95.9046795_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_97	 = 96.9060215_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_98	 = 97.9054082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_99	 = 98.9077119_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_100	 = 99.907477_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_101	 = 100.910347_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_102	 = 101.910297_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_103	 = 102.913210_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_104	 = 103.913760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_105	 = 104.916970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_106	 = 105.918137_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_107	 = 106.92169_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_108	 = 107.92345_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_109	 = 108.92781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_110	 = 109.92973_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_111	 = 110.93441_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_112	 = 111.93684_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_113	 = 112.94188_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_114	 = 113.94492_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mo_115	 = 114.95029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_85	 = 84.94883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_86	 = 85.94288_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_87	 = 86.93653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_88	 = 87.93268_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_89	 = 88.92717_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_90	 = 89.92356_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_91	 = 90.91843_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_92	 = 91.915260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_93	 = 92.910249_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_94	 = 93.909657_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_95	 = 94.907657_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_96	 = 95.907871_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_97	 = 96.906365_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_98	 = 97.907216_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_99	 = 98.9062547_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_100	 = 99.9076578_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_101	 = 100.907315_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_102	 = 101.909215_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_103	 = 102.909181_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_104	 = 103.911450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_105	 = 104.911660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_106	 = 105.914358_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_107	 = 106.91508_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_108	 = 107.91846_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_109	 = 108.91998_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_110	 = 109.923820_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_111	 = 110.92569_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_112	 = 111.92915_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_113	 = 112.93159_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_114	 = 113.93588_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_115	 = 114.93869_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_116	 = 115.94337_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_117	 = 116.94648_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tc_118	 = 117.95148_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_87	 = 86.94918_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_88	 = 87.94026_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_89	 = 88.93611_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_90	 = 89.92989_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_91	 = 90.92629_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_92	 = 91.92012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_93	 = 92.917050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_94	 = 93.911360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_95	 = 94.910413_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_96	 = 95.907598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_97	 = 96.907555_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_98	 = 97.905287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_99	 = 98.9059393_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_100	 = 99.9042195_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_101	 = 100.9055821_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_102	 = 101.9043493_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_103	 = 102.9063238_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_104	 = 103.905433_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_105	 = 104.907753_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_106	 = 105.907329_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_107	 = 106.90991_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_108	 = 107.91017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_109	 = 108.913200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_110	 = 109.914140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_111	 = 110.917700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_112	 = 111.918970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_113	 = 112.922490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_114	 = 113.92428_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_115	 = 114.92869_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_116	 = 115.93081_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_117	 = 116.93558_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_118	 = 117.93782_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_119	 = 118.94284_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ru_120	 = 119.94531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_89	 = 88.94884_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_90	 = 89.94287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_91	 = 90.93655_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_92	 = 91.93198_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_93	 = 92.92574_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_94	 = 93.92170_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_95	 = 94.91590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_96	 = 95.914461_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_97	 = 96.911340_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_98	 = 97.910708_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_99	 = 98.908132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_100	 = 99.908122_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_101	 = 100.906164_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_102	 = 101.906843_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_103	 = 102.905504_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_104	 = 103.906656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_105	 = 104.905694_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_106	 = 105.907287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_107	 = 106.906748_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_108	 = 107.90873_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_109	 = 108.908737_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_110	 = 109.911140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_111	 = 110.911590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_112	 = 111.914390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_113	 = 112.915530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_114	 = 113.91881_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_115	 = 114.920330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_116	 = 115.92406_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_117	 = 116.92598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_118	 = 117.93007_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_119	 = 118.93211_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_120	 = 119.93641_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_121	 = 120.93872_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rh_122	 = 121.94321_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_91	 = 90.94911_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_92	 = 91.94042_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_93	 = 92.93591_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_94	 = 93.92877_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_95	 = 94.92469_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_96	 = 95.91816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_97	 = 96.91648_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_98	 = 97.912721_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_99	 = 98.911768_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_100	 = 99.908506_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_101	 = 100.908289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_102	 = 101.905609_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_103	 = 102.906087_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_104	 = 103.904036_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_105	 = 104.905085_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_106	 = 105.903486_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_107	 = 106.905133_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_108	 = 107.903892_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_109	 = 108.905950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_110	 = 109.905153_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_111	 = 110.907671_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_112	 = 111.907314_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_113	 = 112.910150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_114	 = 113.910363_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_115	 = 114.913680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_116	 = 115.914160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_117	 = 116.917840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_118	 = 117.91898_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_119	 = 118.92311_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_120	 = 119.92469_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_121	 = 120.92887_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_122	 = 121.93055_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_123	 = 122.93493_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pd_124	 = 123.93688_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_93	 = 92.94978_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_94	 = 93.94278_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_95	 = 94.93548_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_96	 = 95.93068_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_97	 = 96.92397_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_98	 = 97.921570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_99	 = 98.91760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_100	 = 99.916100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_101	 = 100.91280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_102	 = 101.911690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_103	 = 102.908973_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_104	 = 103.908629_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_105	 = 104.906529_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_106	 = 105.906669_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_107	 = 106.905097_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_108	 = 107.905956_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_109	 = 108.904752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_110	 = 109.906107_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_111	 = 110.905291_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_112	 = 111.907005_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_113	 = 112.906567_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_114	 = 113.908804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_115	 = 114.908760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_116	 = 115.911360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_117	 = 116.911680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_118	 = 117.914580_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_119	 = 118.91567_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_120	 = 119.918790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_121	 = 120.91985_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_122	 = 121.92353_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_123	 = 122.92490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_124	 = 123.92864_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_125	 = 124.93043_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_126	 = 125.93450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_127	 = 126.93677_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_128	 = 127.94117_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_129	 = 128.94369_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ag_130	 = 129.95045_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_95	 = 94.94987_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_96	 = 95.93977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_97	 = 96.93494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_98	 = 97.927400_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_99	 = 98.92501_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_100	 = 99.92029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_101	 = 100.91868_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_102	 = 101.914460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_103	 = 102.913419_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_104	 = 103.909849_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_105	 = 104.909468_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_106	 = 105.906459_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_107	 = 106.906618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_108	 = 107.904184_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_109	 = 108.904982_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_110	 = 109.9030021_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_111	 = 110.9041781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_112	 = 111.9027578_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_113	 = 112.9044017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_114	 = 113.9033585_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_115	 = 114.9054310_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_116	 = 115.904756_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_117	 = 116.907219_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_118	 = 117.906915_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_119	 = 118.909920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_120	 = 119.909850_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_121	 = 120.912980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_122	 = 121.913330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_123	 = 122.917000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_124	 = 123.917650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_125	 = 124.921250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_126	 = 125.922350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_127	 = 126.926440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_128	 = 127.92776_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_129	 = 128.93215_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_130	 = 129.93390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_131	 = 130.94067_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cd_132	 = 131.94555_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_97	 = 96.94954_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_98	 = 97.94214_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_99	 = 98.93422_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_100	 = 99.93111_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_101	 = 100.92634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_102	 = 101.92409_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_103	 = 102.919914_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_104	 = 103.918300_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_105	 = 104.914674_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_106	 = 105.913465_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_107	 = 106.910295_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_108	 = 107.909698_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_109	 = 108.907151_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_110	 = 109.907165_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_111	 = 110.905103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_112	 = 111.905532_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_113	 = 112.904058_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_114	 = 113.904914_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_115	 = 114.903878_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_116	 = 115.905260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_117	 = 116.904514_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_118	 = 117.906354_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_119	 = 118.905845_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_120	 = 119.907960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_121	 = 120.907846_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_122	 = 121.910280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_123	 = 122.910438_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_124	 = 123.913180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_125	 = 124.913600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_126	 = 125.916460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_127	 = 126.917350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_128	 = 127.920170_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_129	 = 128.921700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_130	 = 129.924970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_131	 = 130.926850_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_132	 = 131.932990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_133	 = 132.93781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_134	 = 133.94415_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_In_135	 = 134.94933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_99	 = 98.94933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_100	 = 99.93904_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_101	 = 100.93606_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_102	 = 101.93030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_103	 = 102.92810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_104	 = 103.92314_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_105	 = 104.921350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_106	 = 105.916880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_107	 = 106.915640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_108	 = 107.911925_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_109	 = 108.911283_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_110	 = 109.907843_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_111	 = 110.907734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_112	 = 111.904818_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_113	 = 112.905171_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_114	 = 113.902779_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_115	 = 114.903342_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_116	 = 115.901741_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_117	 = 116.902952_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_118	 = 117.901603_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_119	 = 118.903308_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_120	 = 119.9021947_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_121	 = 120.9042355_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_122	 = 121.9034390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_123	 = 122.9057208_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_124	 = 123.9052739_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_125	 = 124.9077841_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_126	 = 125.907653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_127	 = 126.910360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_128	 = 127.910537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_129	 = 128.913480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_130	 = 129.913967_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_131	 = 130.917000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_132	 = 131.917816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_133	 = 132.923830_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_134	 = 133.92829_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_135	 = 134.93473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_136	 = 135.93934_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sn_137	 = 136.94599_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_103	 = 102.93969_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_104	 = 103.93647_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_105	 = 104.93149_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_106	 = 105.92879_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_107	 = 106.92415_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_108	 = 107.92216_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_109	 = 108.918132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_110	 = 109.91675_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_111	 = 110.913160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_112	 = 111.912398_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_113	 = 112.909372_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_114	 = 113.909270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_115	 = 114.906598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_116	 = 115.906794_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_117	 = 116.904836_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_118	 = 117.905529_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_119	 = 118.903942_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_120	 = 119.905072_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_121	 = 120.9038157_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_122	 = 121.9051737_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_123	 = 122.9042140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_124	 = 123.9059357_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_125	 = 124.9052538_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_126	 = 125.907250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_127	 = 126.906924_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_128	 = 127.909169_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_129	 = 128.909148_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_130	 = 129.911656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_131	 = 130.911982_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_132	 = 131.914467_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_133	 = 132.915252_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_134	 = 133.920380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_135	 = 134.92517_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_136	 = 135.93035_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_137	 = 136.93531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_138	 = 137.94079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sb_139	 = 138.94598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_105	 = 104.94364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_106	 = 105.93750_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_107	 = 106.93501_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_108	 = 107.92944_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_109	 = 108.927420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_110	 = 109.922410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_111	 = 110.921110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_112	 = 111.91701_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_113	 = 112.915890_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_114	 = 113.912090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_115	 = 114.911900_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_116	 = 115.908460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_117	 = 116.908645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_118	 = 117.905828_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_119	 = 118.906404_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_120	 = 119.904020_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_121	 = 120.904936_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_122	 = 121.9030439_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_123	 = 122.9042700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_124	 = 123.9028179_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_125	 = 124.9044307_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_126	 = 125.9033117_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_127	 = 126.9052263_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_128	 = 127.9044631_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_129	 = 128.9065982_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_130	 = 129.9062244_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_131	 = 130.9085239_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_132	 = 131.908553_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_133	 = 132.910955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_134	 = 133.911369_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_135	 = 134.91645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_136	 = 135.920100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_137	 = 136.92532_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_138	 = 137.92922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_139	 = 138.93473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_140	 = 139.93885_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_141	 = 140.94465_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Te_142	 = 141.94908_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_108	 = 107.94348_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_109	 = 108.93815_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_110	 = 109.93524_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_111	 = 110.93028_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_112	 = 111.92797_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_113	 = 112.923640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_114	 = 113.92185_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_115	 = 114.918050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_116	 = 115.91681_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_117	 = 116.913650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_118	 = 117.913074_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_119	 = 118.910070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_120	 = 119.910048_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_121	 = 120.907367_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_122	 = 121.907589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_123	 = 122.905589_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_124	 = 123.9062099_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_125	 = 124.9046302_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_126	 = 125.905624_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_127	 = 126.904473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_128	 = 127.905809_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_129	 = 128.904988_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_130	 = 129.906674_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_131	 = 130.9061246_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_132	 = 131.907997_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_133	 = 132.907797_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_134	 = 133.909744_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_135	 = 134.910048_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_136	 = 135.914650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_137	 = 136.917871_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_138	 = 137.922350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_139	 = 138.926100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_140	 = 139.93100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_141	 = 140.93503_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_142	 = 141.94018_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_143	 = 142.94456_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_I_144	 = 143.94999_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_110	 = 109.94428_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_111	 = 110.94160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_112	 = 111.93562_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_113	 = 112.933340_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_114	 = 113.927980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_115	 = 114.926294_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_116	 = 115.921581_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_117	 = 116.920359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_118	 = 117.916179_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_119	 = 118.915411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_120	 = 119.911784_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_121	 = 120.911462_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_122	 = 121.908368_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_123	 = 122.908482_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_124	 = 123.9058930_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_125	 = 124.9063955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_126	 = 125.904274_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_127	 = 126.905184_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_128	 = 127.9035313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_129	 = 128.9047794_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_130	 = 129.9035080_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_131	 = 130.9050824_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_132	 = 131.9041535_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_133	 = 132.9059107_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_134	 = 133.9053945_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_135	 = 134.907227_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_136	 = 135.907219_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_137	 = 136.911562_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_138	 = 137.913950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_139	 = 138.918793_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_140	 = 139.921640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_141	 = 140.92665_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_142	 = 141.92971_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_143	 = 142.93511_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_144	 = 143.93851_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_145	 = 144.94407_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_146	 = 145.94775_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Xe_147	 = 146.95356_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_112	 = 111.95030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_113	 = 112.94449_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_114	 = 113.94145_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_115	 = 114.93591_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_116	 = 115.93337_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_117	 = 116.928670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_118	 = 117.926559_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_119	 = 118.922377_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_120	 = 119.920677_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_121	 = 120.917229_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_122	 = 121.916110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_123	 = 122.912996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_124	 = 123.912258_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_125	 = 124.909728_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_126	 = 125.909452_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_127	 = 126.907418_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_128	 = 127.907749_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_129	 = 128.906064_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_130	 = 129.906709_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_131	 = 130.905464_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_132	 = 131.9064343_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_133	 = 132.905451933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_134	 = 133.906718475_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_135	 = 134.9059770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_136	 = 135.9073116_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_137	 = 136.9070895_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_138	 = 137.911017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_139	 = 138.913364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_140	 = 139.917282_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_141	 = 140.920046_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_142	 = 141.924299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_143	 = 142.927352_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_144	 = 143.932077_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_145	 = 144.935526_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_146	 = 145.940290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_147	 = 146.944160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_148	 = 147.94922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_149	 = 148.95293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_150	 = 149.95817_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cs_151	 = 150.96219_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_114	 = 113.95068_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_115	 = 114.94737_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_116	 = 115.94138_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_117	 = 116.93850_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_118	 = 117.93304_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_119	 = 118.93066_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_120	 = 119.92604_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_121	 = 120.92405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_122	 = 121.919900_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_123	 = 122.918781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_124	 = 123.915094_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_125	 = 124.914473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_126	 = 125.911250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_127	 = 126.911094_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_128	 = 127.908318_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_129	 = 128.908679_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_130	 = 129.9063208_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_131	 = 130.906941_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_132	 = 131.9050613_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_133	 = 132.9060075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_134	 = 133.9045084_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_135	 = 134.9056886_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_136	 = 135.9045759_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_137	 = 136.9058274_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_138	 = 137.9052472_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_139	 = 138.9088413_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_140	 = 139.910605_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_141	 = 140.914411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_142	 = 141.916453_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_143	 = 142.920627_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_144	 = 143.922953_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_145	 = 144.927630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_146	 = 145.930220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_147	 = 146.93495_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_148	 = 147.937720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_149	 = 148.94258_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_150	 = 149.94568_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_151	 = 150.95081_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_152	 = 151.95427_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ba_153	 = 152.95961_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_117	 = 116.95007_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_118	 = 117.94673_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_119	 = 118.94099_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_120	 = 119.93807_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_121	 = 120.93301_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_122	 = 121.93071_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_123	 = 122.92624_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_124	 = 123.924570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_125	 = 124.920816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_126	 = 125.91951_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_127	 = 126.916375_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_128	 = 127.915590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_129	 = 128.912693_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_130	 = 129.912369_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_131	 = 130.910070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_132	 = 131.910100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_133	 = 132.908220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_134	 = 133.908514_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_135	 = 134.906977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_136	 = 135.907640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_137	 = 136.906494_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_138	 = 137.907112_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_139	 = 138.9063533_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_140	 = 139.9094776_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_141	 = 140.910962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_142	 = 141.914079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_143	 = 142.916063_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_144	 = 143.919600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_145	 = 144.92165_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_146	 = 145.925790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_147	 = 146.928240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_148	 = 147.932230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_149	 = 148.93473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_150	 = 149.93877_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_151	 = 150.94172_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_152	 = 151.94625_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_153	 = 152.94962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_154	 = 153.95450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_La_155	 = 154.95835_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_119	 = 118.95276_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_120	 = 119.94664_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_121	 = 120.94342_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_122	 = 121.93791_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_123	 = 122.93540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_124	 = 123.93041_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_125	 = 124.92844_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_126	 = 125.923970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_127	 = 126.922730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_128	 = 127.918910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_129	 = 128.918100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_130	 = 129.914740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_131	 = 130.914420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_132	 = 131.911460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_133	 = 132.911515_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_134	 = 133.908925_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_135	 = 134.909151_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_136	 = 135.907172_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_137	 = 136.907806_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_138	 = 137.905991_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_139	 = 138.906653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_140	 = 139.9054387_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_141	 = 140.9082763_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_142	 = 141.909244_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_143	 = 142.912386_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_144	 = 143.913647_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_145	 = 144.917230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_146	 = 145.918760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_147	 = 146.922670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_148	 = 147.924430_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_149	 = 148.92840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_150	 = 149.930410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_151	 = 150.93398_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_152	 = 151.93654_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_153	 = 152.94058_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_154	 = 153.94342_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_155	 = 154.94804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_156	 = 155.95126_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ce_157	 = 156.95634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_121	 = 120.95536_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_122	 = 121.95181_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_123	 = 122.94596_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_124	 = 123.94296_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_125	 = 124.93783_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_126	 = 125.93531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_127	 = 126.93083_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_128	 = 127.928790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_129	 = 128.925100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_130	 = 129.923590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_131	 = 130.920260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_132	 = 131.919260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_133	 = 132.916331_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_134	 = 133.915710_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_135	 = 134.913112_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_136	 = 135.912692_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_137	 = 136.910705_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_138	 = 137.910755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_139	 = 138.908938_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_140	 = 139.909076_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_141	 = 140.9076528_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_142	 = 141.9100448_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_143	 = 142.9108169_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_144	 = 143.913305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_145	 = 144.914512_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_146	 = 145.917640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_147	 = 146.918996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_148	 = 147.922135_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_149	 = 148.923720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_150	 = 149.926673_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_151	 = 150.928319_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_152	 = 151.93150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_153	 = 152.93384_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_154	 = 153.93752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_155	 = 154.94012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_156	 = 155.94427_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_157	 = 156.94743_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_158	 = 157.95198_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pr_159	 = 158.95550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_124	 = 123.95223_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_125	 = 124.94888_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_126	 = 125.94322_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_127	 = 126.94050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_128	 = 127.93539_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_129	 = 128.93319_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_130	 = 129.928510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_131	 = 130.927250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_132	 = 131.923321_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_133	 = 132.922350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_134	 = 133.918790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_135	 = 134.918181_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_136	 = 135.914976_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_137	 = 136.914567_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_138	 = 137.911950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_139	 = 138.911978_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_140	 = 139.909550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_141	 = 140.909610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_142	 = 141.9077233_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_143	 = 142.9098143_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_144	 = 143.9100873_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_145	 = 144.9125736_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_146	 = 145.9131169_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_147	 = 146.9161004_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_148	 = 147.916893_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_149	 = 148.920149_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_150	 = 149.920891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_151	 = 150.923829_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_152	 = 151.924682_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_153	 = 152.927698_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_154	 = 153.92948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_155	 = 154.93293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_156	 = 155.93502_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_157	 = 156.93903_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_158	 = 157.94160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_159	 = 158.94609_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_160	 = 159.94909_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Nd_161	 = 160.95388_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_126	 = 125.95752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_127	 = 126.95163_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_128	 = 127.94842_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_129	 = 128.94316_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_130	 = 129.94045_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_131	 = 130.93587_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_132	 = 131.93375_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_133	 = 132.929780_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_134	 = 133.928350_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_135	 = 134.924880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_136	 = 135.923570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_137	 = 136.920479_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_138	 = 137.919548_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_139	 = 138.916804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_140	 = 139.916040_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_141	 = 140.913555_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_142	 = 141.912874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_143	 = 142.910933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_144	 = 143.912591_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_145	 = 144.912749_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_146	 = 145.914696_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_147	 = 146.9151385_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_148	 = 147.917475_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_149	 = 148.918334_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_150	 = 149.920984_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_151	 = 150.921207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_152	 = 151.923497_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_153	 = 152.924117_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_154	 = 153.926460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_155	 = 154.928100_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_156	 = 155.931060_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_157	 = 156.93304_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_158	 = 157.93656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_159	 = 158.93897_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_160	 = 159.94299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_161	 = 160.94586_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_162	 = 161.95029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pm_163	 = 162.95368_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_128	 = 127.95808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_129	 = 128.95464_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_130	 = 129.94892_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_131	 = 130.94611_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_132	 = 131.94069_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_133	 = 132.93867_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_134	 = 133.93397_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_135	 = 134.93252_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_136	 = 135.928276_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_137	 = 136.926970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_138	 = 137.923244_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_139	 = 138.922297_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_140	 = 139.918995_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_141	 = 140.918476_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_142	 = 141.915198_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_143	 = 142.914628_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_144	 = 143.911999_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_145	 = 144.913410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_146	 = 145.913041_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_147	 = 146.9148979_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_148	 = 147.9148227_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_149	 = 148.9171847_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_150	 = 149.9172755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_151	 = 150.9199324_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_152	 = 151.9197324_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_153	 = 152.9220974_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_154	 = 153.9222093_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_155	 = 154.9246402_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_156	 = 155.925528_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_157	 = 156.928360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_158	 = 157.929990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_159	 = 158.93321_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_160	 = 159.93514_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_161	 = 160.93883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_162	 = 161.94122_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_163	 = 162.94536_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_164	 = 163.94828_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sm_165	 = 164.95298_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_130	 = 129.96357_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_131	 = 130.95775_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_132	 = 131.95437_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_133	 = 132.94924_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_134	 = 133.94651_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_135	 = 134.94182_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_136	 = 135.93960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_137	 = 136.93557_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_138	 = 137.933710_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_139	 = 138.929792_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_140	 = 139.928090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_141	 = 140.924931_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_142	 = 141.923430_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_143	 = 142.920298_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_144	 = 143.918817_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_145	 = 144.916265_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_146	 = 145.917206_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_147	 = 146.916746_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_148	 = 147.918086_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_149	 = 148.917931_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_150	 = 149.919702_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_151	 = 150.9198502_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_152	 = 151.9217445_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_153	 = 152.9212303_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_154	 = 153.9229792_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_155	 = 154.9228933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_156	 = 155.924752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_157	 = 156.925424_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_158	 = 157.927850_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_159	 = 158.929089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_160	 = 159.93197_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_161	 = 160.93368_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_162	 = 161.93704_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_163	 = 162.93921_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_164	 = 163.94299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_165	 = 164.94572_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_166	 = 165.94997_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Eu_167	 = 166.95321_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_134	 = 133.95537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_135	 = 134.95257_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_136	 = 135.94734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_137	 = 136.94502_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_138	 = 137.94012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_139	 = 138.93824_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_140	 = 139.933670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_141	 = 140.932126_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_142	 = 141.928120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_143	 = 142.92675_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_144	 = 143.922960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_145	 = 144.921709_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_146	 = 145.918311_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_147	 = 146.919094_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_148	 = 147.918115_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_149	 = 148.919341_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_150	 = 149.918659_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_151	 = 150.920348_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_152	 = 151.9197910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_153	 = 152.9217495_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_154	 = 153.9208656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_155	 = 154.9226220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_156	 = 155.9221227_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_157	 = 156.9239601_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_158	 = 157.9241039_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_159	 = 158.9263887_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_160	 = 159.9270541_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_161	 = 160.9296692_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_162	 = 161.930985_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_163	 = 162.93399_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_164	 = 163.93586_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_165	 = 164.93938_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_166	 = 165.94160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_167	 = 166.94557_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_168	 = 167.94836_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Gd_169	 = 168.95287_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_136	 = 135.96138_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_137	 = 136.95598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_138	 = 137.95316_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_139	 = 138.94829_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_140	 = 139.94581_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_141	 = 140.94145_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_142	 = 141.93874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_143	 = 142.935120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_144	 = 143.933050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_145	 = 144.929270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_146	 = 145.927250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_147	 = 146.924045_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_148	 = 147.924272_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_149	 = 148.923246_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_150	 = 149.923660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_151	 = 150.923103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_152	 = 151.924070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_153	 = 152.923435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_154	 = 153.924680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_155	 = 154.923505_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_156	 = 155.924747_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_157	 = 156.9240246_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_158	 = 157.9254131_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_159	 = 158.9253468_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_160	 = 159.9271676_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_161	 = 160.9275699_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_162	 = 161.929490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_163	 = 162.930648_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_164	 = 163.93335_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_165	 = 164.93488_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_166	 = 165.93799_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_167	 = 166.94005_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_168	 = 167.94364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_169	 = 168.94622_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_170	 = 169.95025_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tb_171	 = 170.95330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_138	 = 137.96249_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_139	 = 138.95954_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_140	 = 139.95401_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_141	 = 140.95135_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_142	 = 141.94637_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_143	 = 142.94383_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_144	 = 143.939250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_145	 = 144.937430_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_146	 = 145.932845_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_147	 = 146.931092_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_148	 = 147.927150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_149	 = 148.927305_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_150	 = 149.925585_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_151	 = 150.926185_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_152	 = 151.924718_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_153	 = 152.925765_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_154	 = 153.924424_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_155	 = 154.925754_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_156	 = 155.924283_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_157	 = 156.925466_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_158	 = 157.924409_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_159	 = 158.9257392_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_160	 = 159.9251975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_161	 = 160.9269334_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_162	 = 161.9267984_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_163	 = 162.9287312_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_164	 = 163.9291748_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_165	 = 164.9317033_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_166	 = 165.9328067_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_167	 = 166.935660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_168	 = 167.93713_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_169	 = 168.94031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_170	 = 169.94239_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_171	 = 170.94620_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_172	 = 171.94876_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Dy_173	 = 172.95300_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_140	 = 139.96854_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_141	 = 140.96310_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_142	 = 141.95977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_143	 = 142.95461_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_144	 = 143.95148_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_145	 = 144.94720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_146	 = 145.94464_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_147	 = 146.940060_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_148	 = 147.93772_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_149	 = 148.933775_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_150	 = 149.933496_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_151	 = 150.931688_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_152	 = 151.931714_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_153	 = 152.930199_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_154	 = 153.930602_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_155	 = 154.929103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_156	 = 155.929840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_157	 = 156.928256_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_158	 = 157.928941_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_159	 = 158.927712_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_160	 = 159.928729_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_161	 = 160.927855_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_162	 = 161.929096_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_163	 = 162.9287339_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_164	 = 163.9302335_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_165	 = 164.9303221_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_166	 = 165.9322842_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_167	 = 166.933133_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_168	 = 167.935520_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_169	 = 168.936872_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_170	 = 169.939620_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_171	 = 170.94147_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_172	 = 171.94482_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_173	 = 172.94729_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_174	 = 173.95115_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ho_175	 = 174.95405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_143	 = 142.96634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_144	 = 143.96038_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_145	 = 144.95739_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_146	 = 145.95200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_147	 = 146.94949_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_148	 = 147.94455_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_149	 = 148.942310_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_150	 = 149.937914_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_151	 = 150.937449_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_152	 = 151.935050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_153	 = 152.935063_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_154	 = 153.932783_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_155	 = 154.933209_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_156	 = 155.931065_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_157	 = 156.931920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_158	 = 157.929893_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_159	 = 158.930684_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_160	 = 159.929083_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_161	 = 160.929995_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_162	 = 161.928778_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_163	 = 162.930033_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_164	 = 163.929200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_165	 = 164.930726_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_166	 = 165.9302931_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_167	 = 166.9320482_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_168	 = 167.9323702_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_169	 = 168.9345904_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_170	 = 169.9354643_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_171	 = 170.9380298_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_172	 = 171.939356_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_173	 = 172.94240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_174	 = 173.94423_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_175	 = 174.94777_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_176	 = 175.95008_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Er_177	 = 176.95405_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_145	 = 144.97007_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_146	 = 145.96643_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_147	 = 146.96096_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_148	 = 147.95784_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_149	 = 148.95272_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_150	 = 149.94996_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_151	 = 150.945483_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_152	 = 151.944420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_153	 = 152.942012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_154	 = 153.941568_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_155	 = 154.939199_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_156	 = 155.938980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_157	 = 156.936970_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_158	 = 157.936980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_159	 = 158.934980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_160	 = 159.935260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_161	 = 160.933550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_162	 = 161.933995_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_163	 = 162.932651_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_164	 = 163.933560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_165	 = 164.932435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_166	 = 165.933554_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_167	 = 166.9328516_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_168	 = 167.934173_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_169	 = 168.9342133_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_170	 = 169.9358014_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_171	 = 170.9364294_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_172	 = 171.938400_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_173	 = 172.939604_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_174	 = 173.942170_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_175	 = 174.943840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_176	 = 175.94699_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_177	 = 176.94904_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_178	 = 177.95264_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tm_179	 = 178.95534_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_148	 = 147.96742_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_149	 = 148.96404_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_150	 = 149.95842_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_151	 = 150.95540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_152	 = 151.95029_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_153	 = 152.94948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_154	 = 153.946394_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_155	 = 154.945782_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_156	 = 155.942818_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_157	 = 156.942628_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_158	 = 157.939866_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_159	 = 158.940050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_160	 = 159.937552_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_161	 = 160.937902_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_162	 = 161.935768_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_163	 = 162.936334_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_164	 = 163.934489_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_165	 = 164.935280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_166	 = 165.933882_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_167	 = 166.934950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_168	 = 167.933897_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_169	 = 168.935190_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_170	 = 169.9347618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_171	 = 170.9363258_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_172	 = 171.9363815_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_173	 = 172.9382108_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_174	 = 173.9388621_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_175	 = 174.9412765_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_176	 = 175.9425717_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_177	 = 176.9452608_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_178	 = 177.946647_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_179	 = 178.95017_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_180	 = 179.95233_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Yb_181	 = 180.95615_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_150	 = 149.97323_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_151	 = 150.96758_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_152	 = 151.96412_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_153	 = 152.95877_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_154	 = 153.95752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_155	 = 154.954316_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_156	 = 155.953030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_157	 = 156.950098_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_158	 = 157.949313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_159	 = 158.946630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_160	 = 159.946030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_161	 = 160.943570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_162	 = 161.943280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_163	 = 162.941180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_164	 = 163.941340_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_165	 = 164.939407_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_166	 = 165.939860_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_167	 = 166.938270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_168	 = 167.938740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_169	 = 168.937651_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_170	 = 169.938475_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_171	 = 170.9379131_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_172	 = 171.939086_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_173	 = 172.9389306_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_174	 = 173.9403375_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_175	 = 174.9407718_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_176	 = 175.9426863_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_177	 = 176.9437581_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_178	 = 177.945955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_179	 = 178.947327_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_180	 = 179.949880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_181	 = 180.95197_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_182	 = 181.95504_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_183	 = 182.95757_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lu_184	 = 183.96091_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_153	 = 152.97069_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_154	 = 153.96486_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_155	 = 154.96339_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_156	 = 155.95936_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_157	 = 156.95840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_158	 = 157.954799_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_159	 = 158.953995_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_160	 = 159.950684_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_161	 = 160.950275_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_162	 = 161.947210_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_163	 = 162.947090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_164	 = 163.944367_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_165	 = 164.944570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_166	 = 165.942180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_167	 = 166.942600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_168	 = 167.940570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_169	 = 168.941260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_170	 = 169.939610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_171	 = 170.940490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_172	 = 171.939448_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_173	 = 172.940510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_174	 = 173.940046_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_175	 = 174.941509_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_176	 = 175.9414086_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_177	 = 176.9432207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_178	 = 177.9436988_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_179	 = 178.9458161_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_180	 = 179.9465500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_181	 = 180.9491012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_182	 = 181.950554_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_183	 = 182.953530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_184	 = 183.955450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_185	 = 184.95882_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_186	 = 185.96089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_187	 = 186.96459_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hf_188	 = 187.96685_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_155	 = 154.97459_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_156	 = 155.97230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_157	 = 156.96819_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_158	 = 157.96670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_159	 = 158.963018_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_160	 = 159.96149_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_161	 = 160.958420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_162	 = 161.957290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_163	 = 162.954330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_164	 = 163.953530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_165	 = 164.950773_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_166	 = 165.950510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_167	 = 166.948090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_168	 = 167.948050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_169	 = 168.946010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_170	 = 169.946180_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_171	 = 170.944480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_172	 = 171.944900_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_173	 = 172.943750_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_174	 = 173.944450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_175	 = 174.943740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_176	 = 175.944860_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_177	 = 176.944472_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_178	 = 177.945778_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_179	 = 178.9459295_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_180	 = 179.9474648_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_181	 = 180.9479958_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_182	 = 181.9501518_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_183	 = 182.9513726_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_184	 = 183.954008_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_185	 = 184.955559_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_186	 = 185.958550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_187	 = 186.96053_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_188	 = 187.96370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_189	 = 188.96583_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ta_190	 = 189.96923_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_158	 = 157.97456_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_159	 = 158.97292_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_160	 = 159.96848_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_161	 = 160.96736_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_162	 = 161.963497_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_163	 = 162.962520_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_164	 = 163.958954_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_165	 = 164.958280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_166	 = 165.955027_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_167	 = 166.954816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_168	 = 167.951808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_169	 = 168.951779_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_170	 = 169.949228_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_171	 = 170.949450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_172	 = 171.947290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_173	 = 172.947690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_174	 = 173.946080_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_175	 = 174.946720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_176	 = 175.945630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_177	 = 176.946640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_178	 = 177.945876_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_179	 = 178.947070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_180	 = 179.946704_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_181	 = 180.948197_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_182	 = 181.9482042_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_183	 = 182.9502230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_184	 = 183.9509312_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_185	 = 184.9534193_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_186	 = 185.9543641_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_187	 = 186.9571605_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_188	 = 187.958489_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_189	 = 188.96191_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_190	 = 189.96318_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_191	 = 190.96660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_W_192	 = 191.96817_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_160	 = 159.98212_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_161	 = 160.97759_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_162	 = 161.97600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_163	 = 162.972081_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_164	 = 163.97032_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_165	 = 164.967089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_166	 = 165.965810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_167	 = 166.962600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_168	 = 167.961570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_169	 = 168.958790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_170	 = 169.958220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_171	 = 170.955720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_172	 = 171.955420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_173	 = 172.953240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_174	 = 173.953120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_175	 = 174.951380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_176	 = 175.951620_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_177	 = 176.950330_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_178	 = 177.950990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_179	 = 178.949988_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_180	 = 179.950789_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_181	 = 180.950068_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_182	 = 181.95121_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_183	 = 182.950820_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_184	 = 183.952521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_185	 = 184.9529550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_186	 = 185.9549861_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_187	 = 186.9557531_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_188	 = 187.9581144_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_189	 = 188.959229_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_190	 = 189.96182_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_191	 = 190.963125_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_192	 = 191.96596_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_193	 = 192.96747_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Re_194	 = 193.97042_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_162	 = 161.98443_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_163	 = 162.98269_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_164	 = 163.97804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_165	 = 164.97676_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_166	 = 165.972691_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_167	 = 166.971550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_168	 = 167.967804_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_169	 = 168.967019_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_170	 = 169.963577_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_171	 = 170.963185_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_172	 = 171.960023_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_173	 = 172.959808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_174	 = 173.957062_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_175	 = 174.956946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_176	 = 175.954810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_177	 = 176.954965_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_178	 = 177.953251_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_179	 = 178.953816_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_180	 = 179.952379_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_181	 = 180.953240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_182	 = 181.952110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_183	 = 182.953130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_184	 = 183.9524891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_185	 = 184.9540423_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_186	 = 185.9538382_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_187	 = 186.9557505_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_188	 = 187.9558382_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_189	 = 188.9581475_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_190	 = 189.9584470_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_191	 = 190.9609297_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_192	 = 191.9614807_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_193	 = 192.9641516_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_194	 = 193.9651821_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_195	 = 194.96813_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Os_196	 = 195.969640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_164	 = 163.99220_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_165	 = 164.98752_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_166	 = 165.98582_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_167	 = 166.981665_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_168	 = 167.97988_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_169	 = 168.976295_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_170	 = 169.97497_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_171	 = 170.971630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_172	 = 171.97046_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_173	 = 172.967502_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_174	 = 173.966861_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_175	 = 174.964113_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_176	 = 175.963649_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_177	 = 176.961302_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_178	 = 177.961082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_179	 = 178.959122_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_180	 = 179.959229_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_181	 = 180.957625_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_182	 = 181.958076_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_183	 = 182.956846_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_184	 = 183.957480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_185	 = 184.956700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_186	 = 185.957946_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_187	 = 186.957363_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_188	 = 187.958853_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_189	 = 188.958719_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_190	 = 189.9605460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_191	 = 190.9605940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_192	 = 191.9626050_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_193	 = 192.9629264_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_194	 = 193.9650784_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_195	 = 194.9659796_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_196	 = 195.968400_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_197	 = 196.969653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_198	 = 197.97228_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ir_199	 = 198.973800_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_166	 = 165.99486_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_167	 = 166.99298_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_168	 = 167.98815_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_169	 = 168.98672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_170	 = 169.982495_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_171	 = 170.981240_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_172	 = 171.977347_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_173	 = 172.976440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_174	 = 173.972819_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_175	 = 174.972421_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_176	 = 175.968945_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_177	 = 176.968469_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_178	 = 177.965649_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_179	 = 178.965363_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_180	 = 179.963031_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_181	 = 180.963097_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_182	 = 181.961171_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_183	 = 182.961597_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_184	 = 183.959922_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_185	 = 184.960620_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_186	 = 185.959351_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_187	 = 186.960590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_188	 = 187.959395_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_189	 = 188.960834_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_190	 = 189.959932_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_191	 = 190.961677_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_192	 = 191.9610380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_193	 = 192.9629874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_194	 = 193.9626803_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_195	 = 194.9647911_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_196	 = 195.9649515_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_197	 = 196.9673402_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_198	 = 197.967893_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_199	 = 198.970593_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_200	 = 199.971441_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_201	 = 200.974510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pt_202	 = 201.97574_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_169	 = 168.99808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_170	 = 169.99612_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_171	 = 170.991879_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_172	 = 171.99004_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_173	 = 172.986237_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_174	 = 173.98476_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_175	 = 174.981270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_176	 = 175.98010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_177	 = 176.976865_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_178	 = 177.976030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_179	 = 178.973213_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_180	 = 179.972521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_181	 = 180.970079_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_182	 = 181.969618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_183	 = 182.967593_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_184	 = 183.967452_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_185	 = 184.965789_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_186	 = 185.965953_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_187	 = 186.964568_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_188	 = 187.965324_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_189	 = 188.963948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_190	 = 189.964700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_191	 = 190.963700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_192	 = 191.964813_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_193	 = 192.964150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_194	 = 193.965365_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_195	 = 194.9650346_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_196	 = 195.966570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_197	 = 196.9665687_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_198	 = 197.9682423_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_199	 = 198.9687652_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_200	 = 199.970730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_201	 = 200.971657_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_202	 = 201.97381_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_203	 = 202.975155_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_204	 = 203.97772_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Au_205	 = 204.97987_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_171	 = 171.00376_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_172	 = 171.99883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_173	 = 172.99724_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_174	 = 173.992864_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_175	 = 174.99142_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_176	 = 175.987355_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_177	 = 176.986280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_178	 = 177.982483_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_179	 = 178.981834_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_180	 = 179.978266_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_181	 = 180.977819_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_182	 = 181.974690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_183	 = 182.974450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_184	 = 183.971713_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_185	 = 184.971899_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_186	 = 185.969362_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_187	 = 186.969814_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_188	 = 187.967577_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_189	 = 188.968190_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_190	 = 189.966322_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_191	 = 190.967157_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_192	 = 191.965634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_193	 = 192.966665_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_194	 = 193.965439_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_195	 = 194.966720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_196	 = 195.965833_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_197	 = 196.967213_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_198	 = 197.9667690_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_199	 = 198.9682799_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_200	 = 199.9683260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_201	 = 200.9703023_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_202	 = 201.9706430_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_203	 = 202.9728725_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_204	 = 203.9734939_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_205	 = 204.976073_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_206	 = 205.977514_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_207	 = 206.98259_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_208	 = 207.98594_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_209	 = 208.99104_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hg_210	 = 209.99451_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_176	 = 176.00059_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_177	 = 176.996427_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_178	 = 177.99490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_179	 = 178.991090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_180	 = 179.98991_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_181	 = 180.986257_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_182	 = 181.985670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_183	 = 182.982193_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_184	 = 183.981870_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_185	 = 184.978790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_186	 = 185.97833_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_187	 = 186.975906_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_188	 = 187.976010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_189	 = 188.973588_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_190	 = 189.973880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_191	 = 190.971786_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_192	 = 191.972230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_193	 = 192.97067_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_194	 = 193.97120_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_195	 = 194.969774_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_196	 = 195.970481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_197	 = 196.969575_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_198	 = 197.970480_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_199	 = 198.969880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_200	 = 199.970963_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_201	 = 200.970819_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_202	 = 201.972106_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_203	 = 202.9723442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_204	 = 203.9738635_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_205	 = 204.9744275_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_206	 = 205.9761103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_207	 = 206.977419_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_208	 = 207.9820187_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_209	 = 208.985359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_210	 = 209.990074_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_211	 = 210.99348_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Tl_212	 = 211.99823_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_178	 = 178.003830_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_179	 = 179.00215_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_180	 = 179.997918_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_181	 = 180.99662_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_182	 = 181.992672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_183	 = 182.991870_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_184	 = 183.988142_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_185	 = 184.987610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_186	 = 185.984239_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_187	 = 186.983918_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_188	 = 187.980874_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_189	 = 188.980810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_190	 = 189.978082_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_191	 = 190.978270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_192	 = 191.975785_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_193	 = 192.976170_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_194	 = 193.974012_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_195	 = 194.974542_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_196	 = 195.972774_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_197	 = 196.973431_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_198	 = 197.972034_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_199	 = 198.972917_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_200	 = 199.971827_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_201	 = 200.972885_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_202	 = 201.972159_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_203	 = 202.973391_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_204	 = 203.9730436_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_205	 = 204.9744818_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_206	 = 205.9744653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_207	 = 206.9758969_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_208	 = 207.9766521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_209	 = 208.9810901_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_210	 = 209.9841885_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_211	 = 210.9887370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_212	 = 211.9918975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_213	 = 212.996581_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_214	 = 213.9998054_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pb_215	 = 215.00481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_184	 = 184.00112_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_185	 = 184.997630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_186	 = 185.996600_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_187	 = 186.993158_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_188	 = 187.992270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_189	 = 188.989200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_190	 = 189.98830_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_191	 = 190.985786_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_192	 = 191.985460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_193	 = 192.982960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_194	 = 193.982830_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_195	 = 194.980651_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_196	 = 195.980667_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_197	 = 196.978864_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_198	 = 197.979210_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_199	 = 198.977672_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_200	 = 199.978132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_201	 = 200.977009_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_202	 = 201.977742_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_203	 = 202.976876_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_204	 = 203.977813_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_205	 = 204.977389_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_206	 = 205.978499_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_207	 = 206.9784707_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_208	 = 207.9797422_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_209	 = 208.9803987_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_210	 = 209.9841204_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_211	 = 210.987269_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_212	 = 211.9912857_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_213	 = 212.994385_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_214	 = 213.998712_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_215	 = 215.001770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_216	 = 216.006306_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_217	 = 217.00947_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bi_218	 = 218.01432_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_188	 = 187.999422_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_189	 = 188.998481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_190	 = 189.995101_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_191	 = 190.994574_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_192	 = 191.991335_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_193	 = 192.991030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_194	 = 193.988186_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_195	 = 194.988110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_196	 = 195.985535_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_197	 = 196.985660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_198	 = 197.983389_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_199	 = 198.983666_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_200	 = 199.981799_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_201	 = 200.982260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_202	 = 201.980758_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_203	 = 202.981420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_204	 = 203.980318_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_205	 = 204.981203_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_206	 = 205.980481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_207	 = 206.981593_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_208	 = 207.9812457_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_209	 = 208.9824304_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_210	 = 209.9828737_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_211	 = 210.9866532_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_212	 = 211.9888680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_213	 = 212.992857_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_214	 = 213.9952014_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_215	 = 214.9994200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_216	 = 216.0019150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_217	 = 217.006335_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_218	 = 218.0089730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_219	 = 219.01374_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Po_220	 = 220.01660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_193	 = 192.999840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_194	 = 193.99873_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_195	 = 194.996268_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_196	 = 195.995790_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_197	 = 196.993190_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_198	 = 197.992840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_199	 = 198.990530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_200	 = 199.990351_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_201	 = 200.988417_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_202	 = 201.988630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_203	 = 202.986942_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_204	 = 203.987251_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_205	 = 204.986074_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_206	 = 205.986667_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_207	 = 206.985784_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_208	 = 207.986590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_209	 = 208.986173_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_210	 = 209.987148_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_211	 = 210.9874963_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_212	 = 211.990745_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_213	 = 212.992937_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_214	 = 213.996372_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_215	 = 214.998653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_216	 = 216.002423_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_217	 = 217.004719_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_218	 = 218.008694_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_219	 = 219.011162_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_220	 = 220.015410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_221	 = 221.01805_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_222	 = 222.02233_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_At_223	 = 223.02519_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_195	 = 195.005440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_196	 = 196.002115_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_197	 = 197.001580_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_198	 = 197.998679_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_199	 = 198.998370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_200	 = 199.995699_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_201	 = 200.995630_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_202	 = 201.993263_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_203	 = 202.993387_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_204	 = 203.991429_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_205	 = 204.991720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_206	 = 205.990214_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_207	 = 206.990734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_208	 = 207.989642_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_209	 = 208.990415_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_210	 = 209.989696_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_211	 = 210.990601_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_212	 = 211.990704_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_213	 = 212.993883_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_214	 = 213.995363_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_215	 = 214.998745_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_216	 = 216.000274_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_217	 = 217.003928_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_218	 = 218.0056013_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_219	 = 219.0094802_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_220	 = 220.0113940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_221	 = 221.015537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_222	 = 222.0175777_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_223	 = 223.02179_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_224	 = 224.02409_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_225	 = 225.02844_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_226	 = 226.03089_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_227	 = 227.03541_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rn_228	 = 228.03799_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_199	 = 199.007260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_200	 = 200.006570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_201	 = 201.003860_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_202	 = 202.003370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_203	 = 203.000925_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_204	 = 204.000653_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_205	 = 204.998594_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_206	 = 205.998670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_207	 = 206.996950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_208	 = 207.997140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_209	 = 208.995954_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_210	 = 209.996408_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_211	 = 210.995537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_212	 = 211.996202_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_213	 = 212.996189_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_214	 = 213.998971_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_215	 = 215.000341_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_216	 = 216.003198_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_217	 = 217.004632_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_218	 = 218.007578_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_219	 = 219.009252_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_220	 = 220.012327_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_221	 = 221.014255_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_222	 = 222.017552_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_223	 = 223.0197359_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_224	 = 224.023250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_225	 = 225.025570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_226	 = 226.02939_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_227	 = 227.03184_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_228	 = 228.03573_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_229	 = 229.038450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_230	 = 230.04251_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_231	 = 231.04544_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fr_232	 = 232.04977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_202	 = 202.009890_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_203	 = 203.009270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_204	 = 204.006500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_205	 = 205.006270_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_206	 = 206.003827_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_207	 = 207.003800_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_208	 = 208.001840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_209	 = 209.001990_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_210	 = 210.000495_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_211	 = 211.000898_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_212	 = 211.999794_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_213	 = 213.000384_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_214	 = 214.000108_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_215	 = 215.002720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_216	 = 216.003533_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_217	 = 217.006320_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_218	 = 218.007140_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_219	 = 219.010085_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_220	 = 220.011028_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_221	 = 221.013917_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_222	 = 222.015375_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_223	 = 223.0185022_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_224	 = 224.0202118_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_225	 = 225.023612_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_226	 = 226.0254098_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_227	 = 227.0291778_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_228	 = 228.0310703_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_229	 = 229.034958_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_230	 = 230.037056_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_231	 = 231.04122_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_232	 = 232.04364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_233	 = 233.04806_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ra_234	 = 234.05070_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_206	 = 206.014500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_207	 = 207.011950_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_208	 = 208.011550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_209	 = 209.009490_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_210	 = 210.009440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_211	 = 211.007730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_212	 = 212.007810_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_213	 = 213.006610_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_214	 = 214.006902_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_215	 = 215.006454_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_216	 = 216.008720_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_217	 = 217.009347_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_218	 = 218.011640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_219	 = 219.012420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_220	 = 220.014763_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_221	 = 221.015590_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_222	 = 222.017844_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_223	 = 223.019137_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_224	 = 224.021723_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_225	 = 225.023230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_226	 = 226.026098_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_227	 = 227.0277521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_228	 = 228.0310211_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_229	 = 229.033020_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_230	 = 230.03629_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_231	 = 231.03856_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_232	 = 232.04203_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_233	 = 233.04455_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_234	 = 234.04842_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_235	 = 235.05123_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ac_236	 = 236.05530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_209	 = 209.01772_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_210	 = 210.015075_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_211	 = 211.014930_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_212	 = 212.012980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_213	 = 213.013010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_214	 = 214.011500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_215	 = 215.011730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_216	 = 216.011062_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_217	 = 217.013114_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_218	 = 218.013284_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_219	 = 219.015540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_220	 = 220.015748_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_221	 = 221.018184_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_222	 = 222.018468_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_223	 = 223.020811_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_224	 = 224.021467_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_225	 = 225.023951_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_226	 = 226.024903_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_227	 = 227.0277041_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_228	 = 228.0287411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_229	 = 229.031762_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_230	 = 230.0331338_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_231	 = 231.0363043_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_232	 = 232.0380553_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_233	 = 233.0415818_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_234	 = 234.043601_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_235	 = 235.047510_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_236	 = 236.04987_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_237	 = 237.05389_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Th_238	 = 238.05650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_212	 = 212.023200_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_213	 = 213.021110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_214	 = 214.020920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_215	 = 215.019190_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_216	 = 216.019110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_217	 = 217.018320_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_218	 = 218.020042_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_219	 = 219.019880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_220	 = 220.021880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_221	 = 221.021880_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_222	 = 222.023740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_223	 = 223.023960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_224	 = 224.025626_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_225	 = 225.026130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_226	 = 226.027948_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_227	 = 227.028805_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_228	 = 228.031051_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_229	 = 229.0320968_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_230	 = 230.034541_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_231	 = 231.0358840_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_232	 = 232.038592_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_233	 = 233.0402473_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_234	 = 234.043308_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_235	 = 235.045440_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_236	 = 236.04868_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_237	 = 237.05115_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_238	 = 238.054500_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_239	 = 239.05726_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pa_240	 = 240.06098_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_217	 = 217.024370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_218	 = 218.023540_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_219	 = 219.024920_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_220	 = 220.02472_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_221	 = 221.02640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_222	 = 222.02609_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_223	 = 223.027740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_224	 = 224.027605_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_225	 = 225.029391_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_226	 = 226.029339_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_227	 = 227.031156_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_228	 = 228.031374_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_229	 = 229.033506_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_230	 = 230.033940_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_231	 = 231.036294_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_232	 = 232.0371562_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_233	 = 233.0396352_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_234	 = 234.0409521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_235	 = 235.0439299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_236	 = 236.0455680_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_237	 = 237.0487302_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_238	 = 238.0507882_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_239	 = 239.0542933_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_240	 = 240.056592_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_241	 = 241.06033_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_U_242	 = 242.06293_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_225	 = 225.033910_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_226	 = 226.03515_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_227	 = 227.034960_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_228	 = 228.03618_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_229	 = 229.036260_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_230	 = 230.037830_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_231	 = 231.038250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_232	 = 232.04011_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_233	 = 233.040740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_234	 = 234.042895_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_235	 = 235.0440633_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_236	 = 236.046570_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_237	 = 237.0481734_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_238	 = 238.0509464_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_239	 = 239.0529390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_240	 = 240.056162_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_241	 = 241.058250_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_242	 = 242.06164_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_243	 = 243.064280_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Np_244	 = 244.06785_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_228	 = 228.038740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_229	 = 229.040150_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_230	 = 230.039650_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_231	 = 231.041101_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_232	 = 232.041187_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_233	 = 233.043000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_234	 = 234.043317_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_235	 = 235.045286_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_236	 = 236.0460580_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_237	 = 237.0484097_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_238	 = 238.0495599_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_239	 = 239.0521634_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_240	 = 240.0538135_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_241	 = 241.0568515_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_242	 = 242.0587426_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_243	 = 243.062003_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_244	 = 244.064204_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_245	 = 245.067747_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_246	 = 246.070205_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Pu_247	 = 247.07407_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_231	 = 231.04556_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_232	 = 232.04659_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_233	 = 233.04635_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_234	 = 234.04781_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_235	 = 235.04795_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_236	 = 236.04958_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_237	 = 237.050000_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_238	 = 238.051980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_239	 = 239.0530245_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_240	 = 240.055300_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_241	 = 241.0568291_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_242	 = 242.0595492_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_243	 = 243.0613811_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_244	 = 244.0642848_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_245	 = 245.066452_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_246	 = 246.069775_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_247	 = 247.07209_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_248	 = 248.07575_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Am_249	 = 249.07848_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_233	 = 233.050770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_234	 = 234.050160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_235	 = 235.05143_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_236	 = 236.05141_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_237	 = 237.05290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_238	 = 238.053030_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_239	 = 239.05496_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_240	 = 240.0555295_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_241	 = 241.0576530_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_242	 = 242.0588358_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_243	 = 243.0613891_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_244	 = 244.0627526_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_245	 = 245.0654912_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_246	 = 246.0672237_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_247	 = 247.070354_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_248	 = 248.072349_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_249	 = 249.075953_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_250	 = 250.078357_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_251	 = 251.082285_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cm_252	 = 252.08487_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_235	 = 235.05658_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_236	 = 236.05733_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_237	 = 237.05700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_238	 = 238.05828_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_239	 = 239.05828_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_240	 = 240.05976_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_241	 = 241.06023_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_242	 = 242.06198_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_243	 = 243.063008_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_244	 = 244.065181_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_245	 = 245.0663616_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_246	 = 246.068670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_247	 = 247.070307_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_248	 = 248.073090_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_249	 = 249.0749867_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_250	 = 250.078317_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_251	 = 251.080760_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_252	 = 252.08431_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_253	 = 253.08688_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bk_254	 = 254.09060_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_237	 = 237.06207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_238	 = 238.06141_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_239	 = 239.06242_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_240	 = 240.06230_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_241	 = 241.06373_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_242	 = 242.063700_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_243	 = 243.06543_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_244	 = 244.066001_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_245	 = 245.068049_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_246	 = 246.0688053_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_247	 = 247.071001_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_248	 = 248.072185_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_249	 = 249.0748535_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_250	 = 250.0764061_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_251	 = 251.079587_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_252	 = 252.081626_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_253	 = 253.085133_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_254	 = 254.087323_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_255	 = 255.09105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cf_256	 = 256.09344_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_240	 = 240.06892_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_241	 = 241.06854_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_242	 = 242.06975_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_243	 = 243.06955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_244	 = 244.07088_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_245	 = 245.07132_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_246	 = 246.07290_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_247	 = 247.073660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_248	 = 248.075470_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_249	 = 249.076410_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_250	 = 250.07861_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_251	 = 251.079992_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_252	 = 252.082980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_253	 = 253.0848247_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_254	 = 254.088022_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_255	 = 255.090273_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_256	 = 256.09360_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_257	 = 257.09598_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Es_258	 = 258.09952_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_242	 = 242.07343_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_243	 = 243.07435_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_244	 = 244.07408_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_245	 = 245.07539_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_246	 = 246.075300_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_247	 = 247.07685_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_248	 = 248.077195_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_249	 = 249.07903_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_250	 = 250.079521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_251	 = 251.081575_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_252	 = 252.082467_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_253	 = 253.085185_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_254	 = 254.0868542_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_255	 = 255.089962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_256	 = 256.091773_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_257	 = 257.095105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_258	 = 258.09708_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_259	 = 259.10060_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Fm_260	 = 260.10268_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_245	 = 245.08083_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_246	 = 246.08189_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_247	 = 247.08164_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_248	 = 248.08282_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_249	 = 249.08301_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_250	 = 250.08442_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_251	 = 251.08484_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_252	 = 252.08656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_253	 = 253.08728_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_254	 = 254.08966_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_255	 = 255.091083_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_256	 = 256.094060_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_257	 = 257.095541_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_258	 = 258.098431_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_259	 = 259.10051_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_260	 = 260.10365_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_261	 = 261.10572_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Md_262	 = 262.10887_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_248	 = 248.08660_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_249	 = 249.08783_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_250	 = 250.08751_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_251	 = 251.08901_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_252	 = 252.088977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_253	 = 253.09068_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_254	 = 254.090955_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_255	 = 255.093241_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_256	 = 256.094283_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_257	 = 257.096877_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_258	 = 258.09821_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_259	 = 259.10103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_260	 = 260.10264_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_261	 = 261.10575_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_262	 = 262.10730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_263	 = 263.11055_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_No_264	 = 264.11235_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_251	 = 251.09436_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_252	 = 252.09537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_253	 = 253.09521_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_254	 = 254.09645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_255	 = 255.09668_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_256	 = 256.09863_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_257	 = 257.09956_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_258	 = 258.10181_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_259	 = 259.102900_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_260	 = 260.10550_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_261	 = 261.10688_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_262	 = 262.10963_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_263	 = 263.11129_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_264	 = 264.11404_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_265	 = 265.11584_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Lr_266	 = 266.11931_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_253	 = 253.10069_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_254	 = 254.10018_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_255	 = 255.10134_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_256	 = 256.101166_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_257	 = 257.10299_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_258	 = 258.10349_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_259	 = 259.105640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_260	 = 260.10644_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_261	 = 261.108770_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_262	 = 262.10993_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_263	 = 263.11255_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_264	 = 264.11399_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_265	 = 265.11670_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_266	 = 266.11796_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_267	 = 267.12153_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rf_268	 = 268.12364_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_255	 = 255.10740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_256	 = 256.10813_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_257	 = 257.10772_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_258	 = 258.10923_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_259	 = 259.10961_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_260	 = 260.11130_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_261	 = 261.11206_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_262	 = 262.11408_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_263	 = 263.11499_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_264	 = 264.11740_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_265	 = 265.11860_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_266	 = 266.12103_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_267	 = 267.12238_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_268	 = 268.12545_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_269	 = 269.12746_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Db_270	 = 270.13071_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_258	 = 258.11317_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_259	 = 259.11450_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_260	 = 260.114420_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_261	 = 261.11612_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_262	 = 262.11640_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_263	 = 263.11832_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_264	 = 264.11893_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_265	 = 265.121110_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_266	 = 266.12207_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_267	 = 267.12443_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_268	 = 268.12561_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_269	 = 269.12876_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_270	 = 270.13033_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_271	 = 271.13347_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_272	 = 272.13516_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Sg_273	 = 273.13822_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_260	 = 260.12197_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_261	 = 261.12166_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_262	 = 262.12289_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_263	 = 263.12304_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_264	 = 264.12460_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_265	 = 265.12515_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_266	 = 266.12694_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_267	 = 267.12765_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_268	 = 268.12976_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_269	 = 269.13069_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_270	 = 270.13362_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_271	 = 271.13518_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_272	 = 272.13803_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_273	 = 273.13962_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_274	 = 274.14244_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Bh_275	 = 275.14425_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_263	 = 263.12856_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_264	 = 264.128390_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_265	 = 265.13009_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_266	 = 266.13010_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_267	 = 267.13179_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_268	 = 268.13216_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_269	 = 269.13406_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_270	 = 270.13465_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_271	 = 271.13766_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_272	 = 272.13905_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_273	 = 273.14199_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_274	 = 274.14313_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_275	 = 275.14595_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_276	 = 276.14721_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Hs_277	 = 277.14984_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_265	 = 265.13615_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_266	 = 266.13730_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_267	 = 267.13731_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_268	 = 268.13873_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_269	 = 269.13906_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_270	 = 270.14066_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_271	 = 271.14114_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_272	 = 272.14374_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_273	 = 273.14491_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_274	 = 274.14749_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_275	 = 275.14865_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_276	 = 276.15116_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_277	 = 277.15242_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_278	 = 278.15481_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Mt_279	 = 279.15619_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_267	 = 267.14434_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_268	 = 268.14380_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_269	 = 269.14512_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_270	 = 270.14472_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_271	 = 271.14606_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_272	 = 272.14632_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_273	 = 273.14886_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_274	 = 274.14949_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_275	 = 275.15218_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_276	 = 276.15303_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_277	 = 277.15565_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_278	 = 278.15647_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_279	 = 279.15886_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_280	 = 280.15980_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Ds_281	 = 281.16206_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_272	 = 272.15362_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_273	 = 273.15368_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_274	 = 274.15571_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_275	 = 275.15614_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_276	 = 276.15849_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_277	 = 277.15952_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_278	 = 278.16160_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_279	 = 279.16247_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_280	 = 280.16447_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_281	 = 281.16537_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_282	 = 282.16749_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Rg_283	 = 283.16842_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_277	 = 277.16394_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_278	 = 278.16431_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_279	 = 279.16655_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_280	 = 280.16704_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_281	 = 281.16929_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_282	 = 282.16977_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_283	 = 283.17179_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_284	 = 284.17238_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Cn_285	 = 285.17411_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uut_283	 = 283.17645_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uut_284	 = 284.17808_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uut_285	 = 285.17873_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uut_286	 = 286.18048_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uut_287	 = 287.18105_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuq_285	 = 285.18370_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuq_286	 = 286.18386_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuq_287	 = 287.18560_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuq_288	 = 288.18569_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuq_289	 = 289.18728_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uup_287	 = 287.19119_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uup_288	 = 288.19249_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uup_289	 = 289.19272_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uup_290	 = 290.19414_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uup_291	 = 291.19438_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuh_289	 = 289.19886_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuh_290	 = 290.19859_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuh_291	 = 291.20001_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuh_292	 = 292.19979_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uus_291	 = 291.20656_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uus_292	 = 292.20755_R8 * ITM_AMU   !< isotope mass [kg]
	real(kind=R8), parameter :: ITM_MASS_Uuo_293	 = 293.21467_R8 * ITM_AMU   !< isotope mass [kg]

	!> Time interpolation constants required for euITM_routines.euitm_get_slice
	!> C.f. $UAL/include/UALDef.h and the UAL documentation, section 2.3
	integer, parameter :: UAL_CLOSEST_SAMPLE	 = 1  !< Closest time slice
	integer, parameter :: UAL_PREVIOUS_SAMPLE	 = 2  !< Previous time slice
	integer, parameter :: UAL_INTERPOLATION	 = 3  !< Interpolation in time

	!> Constants for initializing unused CPO fields. Fortran users should also see itm_types.f90
	integer, parameter :: ITM_INVALID_INT	 = -999999999  !< Value for invalid/uninitialized integer field
	real(kind=R8), parameter :: ITM_INVALID_FLOAT	 = -9.0e40_R8  !< Value for invalid/uninitialized float field

	!> Version string for itm_constants
	character(len=*), parameter :: ITM_CONSTANTS_VERSION	 = "$Id: itm_constants.xml 553 2012-07-13 11:21:35Z coster $"

  interface get_type_description
     module procedure  get_type_description__name
  end interface get_type_description

contains

  !> Function returning the VALUE of the type with name NAME.
  function get_type_value(NAME)
    integer :: get_type_value
    character*32 :: NAME  !< The name of the type
    get_type_value=-999999999
    select case (NAME)
      case ('UAL_CLOSEST_SAMPLE')
        get_type_id=1
      case ('UAL_PREVIOUS_SAMPLE')
        get_type_id=2
      case ('UAL_INTERPOLATION')
        get_type_id=3
      case ('ITM_INVALID_INT')
        get_type_id=-999999999
    end select
  end function get_type_value

  !> Function returning the DESCRIPTION of the type with name NAME.
  function get_type_description__name(NAME)
    character*124 :: get_type_description__name
    character*32 :: NAME  !< The name of the type
    get_type_description__name=''
    select case (NAME)
      case ('Fortran')
        get_type_description__name=''
      case ('ITM_PI')
        get_type_description__name=''
      case ('ITM_C')
        get_type_description__name='speed of light'
      case ('ITM_ME')
        get_type_description__name='electron mass'
      case ('ITM_MP')
        get_type_description__name='proton mass'
      case ('ITM_MN')
        get_type_description__name='neutron mass'
      case ('ITM_MD')
        get_type_description__name='deuteron mass'
      case ('ITM_MT')
        get_type_description__name='triton mass'
      case ('ITM_MA')
        get_type_description__name='alpha mass'
      case ('ITM_AMU')
        get_type_description__name='atomic mass unit'
      case ('ITM_EV')
        get_type_description__name='electron volt (eV)'
      case ('ITM_QE')
        get_type_description__name='elementary charge'
      case ('ITM_MU0')
        get_type_description__name='vacuum permeability'
      case ('ITM_EPS0')
        get_type_description__name=''
      case ('ITM_AVOGR')
        get_type_description__name=''
      case ('ITM_KBOLT')
        get_type_description__name=''
      case ('ITM_MASS_H_1')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_2')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_3')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_4')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_5')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_6')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_H_7')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_3')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_4')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_5')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_6')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_7')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_8')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_9')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_He_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_3')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_4')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_5')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_6')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_7')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_8')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_9')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_11')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Li_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_5')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_6')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_7')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_8')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_9')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_11')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_13')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Be_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_6')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_7')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_8')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_9')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_11')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_13')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_B_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_8')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_9')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_11')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_13')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_C_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_10')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_11')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_13')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_N_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_12')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_13')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_O_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_14')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_15')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_F_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_16')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_17')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ne_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_18')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Na_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_19')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_20')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mg_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_21')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Al_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_22')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_23')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Si_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_24')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_25')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_P_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_26')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_27')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_S_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_28')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_29')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cl_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_30')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_31')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ar_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_32')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_33')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_K_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_34')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_35')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ca_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_36')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_37')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sc_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_38')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_39')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ti_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_40')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_41')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_V_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_42')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_43')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cr_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_44')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mn_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_45')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_46')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fe_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_47')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Co_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_48')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_49')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_50')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_51')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ni_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_52')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_53')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cu_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_54')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_55')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zn_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_56')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_57')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ga_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_58')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_59')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ge_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_60')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_61')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_62')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_63')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_64')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_As_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_65')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_66')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Se_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_67')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_68')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Br_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_69')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_70')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Kr_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_71')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_72')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rb_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_73')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_74')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_75')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sr_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_76')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_77')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Y_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_78')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_79')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_80')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Zr_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_81')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_82')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nb_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_83')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_84')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mo_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_85')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_86')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tc_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_87')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_88')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ru_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_89')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_90')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rh_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_91')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_92')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pd_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_93')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_94')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ag_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_95')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_96')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cd_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_97')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_98')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_In_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_99')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_100')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_101')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_102')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sn_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_103')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_104')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sb_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_105')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_106')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_107')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Te_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_108')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_109')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_I_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_110')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_111')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Xe_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_112')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_113')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cs_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_114')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_115')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_116')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ba_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_117')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_118')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_La_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_119')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_120')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ce_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_121')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_122')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_123')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pr_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_124')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_125')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Nd_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_126')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_127')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pm_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_128')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_129')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sm_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_130')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_131')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_132')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_133')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Eu_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_134')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_135')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Gd_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_136')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_137')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tb_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_138')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_139')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Dy_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_140')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_141')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_142')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ho_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_143')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_144')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Er_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_145')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_146')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_147')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tm_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_148')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_149')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Yb_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_150')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_151')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_152')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lu_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_153')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_154')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hf_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_155')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_156')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_157')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ta_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_158')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_159')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_W_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_160')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_161')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Re_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_162')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_163')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Os_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_164')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_165')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ir_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_166')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_167')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_168')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pt_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_169')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_170')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Au_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_171')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_172')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_173')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_174')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_175')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hg_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_176')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_177')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Tl_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_178')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_179')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_180')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_181')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_182')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_183')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pb_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_184')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_185')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_186')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_187')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bi_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_188')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_189')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_190')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_191')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_192')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Po_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_193')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_194')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_At_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_195')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_196')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_197')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_198')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rn_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_199')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_200')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_201')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fr_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_202')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_203')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_204')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_205')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ra_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_206')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_207')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_208')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ac_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_209')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_210')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_211')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Th_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_212')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_213')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_214')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_215')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_216')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pa_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_217')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_218')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_219')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_220')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_221')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_222')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_223')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_224')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_U_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_225')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_226')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_227')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Np_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_228')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_229')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_230')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Pu_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_231')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_232')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Am_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_233')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_234')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cm_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_235')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_236')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bk_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_237')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_238')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_239')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cf_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_240')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_241')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Es_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_242')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_243')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_244')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Fm_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_245')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_246')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_247')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Md_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_248')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_249')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_250')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_No_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_251')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_252')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Lr_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_253')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_254')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rf_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_255')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_256')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_257')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Db_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_258')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_259')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_271')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Sg_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_260')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_261')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_262')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_271')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_274')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Bh_275')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_263')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_264')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_271')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_274')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_275')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_276')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Hs_277')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_265')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_266')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_271')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_274')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_275')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_276')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_277')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_278')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Mt_279')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_267')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_268')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_269')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_270')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_271')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_274')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_275')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_276')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_277')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_278')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_279')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_280')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Ds_281')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_272')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_273')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_274')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_275')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_276')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_277')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_278')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_279')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_280')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_281')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_282')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Rg_283')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_277')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_278')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_279')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_280')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_281')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_282')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_283')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_284')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Cn_285')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uut_283')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uut_284')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uut_285')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uut_286')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uut_287')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuq_285')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuq_286')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuq_287')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuq_288')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuq_289')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uup_287')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uup_288')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uup_289')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uup_290')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uup_291')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuh_289')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuh_290')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuh_291')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuh_292')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uus_291')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uus_292')
        get_type_description__name='isotope mass'
      case ('ITM_MASS_Uuo_293')
        get_type_description__name='isotope mass'
      case ('UAL_CLOSEST_SAMPLE')
        get_type_description__name='Closest time slice'
      case ('UAL_PREVIOUS_SAMPLE')
        get_type_description__name='Previous time slice'
      case ('UAL_INTERPOLATION')
        get_type_description__name='Interpolation in time'
      case ('ITM_INVALID_INT')
        get_type_description__name='Value for invalid/uninitialized integer field'
      case ('ITM_INVALID_FLOAT')
        get_type_description__name='Value for invalid/uninitialized float field'
      case ('ITM_CONSTANTS_VERSION')
        get_type_description__name=''
    end select
  end function get_type_description__name


end module itm_constants

#!/usr/bin/perl
# to use:
# o.chease_to_cols o.file > o.cols_file
#
$matlab_out=0;
$table_out=1;
$table_head=1;
@tags=(
    "S-MESH",
    "BETA-POLOIDAL",
    "PSIchease=psi/2pi",
    "T=RBphi",
    "T*DT/DPSI",
    "Pressure",
    "Pprime=dp/dpsi",
    "Q profile",
    "DQ/DPSI profile",
    "SHEAR profile",
    "I-STAR=<jphi/R>/<1/R>",
    "I//=<J . B>/<T/R**2>/RGEOM(a)",
    "j//=<J . B>/B0",
    "FTRAP",
# "C1,before scale",
# "C2,before scale",
# "C0/C2(CSM)",
# "C1/C2(CSM)",
# "C3/C2(CSM)",
    "Int(R dlp/|grad(psi)|)=Int(J dchi)",
    "<1/R**2/|grad(psi)|**2>",
    "<1/|grad(psi)|**2>",
    "<1/Bp**2>",
    "<1/R**2>",
    "<Bp**2>",
    "<Bp>",
    "<|grad(psi)|**2>",
    "<|grad(psi)|>",
    "<1/Bp>",
    "<1/R> profile",
    "<R>",
    "<R**2>",
    "<(|grad(psi)|/B)**2>",
    "<1/B**2>",
    "<B>",
    "<B**2>",
    "Bpol0=|grad psi|/R at Bmin",
    "<j.B>BS-CURRENT (ZERO COLL. and all from from ne, etc)",
    "<j.B>BS-CURRENT, all from ne, Te, Zeff, Ti",
    "<j.B>BS-CURRENT, p from equil, gradients from ne', etc",
    "<j.B>BS-CURRENT, p and p' L31 from equil, rest from Te, Ti",
    "NUESTAR (WITHOUT ZEFF)",
    "<j.B> (Eq.44)",
    "<j.B> - <j.B>-BS(all from ne, Te, ...)",
    "ELLIPTICITY",
    "D(ELL.)/Drhovolnorm",
    "MERCIER BY L.A.TH. WITH E ONLY",
    "MERCIER SHAFRANOV YOURCHENKO",
    "Rgeom profile",
    "a/Rgeom",
    "VOLUME profile",
    "RHO_VOL_NORM",
    "RHO_TOR_NORM",
    "RHO_TOR=sqrt(Phi/pi/B0)",
    "1/Rgeom(a) dpsi/drhotor",
    "Ip profile",
    "li profile",
    "ALPHA1=-2 MU0 Rgeom(edge) Q(rho)**2 / Bphi(edge)**2 * DP/DRHOTOR(rho)",
    "ALPHA2=-2 q**2 <R |grad psi|/B**2> dp/dpsi",
    "ALPHA3=-2 Area/pi <1/Bpol> dp/dpsi",
    "ALPHA4=ALPHA3 with 1/sqrt(<Bpol**2>)",
    "alphaGroebner=-dp/dpsi * dV/dpsi /2/pi**2 * sqrt(V/2pi**2/Rgeom)",
    "dVdpsi",
    "dVdrho_tor",
    "dVdrhotor*qoB0rhotor",
    "shiftprime=dD/dRLFS",
    "Bmin profile",
    "Bmax",
    "R_INBOARD",
    "R_OUTBOARD",
    "elongation",
    "delta_bottom",
    "delta_upper",
    "area profile",
    "B1: RHO_PRETOR=r/a",
    "B2: BPO_PRETOR=1/R0 dpsi/d(r/a) (1/2pi)",
    "B3: DVOL/DRHO_TOR",
    "B4: shear(with rhotor)",
    "D1: CONVF",
    "D2: BETABUS_GA=(int pdV - p(rho) V)/V/(Bpavga**2/2mu0)",
    "D3: BETAPLOCALGA=p/(Bpavga**2/2mu0)",
    "D4: L_I_GA",
    "D5: RBAR=RHO_Vnorm*a*kappa**0.5",
    "D6: rhotor*BT/Rgeom/BPO_hinton*2pi",
    "D7: IBS2 => jBStild=dIBS2/dpsi / dA/dpsi",
    "IDEAL MERCIER",
    "RESISTIVE INTERCHANGE",
    "H OF GLASSER, GREENE & JOHNSON",
    "integral[B**2/R**2/Bp**2 dlp/Bp]",
    "int(Connor)=int(jB/|grad psi|**2 dlp/Bp)",
    "1+1/pi/qprime*int(Connor)",
    "sqrt(1-4 DI)",
    "NCBAL"
    );



$varnum=-1;
while(<>){
    if(/^\s*\n/ && $get==1){
	$get=0;
	$matlab_out &&	print "];\n";
    }
    if($get){
	@d=split;
	foreach $datum(@d){
	    $data[$varnum][$subscript]=$datum;
	    $subscript++;
	    $matlab_out && print "$datum\n";
	}
    }
    foreach $tag(@tags){
	$match=quotemeta($tag);
	if(/$match/x){
	    $varnum++;
	    $subscript=0;
	    $get=1;
	    $matlab_out && print "v_$varnum=[...\n";
	}
    }
}
if($table_out){
    $minwidth=15;
    if($table_head){
	print "%";
	for $j ( 0 .. $#tags ) {
	    $width=max(length($tags[$j])+2,$minwidth);
	    $tmp=$tags[$j];$tmp=~s/\s//g;
	    printf("%".$width."s",$tmp);
	}
	print "\n";
    }
    for $i ( 0 .. $#{$data[0]} ) {
	print " ";
	for $j ( 0 .. $#tags ) {
	    $width=max(length($tags[$j])+2,$minwidth);
	    printf("%".$width.".4e",$data[$j][$i]);
	}
	print "\n";
    }
}

sub max{
    my $a=shift;
    my $b=shift;
    return $a<$b?$b:$a;
}

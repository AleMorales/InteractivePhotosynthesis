# Description of environmental variables

RdescriptionEnvironment = list(
  Ca  =  expression(Ambient~CO[2]~(μmol~mol^{-1})),
  VPD  =  expression(VPD~(kPa)),
  O2  =  expression(O[2]~(mmol~mol^{-1})),
  PAR  =  expression(Photosynthetically~active~radiation~(μmol~m^{-2}~s^{-1})),
  Tleaf  =  expression(T[leaf]~(degree*C))
  )


RdescriptionOutput = list(
 An =  expression(Net~CO[2]~assim.~(μmol~m^{-2}~s^{-1})),
 Ag =  expression(Gross~CO[2]~assim.~(μmol~m^{-2}~s^{-1})),
 Ac =  expression(Rubisco-limited~net~CO[2]~assim.~(μmol~m^{-2}~s^{-1})),
 Aj =  expression(e^{"-"}-transport-limited~net~CO[2]~assim.~(μmol~m^{-2}~s^{-1})),
 Ap =  expression(TPU-limited~net~CO[2]~assim.~(μmol~m^{-2}~s^{-1})), 
 gs =  expression(Stomatal~conductance~(mol~m^{-2}~s^{-1})),
 Ci =  expression(Intercellular~CO[2]~(μmol~mol^{-1})),
 Cc =  expression(Chloroplast~CO[2]~(μmol~mol^{-1})),
 Tr =  expression(Transpiration~(mmol~m^{-2}~s^{-1})),
 WUE = expression(Intrinsic~water~use~efficiency~(mmol~mol^{-1}))
) 